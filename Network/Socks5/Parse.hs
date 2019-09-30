{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.Socks5.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A very simple bytestring parser related to Parsec and Attoparsec
--
-- Simple example:
--
-- > > parse ((,) <$> take 2 <*> byte 0x20 <*> (bytes "abc" *> anyByte)) "xx abctest"
-- > ParseOK "est" ("xx", 116)
--
module Network.Socks5.Parse
    ( Parser
    , Result(..)
    -- * run the Parser
    , parse
    , parseFeed
    -- * Parser methods
    , byte
    , anyByte
    , bytes
    , take
    , takeWhile
    , takeAll
    , skip
    , skipWhile
    , skipAll
    , takeStorable
    ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (toForeignPtr)
import Data.Word
import Foreign.Storable (Storable, peekByteOff, sizeOf)
import Foreign.ForeignPtr (withForeignPtr)
import Prelude hiding (take, takeWhile)

import System.IO.Unsafe (unsafePerformIO)

-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more input data
--
-- * success: the remaining unparsed data and the parser value
data Result a =
      ParseFail String
    | ParseMore (ByteString -> Result a)
    | ParseOK   ByteString a

instance Show a => Show (Result a) where
    show (ParseFail err) = "ParseFailure: " ++ err
    show (ParseMore _)   = "ParseMore _"
    show (ParseOK b a)   = "ParseOK " ++ show a ++ " " ++ show b

type Failure r = ByteString -> String -> Result r
type Success a r = ByteString -> a -> Result r

-- | Simple ByteString parser structure
newtype Parser a = Parser
    { runParser :: forall r . ByteString -> Failure r -> Success a r -> Result r }

instance Monad Parser where
    return v = Parser $ \buf _ ok -> ok buf v
    m >>= k = Parser $ \buf err ok ->
         runParser m buf err (\buf' a -> runParser (k a) buf' err ok)
#if MIN_VERSION_base(4,13,0)
instance MonadFail Parser where
#endif
    fail errorMsg = Parser $ \buf err _ -> err buf ("failed: " ++ errorMsg)
instance MonadPlus Parser where
    mzero = fail "Parser.MonadPlus.mzero"
    mplus f g = Parser $ \buf err ok ->
        -- rewrite the err callback of @f to call @g
        runParser f buf (\_ _ -> runParser g buf err ok) ok
instance Functor Parser where
    fmap f p = Parser $ \buf err ok ->
        runParser p buf err (\b a -> ok b (f a))
instance Applicative Parser where
    pure      = return
    (<*>) d e = d >>= \b -> e >>= \a -> return (b a)
instance Alternative Parser where
    empty = fail "Parser.Alternative.empty"
    (<|>) = mplus

-- | Run a parser on an @initial ByteString.
--
-- If the Parser need more data than available, the @feeder function
-- is automatically called and fed to the More continuation.
parseFeed :: Monad m => m B.ByteString -> Parser a -> B.ByteString -> m (Result a)
parseFeed feeder p initial = loop $ parse p initial
  where loop (ParseMore k) = feeder >>= (loop . k)
        loop r             = return r

-- | Run a Parser on a ByteString and return a 'Result'
parse :: Parser a -> ByteString -> Result a
parse p s = runParser p s (\_ msg -> ParseFail msg) (\b a -> ParseOK b a)

------------------------------------------------------------
getMore :: Parser ()
getMore = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    if B.null nextChunk
        then err buf "EOL: need more data"
        else ok (B.append buf nextChunk) ()

getAll :: Parser ()
getAll = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    if B.null nextChunk
        then ok buf ()
        else runParser getAll (B.append buf nextChunk) err ok

flushAll :: Parser ()
flushAll = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    if B.null nextChunk
        then ok buf ()
        else runParser getAll B.empty err ok

------------------------------------------------------------

-- | Get the next byte from the parser
anyByte :: Parser Word8
anyByte = Parser $ \buf err ok ->
    case B.uncons buf of
        Nothing      -> runParser (getMore >> anyByte) buf err ok
        Just (c1,b2) -> ok b2 c1

-- | Parse a specific byte at current position
--
-- if the byte is different than the expected on,
-- this parser will raise a failure.
byte :: Word8 -> Parser ()
byte w = Parser $ \buf err ok ->
    case B.uncons buf of
        Nothing      -> runParser (getMore >> byte w) buf err ok
        Just (c1,b2) | c1 == w   -> ok b2 ()
                     | otherwise -> err buf ("byte " ++ show w ++ " : failed")

-- | Parse a sequence of bytes from current position
--
-- if the following bytes don't match the expected
-- bytestring completely, the parser will raise a failure
bytes :: ByteString -> Parser ()
bytes allExpected = consumeEq allExpected
  where errMsg = "bytes " ++ show allExpected ++ " : failed"

        -- partially consume as much as possible or raise an error.
        consumeEq expected = Parser $ \actual err ok ->
            let eLen = B.length expected in
            if B.length actual >= eLen
                then    -- enough data for doing a full match
                        let (aMatch,aRem) = B.splitAt eLen actual
                         in if aMatch == expected
                                then ok aRem ()
                                else err actual errMsg
                else    -- not enough data, match as much as we have, and then recurse.
                        let (eMatch, eRem) = B.splitAt (B.length actual) expected
                         in if actual == eMatch
                                then runParser (getMore >> consumeEq eRem) B.empty err ok
                                else err actual errMsg

------------------------------------------------------------

-- | Take a storable from the current position in the stream
takeStorable :: Storable d
             => Parser d
takeStorable = anyStorable undefined
  where
    anyStorable :: Storable d => d -> Parser d
    anyStorable a = do
        (fptr, off, _) <- B.toForeignPtr <$> take (sizeOf a)
        return $ unsafePerformIO $ withForeignPtr fptr $ \ptr -> peekByteOff ptr off

-- | Take @n bytes from the current position in the stream
take :: Int -> Parser ByteString
take n = Parser $ \buf err ok ->
    if B.length buf >= n
        then let (b1,b2) = B.splitAt n buf in ok b2 b1
        else runParser (getMore >> take n) buf err ok

-- | Take bytes while the @predicate hold from the current position in the stream
takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile predicate = Parser $ \buf err ok ->
    case B.span predicate buf of
        (_, b2) | B.null b2 -> runParser (getMore >> takeWhile predicate) buf err ok
        (b1, b2) -> ok b2 b1

-- | Take the remaining bytes from the current position in the stream
takeAll :: Parser ByteString
takeAll = Parser $ \buf err ok ->
    runParser (getAll >> returnBuffer) buf err ok
  where
    returnBuffer = Parser $ \buf _ ok -> ok B.empty buf

-- | Skip @n bytes from the current position in the stream
skip :: Int -> Parser ()
skip n = Parser $ \buf err ok ->
    if B.length buf >= n
        then ok (B.drop n buf) ()
        else runParser (getMore >> skip (n - B.length buf)) B.empty err ok

-- | Skip bytes while the @predicate hold from the current position in the stream
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = Parser $ \buf err ok ->
    case B.span p buf of
        (_, b2) | B.null b2 -> runParser (getMore >> skipWhile p) B.empty err ok
        (_, b2) -> ok b2 ()

-- | Skip all the remaining bytes from the current position in the stream
skipAll :: Parser ()
skipAll = Parser $ \buf err ok -> runParser flushAll buf err ok
