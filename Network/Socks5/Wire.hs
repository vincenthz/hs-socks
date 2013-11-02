{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Network.Socks5.Wire
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
module Network.Socks5.Wire
    ( SocksHello(..)
    , SocksHelloResponse(..)
    , SocksRequest(..)
    , SocksResponse(..)
    , SocksUdpEnvelope(..)
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Serialize
import Data.Word (Word8)

import Network.Socket (PortNumber)

import Network.Socks5.Types

-- | Initial message sent by client with the list of authentification methods supported
data SocksHello = SocksHello { getSocksHelloMethods :: [SocksMethod] }
    deriving (Show,Eq)

-- | Initial message send by server in return from Hello, with the
-- server chosen method of authentication
data SocksHelloResponse = SocksHelloResponse { getSocksHelloResponseMethod :: SocksMethod }
    deriving (Show,Eq)

-- | Define a SOCKS requests
data SocksRequest = SocksRequest
    { requestCommand  :: SocksCommand
    , requestDst      :: SocksAddress
    } deriving (Show,Eq)

-- | Define a SOCKS response
data SocksResponse = SocksResponse
    { responseReply    :: SocksReply
    , responseBind     :: SocksAddress
    } deriving (Show,Eq)

data SocksUdpEnvelope = SocksUdpEnvelope Word8 SocksAddress ByteString
  deriving (Show,Eq)

getAddr :: Word8 -> Get SocksHostAddress
getAddr 1 = SocksAddrIPV4 <$> getWord32host
getAddr 3 = SocksAddrDomainName <$> (getWord8 >>= getByteString . fromIntegral)
getAddr 4 = SocksAddrIPV6 <$> (liftM4 (,,,) getWord32be getWord32be getWord32be getWord32be)
getAddr n = fail ("cannot get unknown socket address type: " ++ show n)

putAddr :: SocksHostAddress -> Put
putAddr (SocksAddrIPV4 h)         = putWord8 1 >> putWord32host h
putAddr (SocksAddrDomainName b)   = putWord8 3 >> putWord8 (fromIntegral $ B.length b) >> putByteString b
putAddr (SocksAddrIPV6 (a,b,c,d)) = putWord8 4 >> mapM_ putWord32be [a,b,c,d]

getPort :: Get PortNumber
getPort = fromIntegral <$> getWord16be

putPort :: PortNumber -> Put
putPort p = putWord16be (fromIntegral p)

putAddress :: SocksAddress -> Put
putAddress (SocksAddress h p) = putAddr h >> putPort p

getAddress :: Get SocksAddress
getAddress = do h <- getAddr =<< getWord8
                p <- getPort
                return (SocksAddress h p)

getSocksRequest 5 = do
    cmd <- toEnum . fromIntegral <$> getWord8
    _   <- getWord8
    addr <- getAddress
    return $ SocksRequest cmd addr
getSocksRequest v =
    fail ("unsupported version of the protocol " ++ show v)

getSocksResponse 5 = do
    reply <- toEnum . fromIntegral <$> getWord8
    _     <- getWord8
    addr <- getAddress
    return $ SocksResponse reply addr
getSocksResponse v =
    fail ("unsupported version of the protocol " ++ show v)

instance Serialize SocksHello where
    put (SocksHello ms) = do
        putWord8 5
        putWord8 $ fromIntegral $ length ms
        mapM_ (putWord8 . fromIntegral . fromEnum) ms
    get = do
        v <- getWord8
        case v of
            5 -> do n <- getWord8
                    let getMethod = toEnum . fromIntegral <$> getWord8
                    methods <- replicateM (fromIntegral n) getMethod
                    return (SocksHello methods)
            _ -> fail "unsupported sock hello version"

instance Serialize SocksHelloResponse where
    put (SocksHelloResponse m) = putWord8 5 >> putWord8 (fromIntegral $ fromEnum $ m)
    get = do
        v <- getWord8
        case v of
            5 -> SocksHelloResponse . toEnum . fromIntegral <$> getWord8
            _ -> fail "unsupported sock hello response version"

instance Serialize SocksRequest where
    put req = do
        putWord8 5
        putWord8 $ fromIntegral $ fromEnum $ requestCommand req
        putWord8 0
        putAddress $ requestDst req
    get = getWord8 >>= getSocksRequest

instance Serialize SocksResponse where
    put req = do
        putWord8 5
        putWord8 $ fromIntegral $ fromEnum $ responseReply req
        putWord8 0
        putAddress $ responseBind req
    get = getWord8 >>= getSocksResponse

instance Serialize SocksUdpEnvelope where
    put (SocksUdpEnvelope fragment addr body) = do
        putWord8 0 -- reserved
        putWord8 0 -- reserved
        putWord8 fragment
        putAddress addr
        putByteString body
    get = do _        <- getWord8 -- reserved
             _        <- getWord8 -- reserved
             fragment <- getWord8
             addr     <- getAddress
             n        <- remaining
             body     <- getByteString n
             return (SocksUdpEnvelope fragment addr body)
