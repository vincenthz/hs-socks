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
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.Serialize

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
    , requestDstAddr  :: SocksHostAddress
    , requestDstPort  :: PortNumber
    } deriving (Show,Eq)

-- | Define a SOCKS response
data SocksResponse = SocksResponse
    { responseReply    :: SocksReply
    , responseBindAddr :: SocksHostAddress
    , responseBindPort :: PortNumber
    } deriving (Show,Eq)

getAddr 1 = SocksAddrIPV4 <$> getWord32host
getAddr 3 = SocksAddrDomainName <$> (getWord8 >>= getByteString . fromIntegral)
getAddr 4 = SocksAddrIPV6 <$> (liftM4 (,,,) getWord32be getWord32be getWord32be getWord32be)
getAddr n = error ("cannot get unknown socket address type: " ++ show n)

putAddr (SocksAddrIPV4 h)         = putWord8 1 >> putWord32host h
putAddr (SocksAddrDomainName b)   = putWord8 3 >> putWord8 (fromIntegral $ B.length b) >> putByteString b
putAddr (SocksAddrIPV6 (a,b,c,d)) = putWord8 4 >> mapM_ putWord32be [a,b,c,d]

getSocksRequest 5 = do
    cmd <- toEnum . fromIntegral <$> getWord8
    _   <- getWord8
    addr <- getWord8 >>= getAddr
    port <- fromIntegral <$> getWord16be
    return $ SocksRequest cmd addr port
getSocksRequest v =
    error ("unsupported version of the protocol " ++ show v)

getSocksResponse 5 = do
    reply <- toEnum . fromIntegral <$> getWord8
    _     <- getWord8
    addr <- getWord8 >>= getAddr
    port <- fromIntegral <$> getWord16be
    return $ SocksResponse reply addr port
getSocksResponse v =
    error ("unsupported version of the protocol " ++ show v)

instance Serialize SocksHello where
    put (SocksHello ms) = do
        putWord8 5
        putWord8 $ fromIntegral $ length ms
        mapM_ (putWord8 . fromIntegral . fromEnum) ms
    get = do
        v <- getWord8
        case v of
            5 -> getWord8 >>= flip replicateM (toEnum . fromIntegral <$> getWord8) . fromIntegral >>= return . SocksHello
            _ -> error "unsupported sock hello version"

instance Serialize SocksHelloResponse where
    put (SocksHelloResponse m) = putWord8 5 >> putWord8 (fromIntegral $ fromEnum $ m)
    get = do
        v <- getWord8
        case v of
            5 -> SocksHelloResponse . toEnum . fromIntegral <$> getWord8
            _ -> error "unsupported sock hello response version"

instance Serialize SocksRequest where
    put req = do
        putWord8 5
        putWord8 $ fromIntegral $ fromEnum $ requestCommand req
        putWord8 0
        putAddr $ requestDstAddr req
        putWord16be $ fromIntegral $ requestDstPort req
        
    get = getWord8 >>= getSocksRequest

instance Serialize SocksResponse where
    put req = do
        putWord8 5
        putWord8 $ fromIntegral $ fromEnum $ responseReply req
        putWord8 0
        putAddr $ responseBindAddr req
        putWord16be $ fromIntegral $ responseBindPort req
    get = getWord8 >>= getSocksResponse
