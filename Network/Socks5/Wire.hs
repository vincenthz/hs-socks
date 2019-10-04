{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Network.Socks5.Wire
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
module Network.Socks5.Wire
    ( SocksHello(..)
    , SocksHelloResponse(..)
    , SocksAuthRequest(..)
    , SocksAuthResponse(..)
    , SocksRequest(..)
    , SocksResponse(..)
    ) where

import Basement.Compat.Base
import Control.Monad
import qualified Data.ByteString as B
import Data.Serialize
import qualified Prelude

import Network.Socket (PortNumber)

import Network.Socks5.Types

-- | Initial message sent by client with the list of authentification methods supported
data SocksHello = SocksHello { getSocksHelloMethods :: [SocksMethod] }
    deriving (Show,Eq)

-- | Initial message send by server in return from Hello, with the
-- server chosen method of authentication
data SocksHelloResponse = SocksHelloResponse { getSocksHelloResponseMethod :: SocksMethod }
    deriving (Show,Eq)

-- | Username/Password request used with SocksMethodUsernamePassword authentication method
data SocksAuthRequest = SocksAuthRequest { auth :: SocksAuthUsername }
    deriving (Show, Eq)

-- | Response to an authentication request
data SocksAuthResponse = SocksAuthResponse { authResponseReply :: SocksReply } 
    deriving (Show, Eq)

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
getAddr 3 = SocksAddrDomainName <$> (getLength8 >>= getByteString)
getAddr 4 = SocksAddrIPV6 <$> (liftM4 (,,,) getWord32host getWord32host getWord32host getWord32host)
getAddr n = error ("cannot get unknown socket address type: " <> show n)

putAddr (SocksAddrIPV4 h)         = putWord8 1 >> putWord32host h
putAddr (SocksAddrDomainName b)   = putWord8 3 >> putLength8 (B.length b) >> putByteString b
putAddr (SocksAddrIPV6 (a,b,c,d)) = putWord8 4 >> mapM_ putWord32host [a,b,c,d]

putEnum8 :: Enum e => e -> Put
putEnum8 = putWord8 . Prelude.fromIntegral . fromEnum

getEnum8 :: Enum e => Get e
getEnum8 = toEnum . Prelude.fromIntegral <$> getWord8

putLength8 :: Int -> Put
putLength8 = putWord8 . Prelude.fromIntegral

getLength8 :: Get Int
getLength8 = Prelude.fromIntegral <$> getWord8

getSocksRequest 5 = do
    cmd <- getEnum8
    _   <- getWord8
    addr <- getWord8 >>= getAddr
    port <- Prelude.fromIntegral <$> getWord16be
    return $ SocksRequest cmd addr port
getSocksRequest v =
    error ("unsupported version of the protocol " <> show v)

getSocksResponse 5 = do
    reply <- getEnum8
    _     <- getWord8
    addr <- getWord8 >>= getAddr
    port <- Prelude.fromIntegral <$> getWord16be
    return $ SocksResponse reply addr port
getSocksResponse v =
    error ("unsupported version of the protocol " <> show v)

getAuthResponse 1 = do
    reply <- getEnum8
    return $ SocksAuthResponse reply

getAuthResponse v =
    error ("unsupported version of username/password subnegotiation " <> show v)

instance Serialize SocksHello where
    put (SocksHello ms) = do
        putWord8 5
        putLength8 (Prelude.length ms)
        mapM_ putEnum8 ms
    get = do
        v <- getWord8
        case v of
            5 -> SocksHello <$> (getLength8 >>= flip replicateM getEnum8)
            _ -> error "unsupported sock hello version"

instance Serialize SocksHelloResponse where
    put (SocksHelloResponse m) = putWord8 5 >> putEnum8 m
    get = do
        v <- getWord8
        case v of
            5 -> SocksHelloResponse <$> getEnum8
            _ -> error ("unsupported sock hello response version " <> show v)

instance Serialize SocksAuthRequest where
    put (SocksAuthRequest (SocksAuthUsername username password)) = do
        putWord8 1 -- this is a version of subnegotiation, not Socks protocol
        putLength8 (B.length username)
        putByteString username
        putLength8 (B.length password)
        putByteString password
    get = do
        username <- (getLength8 >>= getByteString)
        password <- (getLength8 >>= getByteString)
        return (SocksAuthRequest (SocksAuthUsername username password))

instance Serialize SocksAuthResponse where
    put (SocksAuthResponse reply) = do
        putWord8 1
        putEnum8 $ reply
    get = getWord8 >>= getAuthResponse

instance Serialize SocksRequest where
    put req = do
        putWord8 5
        putEnum8 $ requestCommand req
        putWord8 0
        putAddr $ requestDstAddr req
        putWord16be $ Prelude.fromIntegral $ requestDstPort req
        
    get = getWord8 >>= getSocksRequest

instance Serialize SocksResponse where
    put req = do
        putWord8 5
        putEnum8 $ responseReply req
        putWord8 0
        putAddr $ responseBindAddr req
        putWord16be $ Prelude.fromIntegral $ responseBindPort req
    get = getWord8 >>= getSocksResponse
