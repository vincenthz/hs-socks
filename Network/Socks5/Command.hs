{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Network.Socks5.Command
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.Socks5.Command
    ( socks5Establish
    , socks5ConnectIPV4
    , socks5ConnectIPV6
    , socks5ConnectDomainName
    -- * lowlevel interface
    , socks5Rpc
    ) where

import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Serialize

import Network.Socket (Socket, PortNumber, HostAddress, HostAddress6)
import Network.Socket.ByteString

import Network.Socks5.Types
import Network.Socks5.Wire

socks5Establish :: Socket -> [SocksMethod] -> IO SocksMethod
socks5Establish socket methods = do
    sendAll socket (encode $ SocksHello methods)
    getSocksHelloResponseMethod <$> runGetDone get (recv socket 4096)

socks5ConnectIPV4 :: Socket -> HostAddress -> PortNumber -> IO (HostAddress, PortNumber)
socks5ConnectIPV4 socket hostaddr port = onReply <$> socks5Rpc socket request
    where
        request = SocksRequest
            { requestCommand  = SocksCommandConnect
            , requestDstAddr  = SocksAddrIPV4 hostaddr
            , requestDstPort  = fromIntegral port
            }

        onReply (SocksAddrIPV4 h, p) = (h, p)
        onReply _                    = error "ipv4 requested, got something different"

socks5ConnectIPV6 :: Socket -> HostAddress6 -> PortNumber -> IO (HostAddress6, PortNumber)
socks5ConnectIPV6 socket hostaddr6 port = onReply <$> socks5Rpc socket request
    where
        request = SocksRequest
            { requestCommand  = SocksCommandConnect
            , requestDstAddr  = SocksAddrIPV6 hostaddr6
            , requestDstPort  = fromIntegral port
            }
        onReply (SocksAddrIPV6 h, p) = (h, p)
        onReply _                    = error "ipv6 requested, got something different"

-- TODO: FQDN should only be ascii, maybe putting a "fqdn" data type
-- in front to make sure and make the BC.pack safe.
socks5ConnectDomainName :: Socket -> String -> PortNumber -> IO (SocksAddr, PortNumber)
socks5ConnectDomainName socket fqdn port = socks5Rpc socket $ SocksRequest
    { requestCommand  = SocksCommandConnect
    , requestDstAddr  = SocksAddrDomainName $ BC.pack fqdn
    , requestDstPort  = fromIntegral port
    }

socks5Rpc :: Socket -> SocksRequest -> IO (SocksAddr, PortNumber)
socks5Rpc socket req = do
    sendAll socket (encode req)
    onReply <$> runGetDone get (recv socket 4096)
    where onReply res@(responseReply -> reply)
                | reply /= SocksReplySuccess = throw $ SocksError reply
                | otherwise                 =
                    (responseBindAddr res, fromIntegral $ responseBindPort res)

runGetDone :: Show a => Get a -> IO ByteString -> IO a
runGetDone getter ioget = ioget >>= return . runGetPartial getter >>= r where
    r (Fail s)       = error s
    r (Partial cont) = ioget >>= r . cont
    r (Done a b)
        | not $ B.null b = error "got too many bytes while receiving data"
        | otherwise      = return a
