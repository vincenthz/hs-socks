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
    ( establish
    , connectIPV4
    , connectIPV6
    , connectDomainName
    -- * lowlevel interface
    , rpc
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

establish :: Socket -> [SocksMethod] -> IO SocksMethod
establish socket methods = do
    sendAll socket (encode $ SocksHello methods)
    getSocksHelloResponseMethod <$> runGetDone get (recv socket 4096)

connectIPV4 :: Socket -> HostAddress -> PortNumber -> IO (HostAddress, PortNumber)
connectIPV4 socket hostaddr port = onReply <$> rpc socket request
    where
        request = SocksRequest
            { requestCommand  = SocksCommandConnect
            , requestDstAddr  = SocksAddrIPV4 hostaddr
            , requestDstPort  = fromIntegral port
            }

        onReply (SocksAddrIPV4 h, p) = (h, p)
        onReply _                    = error "ipv4 requested, got something different"

connectIPV6 :: Socket -> HostAddress6 -> PortNumber -> IO (HostAddress6, PortNumber)
connectIPV6 socket hostaddr6 port = onReply <$> rpc socket request
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
connectDomainName :: Socket -> String -> PortNumber -> IO (SocksAddr, PortNumber)
connectDomainName socket fqdn port = rpc socket $ SocksRequest
    { requestCommand  = SocksCommandConnect
    , requestDstAddr  = SocksAddrDomainName $ BC.pack fqdn
    , requestDstPort  = fromIntegral port
    }

rpc :: Socket -> SocksRequest -> IO (SocksAddr, PortNumber)
rpc socket req = do
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
