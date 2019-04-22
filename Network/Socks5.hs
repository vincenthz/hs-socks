{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Network.Socks5
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- This is an implementation of SOCKS5 as defined in RFC 1928
--
-- In Wikipedia's words:
--
--   SOCKet Secure (SOCKS) is an Internet protocol that routes network packets
--   between a client and server through a proxy server. SOCKS5 additionally
--   provides authentication so only authorized users may access a server.
--   Practically, a SOCKS server will proxy TCP connections to an arbitrary IP
--   address as well as providing a means for UDP packets to be forwarded.
--
-- BIND and UDP ASSOCIATE messages are not implemented.
-- However main usage of SOCKS is covered in this implementation.
--
module Network.Socks5
    (
    -- * Types
      SocksAddress(..)
    , SocksHostAddress(..)
    , SocksReply(..)
    , SocksError(..)
    -- * Configuration
    , module Network.Socks5.Conf
    -- * Methods
    , socksConnectWithSocket
    , socksConnect
    -- * Variants
    , socksConnectName
    ) where

import Control.Monad
import Control.Exception
import qualified Data.ByteString.Char8 as BC
import Network.Socket ( close, Socket, SocketType(..), Family(..)
                      , socket, connect, PortNumber, defaultProtocol)

import qualified Network.Socks5.Command as Cmd
import Network.Socks5.Conf
import Network.Socks5.Types
import Network.Socks5.Lowlevel

-- | connect a user specified new socket on the socks server to a destination
--
-- The socket in parameter needs to be already connected to the socks server
--
-- |socket|-----sockServer----->|server|----destAddr----->|destination|
--
socksConnectWithSocket :: Socket       -- ^ Socket to use.
                       -> SocksConf    -- ^ SOCKS configuration for the server.
                       -> SocksAddress -- ^ SOCKS Address to connect to.
                       -> IO (SocksHostAddress, PortNumber)
socksConnectWithSocket sock serverConf destAddr = do
    r <- Cmd.establish (socksVersion serverConf) sock [SocksMethodNone]
    when (r == SocksMethodNotAcceptable) $ error "cannot connect with no socks method of authentication"
    Cmd.rpc_ sock (Connect destAddr)

-- | connect a new socket to a socks server and connect the stream on the
-- server side to the 'SocksAddress' specified.
socksConnect :: SocksConf    -- ^ SOCKS configuration for the server.
             -> SocksAddress -- ^ SOCKS Address to connect to.
             -> IO (Socket, (SocksHostAddress, PortNumber))
socksConnect serverConf destAddr =
    bracketOnError (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        connect sock (socksServer serverConf)
        ret <- socksConnectWithSocket sock serverConf destAddr
        return (sock, ret)

-- | connect a new socket to the socks server, and connect the stream to a FQDN
-- resolved on the server side.
--
-- The socket needs to *not* be already connected.
--
-- The destination need to be an ASCII string, otherwise unexpected behavior will ensue.
-- For unicode destination, punycode encoding should be used.
socksConnectName :: Socket -> SocksConf -> String -> PortNumber -> IO ()
socksConnectName sock sockConf destination port = do
    connect sock (socksServer sockConf)
    (_,_) <- socksConnectWithSocket sock sockConf addr
    return ()
  where
    addr = SocksAddress (SocksAddrDomainName $ BC.pack destination) port
