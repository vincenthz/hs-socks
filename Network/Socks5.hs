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
    , socksConnectAddr
    , socksConnectName
    , socksConnectTo'
    , socksConnectTo
    , socksConnectWith
    ) where

import Control.Monad
import Control.Exception
import qualified Data.ByteString.Char8 as BC
import Network.Socket ( sClose, Socket, SocketType(..), SockAddr(..), Family(..)
                      , socket, socketToHandle, connect)
import Network.BSD
import Network (PortID(..))

import qualified Network.Socks5.Command as Cmd
import Network.Socks5.Conf
import Network.Socks5.Types
import Network.Socks5.Lowlevel

import System.IO

-- | connect a user specified new socket to the socks server,
-- and connect the stream on the server side to the 'SockAddress' specified.
--
-- |socket|-----sockServer----->|server|----destAddr----->|destination|
--
socksConnectWithSocket :: Socket       -- ^ Socket to use.
                       -> SocksConf    -- ^ SOCKS configuration for the server.
                       -> SocksAddress -- ^ SOCKS Address to connect to.
                       -> IO (SocksHostAddress, PortNumber)
socksConnectWithSocket sock serverConf destAddr = do
    serverAddr <- resolveToSockAddr (socksServer serverConf)
    connect sock serverAddr
    r <- Cmd.establish sock [SocksMethodNone]
    when (r == SocksMethodNotAcceptable) $ error "cannot connect with no socks method of authentication"
    Cmd.rpc_ sock (Connect destAddr)

-- | connect a new socket to a socks server and connect the stream on the
-- server side to the 'SocksAddress' specified.
socksConnect :: SocksConf    -- ^ SOCKS configuration for the server.
             -> SocksAddress -- ^ SOCKS Address to connect to.
             -> IO (Socket, (SocksHostAddress, PortNumber))
socksConnect serverConf destAddr =
    getProtocolNumber "tcp" >>= \proto ->
    bracketOnError (socket AF_INET Stream proto) sClose $ \sock -> do
        ret <- socksConnectWithSocket sock serverConf destAddr
        return (sock, ret)

-- | connect a new socket to the socks server, and connect the stream on the server side
-- to the sockaddr specified. the sockaddr need to be SockAddrInet or SockAddrInet6.
--
-- a unix sockaddr will raises an exception.
--
-- |socket|-----sockServer----->|server|----destAddr----->|destination|
{-# DEPRECATED socksConnectAddr "use socksConnectWithSocket" #-}
socksConnectAddr :: Socket -> SockAddr -> SockAddr -> IO ()
socksConnectAddr sock sockserver destaddr =
    socksConnectWithSocket sock
                           (defaultSocksConfFromSockAddr sockserver)
                           (socksServer $ defaultSocksConfFromSockAddr destaddr) >>
    return ()

-- | connect a new socket to the socks server, and connect the stream to a FQDN
-- resolved on the server side.
socksConnectName :: Socket -> SockAddr -> String -> PortNumber -> IO ()
socksConnectName sock sockserver destination port = do
    socksConnectWithSocket sock
                           (defaultSocksConfFromSockAddr sockserver)
                           (SocksAddress (SocksAddrDomainName $ BC.pack destination) port)
    >> return ()

-- | create a new socket and connect in to a destination through the specified
-- SOCKS configuration.
socksConnectWith :: SocksConf -- ^ SOCKS configuration
                 -> String    -- ^ destination hostname
                 -> PortID    -- ^ destination port
                 -> IO Socket
socksConnectWith socksConf desthost destport = do
    dport <- resolvePortID destport
    proto <- getProtocolNumber "tcp"
    bracketOnError (socket AF_INET Stream proto) sClose $ \sock -> do
        sockaddr <- resolveToSockAddr (socksServer socksConf)
        socksConnectName sock sockaddr desthost dport
        return sock

-- | similar to Network connectTo but use a socks proxy with default socks configuration.
socksConnectTo' :: String -> PortID -> String -> PortID -> IO Socket
socksConnectTo' sockshost socksport desthost destport = do
    sport <- resolvePortID socksport
    let socksConf = defaultSocksConf sockshost sport
    socksConnectWith socksConf desthost destport

-- | similar to Network connectTo but use a socks proxy with default socks configuration.
socksConnectTo :: String -> PortID -> String -> PortID -> IO Handle
socksConnectTo sockshost socksport desthost destport = do
    sport <- resolvePortID socksport
    let socksConf = defaultSocksConf sockshost sport
    sock <- socksConnectWith socksConf desthost destport
    socketToHandle sock ReadWriteMode

resolvePortID (Service serv) = getServicePortNumber serv
resolvePortID (PortNumber n) = return n
resolvePortID _              = error "unsupported unix PortID"
