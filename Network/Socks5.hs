{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Network.Socks5
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.Socks5
    ( module Network.Socks5.Conf
    , socksConnectAddr
    , socksConnectName
    , socksConnectTo
    , socksConnectWith
    ) where

import Control.Monad
import Control.Exception
import Network.Socket
import Network.BSD
import Network.Socks5.Command
import Network.Socks5.Conf
import Network.Socks5.Types
import Network
import System.IO

withSocks sock sockaddr f = do
    connect sock sockaddr
    r <- socks5Establish sock [SocksMethodNone]
    when (r == SocksMethodNotAcceptable) $ error "cannot connect with no socks method of authentication"
    f

-- | connect a new socket to the socks server, and connect the stream on the server side
-- to the sockaddr specified. the sockaddr need to be SockAddrInet or SockAddrInet6.
--
-- a unix sockaddr will raises an exception.
--
-- |socket|-----sockServer----->|server|----destAddr----->|destination|
socksConnectAddr :: Socket -> SockAddr -> SockAddr -> IO ()
socksConnectAddr sock sockserver destaddr = withSocks sock sockserver $ do
    case destaddr of
        SockAddrInet p h      -> socks5ConnectIPV4 sock h p >> return ()
        SockAddrInet6 p _ h _ -> socks5ConnectIPV6 sock h p >> return ()
        _                     -> error "unsupported unix sockaddr type"

-- | connect a new socket to the socks server, and connect the stream to a FQDN
-- resolved on the server side.
socksConnectName :: Socket -> SockAddr -> String -> PortNumber -> IO ()
socksConnectName sock sockserver destination port = withSocks sock sockserver $ do
    _ <- socks5ConnectDomainName sock destination port
    return ()

-- | create a new socket and connect in to a destination through the specified
-- SOCKS configuration.
socksConnectWith :: SocksConf -- ^ SOCKS configuration
                 -> String    -- ^ destination hostname
                 -> PortID    -- ^ destination port
                 -> IO Socket
socksConnectWith sockConf desthost destport = do
    dport <- resolvePortID destport
    proto <- getProtocolNumber "tcp"
    bracketOnError (socket AF_INET Stream proto) sClose $ \sock -> do
        he <- getHostByName $ socksHost sockConf
        let sockaddr = SockAddrInet (socksPort sockConf) (hostAddress he)
        socksConnectName sock sockaddr desthost dport
        return sock

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
