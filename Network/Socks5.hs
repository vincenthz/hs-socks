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
	( socksConnectAddr
	, socksConnectName
	) where

import Control.Monad
import Network.Socket (Socket, SockAddr(..), PortNumber, connect)
import Network.Socks5.Command
import Network.Socks5.Types

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
		SockAddrUnix _        -> error "unsupported unix sockaddr type"

-- | connect a new socket to the socks server, and connect the stream to a FQDN
-- resolved on the server side.
socksConnectName :: Socket -> SockAddr -> String -> PortNumber -> IO ()
socksConnectName sock sockserver destination port = withSocks sock sockserver $ do
	_ <- socks5ConnectDomainName sock destination port
	return ()
