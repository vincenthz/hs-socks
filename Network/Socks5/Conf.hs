-- |
-- Module      : Network.Socks5.Conf
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- typical SOCKS configuration
module Network.Socks5.Conf
    ( SocksConf(..)
    , socksHost
    , defaultSocksConf 
    , defaultSocksConfFromSockAddr
    ) where

import Network.Socket
import Network.Socks5.Types (SocksVersion(..))

-- | SOCKS identification and configuration structure.
--
-- this structure will be extended in future to support authentification.
-- use defaultSocksConf to create new record.
data SocksConf = SocksConf
    { socksServer  :: SockAddr     -- ^ Address of server
    , socksVersion :: SocksVersion -- ^ SOCKS version to use
    }

-- | SOCKS Host
socksHost :: SocksConf -> SockAddr
socksHost conf = socksServer conf

-- | defaultSocksConf create a new record, making sure
-- API remains compatible when the record is extended.
defaultSocksConf :: SockAddr -> SocksConf
defaultSocksConf host = SocksConf host SocksVer5

-- | same as defaultSocksConf.
--
-- soft deprecation: use 'defaultSocksConf"
defaultSocksConfFromSockAddr = defaultSocksConf
