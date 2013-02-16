module Network.Socks5.Conf
    ( SocksConf(..)
    , defaultSocksConf 
    ) where

import Network.Socket

-- | SOCKS configuration structure.
-- this structure will be extended in future to support authentification.
-- use defaultSocksConf to create new record.
data SocksConf = SocksConf
    { socksHost    :: String     -- ^ SOCKS host.
    , socksPort    :: PortNumber -- ^ SOCKS port.
    , socksVersion :: Int        -- ^ SOCKS version to use, only 5 supported for now.
    }

-- | defaultSocksConf create a new record, making sure
-- API remains compatible when the record is extended.
defaultSocksConf host port = SocksConf host port 5

