module Network.Socks5.Conf
    ( SocksConf(..)
    , SocksServerAddress(..)
    , socksHost
    , socksPort
    , defaultSocksConf 
    ) where

import Network.Socket
import Network.Socks5.Types (SocksHostAddress(..), SocksVersion(..))
import qualified Data.ByteString.Char8 as BC

-- | Identify a SOCKS server
data SocksServerAddress = SocksServerAddress SocksHostAddress PortNumber
                        deriving (Show,Eq)


-- | SOCKS configuration structure.
-- this structure will be extended in future to support authentification.
-- use defaultSocksConf to create new record.
data SocksConf = SocksConf
    { socksServer  :: SocksServerAddress -- ^ SOCKS Address
    , socksVersion :: SocksVersion       -- ^ SOCKS version to use
    }

-- | SOCKS Host
socksHost :: SocksConf -> SocksHostAddress
socksHost = undefined

-- | SOCKS Port
socksPort :: SocksConf -> PortNumber
socksPort = undefined

-- | defaultSocksConf create a new record, making sure
-- API remains compatible when the record is extended.
defaultSocksConf host port = SocksConf socksServer SocksVer5
    where socksServer = SocksServerAddress haddr port
          haddr       = SocksAddrDomainName $ BC.pack host

