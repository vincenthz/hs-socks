module Network.Socks5.Lowlevel
    ( resolveToSockAddr
    -- * lowlevel types
    , module Network.Socks5.Wire
    , module Network.Socks5.Command
    ) where

import Network.Socket
import Network.BSD
import Network.Socks5.Command
import Network.Socks5.Wire
import Network.Socks5.Types
import qualified Data.ByteString.Char8 as BC

resolveToSockAddr :: SocksAddress -> IO SockAddr
resolveToSockAddr (SocksAddress sockHostAddr port) =
    case sockHostAddr of
        SocksAddrIPV4 ha       -> return $ SockAddrInet port ha
        SocksAddrIPV6 ha6      -> return $ SockAddrInet6 port 0 ha6 0
        SocksAddrDomainName bs -> do he <- getHostByName (BC.unpack bs)
                                     return $ SockAddrInet port (hostAddress he)

