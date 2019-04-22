{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
-- |
-- Module      : Network.Socks5.Command
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Network.Socks5.Command
    ( establish
    , Connect(..)
    , Command(..)
    , connectIPV4
    , connectIPV6
    , connectDomainName
    -- * lowlevel interface
    , rpc
    , rpc_
    , sendSerialized
    , waitSerialized
    ) where

import Basement.Compat.Base
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Prelude
import Data.Serialize

import Network.Socket (Socket, PortNumber, HostAddress, HostAddress6)
import Network.Socket.ByteString

import Network.Socks5.Types
import Network.Socks5.Wire

establish :: SocksVersion -> Socket -> [SocksMethod] -> IO SocksMethod
establish SocksVer5 socket methods = do
    sendAll socket (encode $ SocksHello methods)
    getSocksHelloResponseMethod <$> runGetDone get (recv socket 4096)

newtype Connect = Connect SocksAddress deriving (Show,Eq,Ord)

class Command a where
    toRequest   :: a -> SocksRequest
    fromRequest :: SocksRequest -> Maybe a

instance Command SocksRequest where
    toRequest   = id
    fromRequest = Just

instance Command Connect where
    toRequest (Connect (SocksAddress ha port)) = SocksRequest
            { requestCommand  = SocksCommandConnect
            , requestDstAddr  = ha
            , requestDstPort  = Prelude.fromIntegral port
            }
    fromRequest req
        | requestCommand req /= SocksCommandConnect = Nothing
        | otherwise = Just $ Connect $ SocksAddress (requestDstAddr req) (requestDstPort req)

connectIPV4 :: Socket -> HostAddress -> PortNumber -> IO (HostAddress, PortNumber)
connectIPV4 socket hostaddr port = onReply <$> rpc_ socket (Connect $ SocksAddress (SocksAddrIPV4 hostaddr) port)
    where onReply (SocksAddrIPV4 h, p) = (h, p)
          onReply _                    = error "ipv4 requested, got something different"

connectIPV6 :: Socket -> HostAddress6 -> PortNumber -> IO (HostAddress6, PortNumber)
connectIPV6 socket hostaddr6 port = onReply <$> rpc_ socket (Connect $ SocksAddress (SocksAddrIPV6 hostaddr6) port)
    where onReply (SocksAddrIPV6 h, p) = (h, p)
          onReply _                    = error "ipv6 requested, got something different"

-- TODO: FQDN should only be ascii, maybe putting a "fqdn" data type
-- in front to make sure and make the BC.pack safe.
connectDomainName :: Socket -> [Char] -> PortNumber -> IO (SocksHostAddress, PortNumber)
connectDomainName socket fqdn port = rpc_ socket $ Connect $ SocksAddress (SocksAddrDomainName $ BC.pack fqdn) port

sendSerialized :: Serialize a => Socket -> a -> IO ()
sendSerialized sock a = sendAll sock $ encode a

waitSerialized :: Serialize a => Socket -> IO a
waitSerialized sock = runGetDone get (getMore sock)

rpc :: Command a => Socket -> a -> IO (Either SocksError (SocksHostAddress, PortNumber))
rpc socket req = do
    sendSerialized socket (toRequest req)
    onReply <$> runGetDone get (getMore socket)
    where onReply res@(responseReply -> reply) =
                case reply of
                    SocksReplySuccess -> Right (responseBindAddr res, Prelude.fromIntegral $ responseBindPort res)
                    SocksReplyError e -> Left e

rpc_ :: Command a => Socket -> a -> IO (SocksHostAddress, PortNumber)
rpc_ socket req = rpc socket req >>= either throwIO return

-- this function expect all the data to be consumed. this is fine for intertwined message,
-- but might not be a good idea for multi messages from one party.
runGetDone :: Serialize a => Get a -> IO ByteString -> IO a
runGetDone getter ioget = ioget >>= return . runGetPartial getter >>= r where
#if MIN_VERSION_cereal(0,4,0)
    r (Fail s _)     = error s
#else
    r (Fail s)       = error s
#endif
    r (Partial cont) = ioget >>= r . cont
    r (Done a b)
        | not $ B.null b = error "got too many bytes while receiving data"
        | otherwise      = return a

getMore :: Socket -> IO ByteString
getMore socket = recv socket 4096
