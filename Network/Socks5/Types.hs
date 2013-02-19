{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Network.Socks5.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
module Network.Socks5.Types
    ( SocksVersion(..)
    , SocksCommand(..)
    , SocksMethod(..)
    , SocksHostAddress(..)
    , SocksAddress(..)
    , SocksReply(..)
    , SocksVersionNotSupported(..)
    , SocksError(..)
    ) where

import Data.ByteString (ByteString)
import Data.Word
import Data.Data
import Network.Socket (HostAddress, HostAddress6, PortNumber)
import Control.Exception

-- | Socks Version
data SocksVersion = SocksVer5
                  deriving (Show,Eq)

data SocksCommand =
      SocksCommandConnect
    | SocksCommandBind
    | SocksCommandUdpAssociate
    | SocksCommandOther Word8
    deriving (Show,Eq,Ord)

data SocksMethod =
      SocksMethodNone
    | SocksMethodGSSAPI
    | SocksMethodUsernamePassword
    | SocksMethodOther Word8
    | SocksMethodNotAcceptable
    deriving (Show,Eq,Ord)

data SocksHostAddress =
      SocksAddrIPV4 HostAddress
    | SocksAddrDomainName ByteString
    | SocksAddrIPV6 HostAddress6
    deriving (Show,Eq)

data SocksAddress = SocksAddress SocksHostAddress PortNumber
    deriving (Show,Eq)

data SocksReply =
      SocksReplySuccess
    | SocksReplyError SocksError
    deriving (Show,Eq,Ord,Data,Typeable)

data SocksError =
      SocksErrorGeneralServerFailure
    | SocksErrorConnectionNotAllowedByRule
    | SocksErrorNetworkUnreachable
    | SocksErrorHostUnreachable
    | SocksErrorConnectionRefused
    | SocksErrorTTLExpired
    | SocksErrorCommandNotSupported
    | SocksErrorAddrTypeNotSupported
    | SocksErrorOther Word8
    deriving (Show,Eq,Ord,Data,Typeable)

data SocksVersionNotSupported = SocksVersionNotSupported
    deriving (Show,Data,Typeable)

instance Exception SocksError
instance Exception SocksVersionNotSupported

instance Enum SocksCommand where
    toEnum 1 = SocksCommandConnect
    toEnum 2 = SocksCommandBind
    toEnum 3 = SocksCommandUdpAssociate
    toEnum w
        | w < 256   = SocksCommandOther $ fromIntegral w
        | otherwise = error "socks command is only 8 bits"
    fromEnum SocksCommandConnect      = 1
    fromEnum SocksCommandBind         = 2
    fromEnum SocksCommandUdpAssociate = 3
    fromEnum (SocksCommandOther w)    = fromIntegral w

instance Enum SocksMethod where
    toEnum 0    = SocksMethodNone
    toEnum 1    = SocksMethodGSSAPI
    toEnum 2    = SocksMethodUsernamePassword
    toEnum 0xff = SocksMethodNotAcceptable
    toEnum w
        | w < 256   = SocksMethodOther $ fromIntegral w
        | otherwise = error "socks method is only 8 bits"
    fromEnum SocksMethodNone             = 0
    fromEnum SocksMethodGSSAPI           = 1
    fromEnum SocksMethodUsernamePassword = 2
    fromEnum (SocksMethodOther w)        = fromIntegral w
    fromEnum SocksMethodNotAcceptable    = 0xff

instance Enum SocksError where
    fromEnum SocksErrorGeneralServerFailure       = 1
    fromEnum SocksErrorConnectionNotAllowedByRule = 2
    fromEnum SocksErrorNetworkUnreachable         = 3
    fromEnum SocksErrorHostUnreachable            = 4
    fromEnum SocksErrorConnectionRefused          = 5
    fromEnum SocksErrorTTLExpired                 = 6
    fromEnum SocksErrorCommandNotSupported        = 7
    fromEnum SocksErrorAddrTypeNotSupported       = 8
    fromEnum (SocksErrorOther w)                  = fromIntegral w
    toEnum 1 = SocksErrorGeneralServerFailure
    toEnum 2 = SocksErrorConnectionNotAllowedByRule
    toEnum 3 = SocksErrorNetworkUnreachable
    toEnum 4 = SocksErrorHostUnreachable
    toEnum 5 = SocksErrorConnectionRefused
    toEnum 6 = SocksErrorTTLExpired
    toEnum 7 = SocksErrorCommandNotSupported
    toEnum 8 = SocksErrorAddrTypeNotSupported
    toEnum w = SocksErrorOther $ fromIntegral w

instance Enum SocksReply where
    fromEnum SocksReplySuccess                    = 0
    fromEnum (SocksReplyError e)                  = fromEnum e
    toEnum 0 = SocksReplySuccess
    toEnum n = SocksReplyError (toEnum n)
