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
    | SocksReplyGeneralServerFailure
    | SocksReplyConnectionNotAllowedByRule
    | SocksReplyNetworkUnreachable
    | SocksReplyHostUnreachable
    | SocksReplyConnectionRefused
    | SocksReplyTTLExpired
    | SocksReplyCommandNotSupported
    | SocksReplyAddrTypeNotSupported
    | SocksReplyOther Word8
    deriving (Show,Eq,Ord,Data,Typeable)

data SocksVersionNotSupported = SocksVersionNotSupported
    deriving (Show,Data,Typeable)

data SocksError = SocksError SocksReply
    deriving (Show,Eq,Data,Typeable)

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

instance Enum SocksReply where
    fromEnum SocksReplySuccess                    = 0
    fromEnum SocksReplyGeneralServerFailure       = 1
    fromEnum SocksReplyConnectionNotAllowedByRule = 2
    fromEnum SocksReplyNetworkUnreachable         = 3
    fromEnum SocksReplyHostUnreachable            = 4
    fromEnum SocksReplyConnectionRefused          = 5
    fromEnum SocksReplyTTLExpired                 = 6
    fromEnum SocksReplyCommandNotSupported        = 7
    fromEnum SocksReplyAddrTypeNotSupported       = 8
    fromEnum (SocksReplyOther w)                  = fromIntegral w
    toEnum 0 = SocksReplySuccess
    toEnum 1 = SocksReplyGeneralServerFailure
    toEnum 2 = SocksReplyConnectionNotAllowedByRule
    toEnum 3 = SocksReplyNetworkUnreachable
    toEnum 4 = SocksReplyHostUnreachable
    toEnum 5 = SocksReplyConnectionRefused
    toEnum 6 = SocksReplyTTLExpired
    toEnum 7 = SocksReplyCommandNotSupported
    toEnum 8 = SocksReplyAddrTypeNotSupported
    toEnum w
        | w < 256   = SocksReplyOther $ fromIntegral w
        | otherwise = error "sock reply is only 8 bits"
