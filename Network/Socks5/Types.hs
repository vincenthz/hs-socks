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

import qualified Basement.String as UTF8
import           Basement.Compat.IsList
import Data.ByteString (ByteString)
import Data.Word
import Data.Data
import Network.Socket (HostAddress, HostAddress6, PortNumber)
import Control.Exception
import qualified Data.ByteString as B
import Numeric (showHex)
import Data.List (intersperse)

-- | Socks Version
data SocksVersion = SocksVer5
                  deriving (Show,Eq,Ord)

-- | Command that can be send and receive on the SOCKS protocol
data SocksCommand =
      SocksCommandConnect
    | SocksCommandBind
    | SocksCommandUdpAssociate
    | SocksCommandOther !Word8
    deriving (Show,Eq,Ord)

-- | Authentication methods available on the SOCKS protocol.
--
-- Only SocksMethodNone is effectively implemented, but
-- other value are enumerated for completeness.
data SocksMethod =
      SocksMethodNone
    | SocksMethodGSSAPI
    | SocksMethodUsernamePassword
    | SocksMethodOther !Word8
    | SocksMethodNotAcceptable
    deriving (Show,Eq,Ord)

-- | A Host address on the SOCKS protocol.
data SocksHostAddress =
      SocksAddrIPV4 !HostAddress
    | SocksAddrDomainName !FQDN
    | SocksAddrIPV6 !HostAddress6
    deriving (Eq,Ord)

type FQDN = ByteString

instance Show SocksHostAddress where
    show (SocksAddrIPV4 ha)       = "SocksAddrIPV4(" ++ showHostAddress ha ++ ")"
    show (SocksAddrIPV6 ha6)      = "SocksAddrIPV6(" ++ showHostAddress6 ha6 ++ ")"
    show (SocksAddrDomainName dn) = "SocksAddrDomainName(" ++ showFQDN dn ++ ")"

-- | Converts a FQDN to a String
showFQDN :: FQDN -> String
showFQDN bs = toList $ fst $ UTF8.fromBytesLenient $ fromList $ B.unpack bs

-- | Converts a HostAddress to a String in dot-decimal notation
showHostAddress :: HostAddress -> String
showHostAddress num = concat [show q1, ".", show q2, ".", show q3, ".", show q4]
  where (num',q1)   = num `quotRem` 256
        (num'',q2)  = num' `quotRem` 256
        (num''',q3) = num'' `quotRem` 256
        (_,q4)      = num''' `quotRem` 256

-- | Converts a IPv6 HostAddress6 to standard hex notation
showHostAddress6 :: HostAddress6 -> String
showHostAddress6 (a,b,c,d) =
    (concat . intersperse ":" . map (flip showHex ""))
        [p1,p2,p3,p4,p5,p6,p7,p8]
    where (a',p2) = a `quotRem` 65536
          (_,p1)  = a' `quotRem` 65536
          (b',p4) = b `quotRem` 65536
          (_,p3)  = b' `quotRem` 65536
          (c',p6) = c `quotRem` 65536
          (_,p5)  = c' `quotRem` 65536
          (d',p8) = d `quotRem` 65536
          (_,p7)  = d' `quotRem` 65536

-- | Describe a Socket address on the SOCKS protocol
data SocksAddress = SocksAddress !SocksHostAddress !PortNumber
    deriving (Show,Eq,Ord)

-- | Type of reply on the SOCKS protocol
data SocksReply =
      SocksReplySuccess
    | SocksReplyError SocksError
    deriving (Show,Eq,Ord,Data,Typeable)

-- | SOCKS error that can be received or sent
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

-- | Exception returned when using a SOCKS version that is not supported.
--
-- This package only implement version 5.
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
