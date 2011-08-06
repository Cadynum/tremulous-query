{-# LANGUAGE CPP, StandaloneDeriving #-}
module Network.Tremulous.SocketExtensions where
import Prelude as P
import Network.Socket
import Data.Word
import Data.Bits
import Data.ByteString.Char8
import Data.ByteString.Internal

deriving instance Ord SockAddr

getIPv4 :: Integral i => i -> i -> i -> i -> i -> i -> SockAddr
getIPv4 i0 i1 i2 i3 p0 p1 = SockAddrInet (PortNum port) ip
	where
	ip	= (f i3 .<<. 24) .|. (f i2 .<<. 16) .|. (f i1 .<<. 8) .|. f i0 :: Word32
	port	= (f p1 .<<. 8) .|. f p0 :: Word16
	f :: (Integral a, Integral b) => a -> b
	f = fromIntegral

putIPv4 :: SockAddr -> ByteString
putIPv4 (SockAddrInet (PortNum p) ip) = pack [i0, i1, i2, i3, p0, p1]
	where
	i0 = f ip
	i1 = f (ip .>>. 8)
	i2 = f (ip .>>. 16)
	i3 = f (ip .>>. 24)
	p0 = f p
	p1 = f (p .>>. 8)
	f :: Integral i => i -> Char
	f = w2c . fromIntegral
putIPv4 _ = empty


(.<<.) :: Bits a => a -> Int -> a
(.<<.) = shiftL

(.>>.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
