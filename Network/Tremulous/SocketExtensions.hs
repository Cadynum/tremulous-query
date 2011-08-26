{-# LANGUAGE CPP, StandaloneDeriving #-}
module Network.Tremulous.SocketExtensions where
import Prelude as P
import Foreign
import Control.DeepSeq
import Network.Socket

deriving instance Ord SockAddr

instance NFData SockAddr where
	rnf (SockAddrInet (PortNum a) b) = rnf a `seq` rnf b
	rnf (SockAddrInet6 (PortNum a) b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
	rnf (SockAddrUnix a) = rnf a
#endif

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define CALLCONV stdcall
#else
#define CALLCONV ccall
#endif

foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32
foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
