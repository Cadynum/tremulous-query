{-# LANGUAGE CPP #-}
module Tremulous.SocketExtensions where
import Control.DeepSeq
import Network.Socket

deriving instance Ord SockAddr

instance NFData SockAddr where
	rnf (SockAddrInet (PortNum p) h) 	= rnf p `seq` rnf h
	rnf (SockAddrInet6 (PortNum p) f h s)	= rnf p `seq` rnf f `seq` rnf h `seq` rnf s
#ifdef linux_HOST_OS
	rnf (SockAddrUnix s)			= rnf s
#endif
	

