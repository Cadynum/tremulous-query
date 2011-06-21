{-# LANGUAGE CPP #-}
module Tremulous.SocketExtensions where
import Control.DeepSeq
import Control.Monad
import Network.Socket
#ifndef linux_HOST_OS
import Network.BSD
import Data.Word
#endif

data DNSEntry = DNSEntry {dnsFamily :: !Family, dnsAddress :: !SockAddr} deriving Show

instance NFData DNSEntry where
	rnf (DNSEntry _ b) = rnf b

deriving instance Ord SockAddr

instance NFData SockAddr where
	rnf (SockAddrInet (PortNum p) h) 	= rnf p `seq` rnf h
	rnf (SockAddrInet6 (PortNum p) f h s)	= rnf p `seq` rnf f `seq` rnf h `seq` rnf s
#ifdef linux_HOST_OS
	rnf (SockAddrUnix s)			= rnf s
#endif
	
getDNS :: String -> String -> IO DNSEntry

#ifdef linux_HOST_OS
getDNS host_ port_ = do
	AddrInfo _ family _ _ addr _ <- head `liftM` getAddrInfo Nothing (Just host_) (Just port_)
	return $ DNSEntry family addr

#else
getDNS host_ port_ = do
	HostEntry _ _ family addr <- getHostByName host_
	let port = read port_ :: Word16
	return $ DNSEntry family (SockAddrInet (fromIntegral port) (head addr))
#endif
