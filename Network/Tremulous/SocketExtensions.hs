{-# LANGUAGE CPP, StandaloneDeriving #-}
module Network.Tremulous.SocketExtensions where
import Network.Socket

deriving instance Ord SockAddr
