{-# LANGUAGE RecordWildCards #-}
import Network.Socket
import Network.Tremulous.Protocol
import Network.Tremulous.Polling
import Network.Tremulous.Util
import Control.Monad

delay = Delay (400*1000) 3 (0)

getDNS :: String -> String -> IO SockAddr
getDNS host port =  do
	AddrInfo _ _ _ _ addr _ <- Prelude.head `liftM` getAddrInfo Nothing (Just host) (Just port)
	return $  addr


main = withSocketsDo $ do
	host	<- getDNS "master.tremulous.net" "30710"
	host2	<- getDNS "master.tremulous.net" "30700"
	PollMasters{..}	<- pollMasters delay	[ MasterServer 69 host
					, MasterServer 70 host2 ]

	print $ serversResponded


