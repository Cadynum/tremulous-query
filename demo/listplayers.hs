-- This program NEEDS to be compiled with -threaded
{-# LANGUAGE RecordWildCards #-}
import Network.Socket
import Network.Tremulous.Protocol
import Network.Tremulous.Polling
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Text.Printf

getDNS :: String -> String -> IO SockAddr
getDNS host port =  do
	AddrInfo _ _ _ _ addr _ <- Prelude.head `liftM` getAddrInfo Nothing (Just host) (Just port)
	return $  addr

main :: IO ()
main = withSocketsDo $ do
	host	<- getDNS "master.tremulous.net" "30710"
	host2	<- getDNS "master.tremulous.net" "30700"
	--host	<- getDNS "master.urbanterror.net" "27950"
	PollResult{..}	<- pollMasters defaultDelay	[ MasterServer host 69, MasterServer host2 70 ]
	printf "%d servers responded out of %d\n" serversResponded serversRequested
	let plist = concatMap players polled
	mapM_ (B.putStrLn . original . name)  plist


