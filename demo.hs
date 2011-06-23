import Network.Socket
import Tremulous.Protocol
import Tremulous.Polling
import Tremulous.Util
import Tremulous.SocketExtensions
import Data.Maybe
import Control.Applicative
import Data.ByteString.Char8
import Control.Monad

delay = Delay (800*1000) 1 (5000)
data DNSEntry = DNSEntry {dnsFamily :: !Family, dnsAddress :: !SockAddr} deriving Show

getDNS :: String -> String -> IO DNSEntry

getDNS host_ port_ = do
	AddrInfo _ family _ _ addr _ <- Prelude.head `liftM` getAddrInfo Nothing (Just host_) (Just port_)
	return $ DNSEntry family addr


main = do
	host <- getDNS "master.tremulous.net" "30710"
	host2 <- getDNS "master.tremulous.net" "30700"
	polledd <- pollMasters delay [ MasterServer 69 (dnsAddress host)
				, MasterServer 70 (dnsAddress host2)]
	--print $ map fmt polledd
	mapM (\Player{..} -> Prelude.putStrLn $
		unpackorig name ++ "\t\t" ++  unpack (cleanedCase name) )  (playerList polledd)


