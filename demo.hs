import Network.Socket
import Tremulous.Protocol
import Tremulous.Polling
import Tremulous.Util
import Tremulous.SocketExtensions
import Data.Maybe
import Control.Applicative
import Data.ByteString.Char8

delay = Delay (400*1000) 3 0


main = do
	host <- getDNS "master.tremulous.net" "30710"
	host2 <- getDNS "master.tremulous.net" "30700"
	polledd <- pollMasters delay [ MasterServer 69 (dnsAddress host)
				, MasterServer 70 (dnsAddress host2)]
	--print $ map fmt polledd
	mapM (\Player{..} -> Prelude.putStrLn $
		unpackorig name ++ "\t\t" ++  unpack (cleanedCase name) )  (playerList polledd)


