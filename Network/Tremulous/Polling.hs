module Network.Tremulous.Polling (
	pollMasters, pollOne
) where
import Prelude hiding (all, concat, mapM_, elem, sequence_, concatMap, catch, Maybe(..), maybe, foldr)
import qualified Data.Maybe as P

import Control.Monad (when)
import Control.Concurrent
import Control.Applicative
import Control.Exception

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.String
import Data.ByteString.Char8 (ByteString, append, pack)

import Network.Tremulous.StrictMaybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Tremulous.Protocol
import Network.Tremulous.ByteStringUtils as B
import Network.Tremulous.MicroTime
import Network.Tremulous.Scheduler

data QType = QMaster !Int !Int | QGame !Int | QJustWait
data State = Pending | Requested !MicroTime | Responded

mtu :: Int
mtu = 2048

getStatus :: IsString s => s
getStatus = "\xFF\xFF\xFF\xFFgetstatus"
getServers :: Int -> ByteString
getServers proto = "\xFF\xFF\xFF\xFFgetservers " `append` pack (show proto) `append` " empty full"


pollMasters :: Delay -> [MasterServer] -> IO PollResult
pollMasters Delay{..} masterservers = do
	sock		<- socket AF_INET Datagram defaultProtocol
	bindSocket sock (SockAddrInet 0 0)

	finished	<- newEmptyMVar

	state		<- newMVar (M.empty :: Map SockAddr State)

	let sf sched host qtype = case qtype of
		QGame n -> do
			now <- getMicroTime
			-- The first packet sent, which is why it's okey to just insert and replace
			-- the current Pending value
			when (n == packetDuplication) $
				pureModifyMVar state $ M.insert host (Requested now)
			sendTo sock getStatus host
			addScheduled sched $ if (n > 0)
				then E (now + fromIntegral packetTimeout) host (QGame (n-1))
				else E (now + fromIntegral packetTimeout) host QJustWait

		QMaster n proto	-> do
			now <- getMicroTime
			sendTo sock (getServers proto) host
			addScheduled sched $ if (n > 0)
				then E (now + fromIntegral packetTimeout `div` 2) host (QMaster (n-1) proto)
				else E (now + fromIntegral packetTimeout) host QJustWait

		QJustWait -> return ()

	sched		<- newScheduler throughputDelay sf (Just (putMVar finished () >> sClose sock))
	addScheduledInstant sched $
		map (\MasterServer{..} -> (masterAddress, QMaster (packetDuplication*4) masterProtocol)) masterservers

	let buildResponse = do
		packet <- forceIO finished $ recvFrom sock mtu
		case parsePacket (map masterAddress masterservers) <$> packet of
			-- The master responded, great! Now lets send requests to the new servers
			Just (Master host xs) -> do
				deleteScheduled sched host
				s <- takeMVar state
				let (s', delta) = foldr masterRoll (s, []) xs
				addScheduledInstant sched $ map (,QGame packetDuplication) delta
				putMVar' state s'

				buildResponse

			Just (Tremulous host x) -> do
				now <- getMicroTime
				s <- takeMVar state
				case M.lookup host s of
					P.Just (Requested start) -> do
						deleteScheduled sched host
						putMVar' state $ M.insert host Responded s
						let gameping = fromIntegral (now - start) `div` 1000
						(x{ gameping } :) <$> buildResponse
					_ -> do
						putMVar' state s
						buildResponse
			Just Invalid -> buildResponse

			Nothing -> return []

	xs	<- buildResponse
	s	<- takeMVar state
	let (nResp, nNot) = ssum s
	return (PollResult xs nResp (nResp+nNot))

	where
	forceIO m f = catch (Just <$> f) $ \(_ :: IOError) -> do
		b <- isEmptyMVar m
		if b then forceIO m f else return Nothing
	masterRoll host ~(!m, xs) | M.member host m = (m, xs)
			          | otherwise       = (M.insert host Pending m, host:xs)
	ssum = foldl' f (0, 0) where
		f (!a, !b) Responded = (a+1, b)
		f (!a, !b) _         = (a, b+1)
data Packet = Master !SockAddr ![SockAddr] | Tremulous !SockAddr !GameServer | Invalid

parsePacket :: [SockAddr] -> (ByteString, SockAddr) -> Packet
parsePacket masters (content, host) = case B.stripPrefix "\xFF\xFF\xFF\xFF" content of
	Just a	| Just x <- parseServer a			-> Tremulous host x
		| Just x <- parseMaster a, host `elem` masters	-> Master host x
	_							-> Invalid
	where
	parseMaster x = parseMasterServer <$> stripPrefix "getserversResponse" x
	parseServer x = parseGameServer host =<< stripPrefix "statusResponse" x


pollOne :: Delay -> SockAddr -> IO (Maybe GameServer)
pollOne Delay{..} sockaddr = handle err $ bracket (socket AF_INET Datagram defaultProtocol) sClose $ \sock -> do
	connect sock sockaddr
	pid <- forkIO $ whileJust packetDuplication $ \n -> do
		send sock getStatus
		threadDelay packetTimeout
		if n > 0 then
			return $ Just (n-1)
		else do
			sClose sock
			return Nothing

	start	<- getMicroTime
	poll	<- ioMaybe $ recv sock mtu <* killThread pid
	stop	<- getMicroTime
	let gameping = fromIntegral (stop - start) `div` 1000
	return $ (\x -> x {gameping}) <$>
		(parseGameServer sockaddr =<< isProper =<< poll)
	where
	err (_ :: IOError) = return Nothing
	isProper = stripPrefix "\xFF\xFF\xFF\xFFstatusResponse"

ioMaybe :: IO a -> IO (Maybe a)
ioMaybe f = catch (Just <$> f) (\(_ :: IOError) -> return Nothing)
