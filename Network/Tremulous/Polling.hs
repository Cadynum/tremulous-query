module Network.Tremulous.Polling (
	pollMasters, pollOne
) where
import Prelude hiding (all, concat, mapM_, elem, sequence_, concatMap, catch)

import Control.DeepSeq
import Control.Monad hiding (mapM_, sequence_)
import Control.Concurrent
import Control.Applicative
import Control.Exception

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.String
import Data.ByteString.Char8 (ByteString, append, pack)

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Tremulous.Protocol
import Network.Tremulous.ByteStringUtils as B
import Network.Tremulous.MicroTime
import Network.Tremulous.Scheduler

data QType = QMaster !Int !Int | QGame !Int | QJustWait deriving Show

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
	
	-- server addresses recieved by master
	mstate		<- newMVar S.empty
	-- Servers that has responded
	tstate		<- newMVar S.empty
	
	-- When first packet was sent to server (removed on proper response)
	pingstate	<- newMVar (M.empty :: Map SockAddr MicroTime)

	let sf sched host qtype = case qtype of
		QGame n -> do
			now <- getMicroTime
			when (n == packetDuplication) $
				pureModifyMVar pingstate $ M.insert host now
			sendTo sock getStatus host
			if (n > 0) then do
				addScheduled sched (now + fromIntegral packetTimeout, host, QGame (n-1))
			else do
				addScheduled sched (now + fromIntegral packetTimeout, host, QJustWait)
			
		QMaster n proto	-> do
			now <- getMicroTime
			sendTo sock (getServers proto) host
			if (n > 0) then do
				addScheduled sched (now + fromIntegral packetTimeout `div` 2 , host, QMaster (n-1) proto)
			else do
				addScheduled sched (now + fromIntegral packetTimeout, host, QJustWait)
				
		QJustWait -> return ()
		
	sched		<- newScheduler throughputDelay sf (Just (putMVar finished () >> sClose sock))
	addScheduledInstant sched $
		map (\MasterServer{..} -> (masterAddress, QMaster (packetDuplication*4) masterProtocol)) masterservers

	let buildResponse = do
		packet <- forceIO finished $ recvFrom sock mtu
		case parsePacket (map masterAddress masterservers) <$> packet of
			-- The master responded, great! Now lets send requests to the new servers
			Just (Master host x) -> do
				deleteScheduled sched host
				m <- takeMVar mstate
				putMVar' mstate (S.union m x)
				let delta = S.difference x m
				when (S.size delta > 0) $
					addScheduledInstant sched $ map (,QGame packetDuplication) (S.toList delta)
				
				buildResponse

			Just (Tremulous host x) -> do
				now <- getMicroTime
				t <- takeMVar tstate
				if S.member host t then do
					putMVar' tstate t
					buildResponse
				else do
					deleteScheduled sched host
					ps	<- takeMVar pingstate
					start	<- return $! M.lookup host ps
					putMVar' pingstate $ M.delete host ps
					putMVar' tstate $ S.insert host t
					-- This also serves as protection against
					-- receiving responses for requests never sent
					case start of
						Nothing -> buildResponse
						Just a	-> do
							let gameping = fromIntegral (now - a) `div` 1000
							( strict x{ gameping } : ) `liftM` buildResponse			
			Just Invalid -> buildResponse
			
			Nothing -> return []

	xs	<- buildResponse
	m	<- takeMVar mstate
	t	<- takeMVar tstate
	return $! PollResult xs (S.size t) (S.size m) t
		
	where forceIO m f = catch (Just <$> f) $ \(_ :: IOError) -> do
		b <- isEmptyMVar m 
		if b then forceIO m f else return Nothing

data Packet = Master !SockAddr !(Set SockAddr) | Tremulous !SockAddr !GameServer | Invalid

parsePacket :: [SockAddr] -> (ByteString, SockAddr) -> Packet
parsePacket masters (content, host) = case B.stripPrefix "\xFF\xFF\xFF\xFF" content of
	Just a	| Just x <- parseServer a			-> Tremulous host x
		| Just x <- parseMaster a, host `elem` masters	-> Master host x
	_							-> Invalid
	where
	parseMaster x = S.fromList . parseMasterServer <$> stripPrefix "getserversResponse" x
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

putMVar' :: MVar a -> a -> IO ()
putMVar' m a = a `seq` putMVar m a

pureModifyMVar :: MVar a -> (a -> a) -> IO ()
pureModifyMVar m f = putMVar' m . f =<< takeMVar m
	
strict :: NFData a => a -> a
strict x = x `deepseq` x

whileJust :: Monad m => a -> (a -> m (Maybe a)) -> m ()
whileJust x f  = f x >>= \c -> case c of
	Just a	-> whileJust a f
	Nothing	-> return ()

