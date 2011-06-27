module Tremulous.Polling (
	pollMasters
	, pollOne
) where
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import System.Timeout

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Chan.Strict
import Control.Concurrent.MVar.Strict
import Data.Foldable
import Control.Monad hiding (mapM_, sequence_)
import Prelude hiding (all, concat, mapM_, elem, sequence_, concatMap, catch)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative
import Control.Exception
import Data.String
import Data.ByteString.Char8 (ByteString, append, pack)
import System.Time

import Tremulous.Protocol
import Tremulous.ByteStringUtils as B

mtu :: Int
mtu = 1500

getStatus :: IsString s => s
getStatus = "\xFF\xFF\xFF\xFFgetstatus"
getServers :: Int -> ByteString
getServers proto = "\xFF\xFF\xFF\xFFgetservers " `append` pack (show proto) `append` " empty full"

pollMasters :: Delay -> [MasterServer] -> IO [GameServer]
pollMasters Delay{..} masterservers = do
	sock		<- socket AF_INET Datagram defaultProtocol
	chan		<- newChan --Packets will be streamed here
	--mstate		<- newMVar S.empty --Current masterlist
	-- (master address, server addresses given by said master)
	mstate		<- newMVar $ ((M.fromList $ map (\x -> (masterHost x, S.empty)) masterservers) :: (Map SockAddr (Set SockAddr)))
	-- Servers that has responded
	tstate		<- newMVar S.empty
	-- When first packet was sent to server
	pingstate	<- newMVar (M.empty :: Map SockAddr Integer)
	recvThread	<- forkIO . forever $ (writeChan chan . Just) =<< recvFrom sock mtu
	-- The incoming data will be sent here
	servers		<- newChan

	outchan		<- newChan :: IO (Chan (ByteString, SockAddr))
	
	outbuffer <- forkIO $ forever $ do
		(packet, to)	<- readChan outchan
		-- set timestamp of sent packet
		now		<- getMicroTime
		x		<- takeMVar pingstate
		putMVar pingstate $ M.insertWith' (\_ b -> b) to now x
		sendTo sock packet to
		threadDelay outBufferDelay
		
	let bufferedSendTo a b = writeChan outchan (a, b)

	-- Since the masterserver's response offers no indication if the result is complete,
	-- we play safe by sending a couple of requests
	forkIO . replicateM_ 3 $ do
		mapM_ (\(MasterServer protocol masterHost) -> bufferedSendTo (getServers protocol) masterHost) masterservers
		threadDelay (100*1000)

	forkIO . whileJust resendTimes $ \n -> do
		threadDelay resendWait
		if n == 0 then do
			killThread recvThread
			killThread outbuffer
			writeChan chan Nothing
			return Nothing
		else do
			m <- S.unions . M.elems <$> readMVar mstate
			t <- readMVar tstate
			let deltas = S.toList $  t `S.difference` m
			mapM_ (bufferedSendTo getStatus) deltas
			return (Just (n-1))


	forkIO . whileTrue $ do
		packet <- readChan chan
		case parsePacket (masterHost <$> masterservers) <$> packet of
			--Time to stop parsing
			Nothing -> do
				writeChan servers Nothing
				sClose sock
				return False
			-- The master responded, great! Now lets send requests to the new servers
			Just (Master host x) -> do
				mvar <- takeMVar mstate
				let m = M.findWithDefault S.empty host mvar
				let m' = S.union m x
				putMVar mstate $ M.insertWith S.union host m' mvar
				let delta = S.difference x m
				when (S.size m' > S.size m) $ do
					mapM_ (bufferedSendTo getStatus) delta

				return True

			Just (Tremulous host x) -> do
				now <- getMicroTime
				t <- takeMVar tstate
				if S.member host t then
					putMVar tstate t
				else do
					putMVar tstate $ S.insert host t
					-- This also servers as protection against
					-- receiving responses for requests never sent
					start <- M.lookup host <$> readMVar pingstate
					whenJust start $ \a -> do
						let gameping = fromInteger (now - a) `div` 1000
						writeChan servers (Just x {gameping})					
				return True

			Just Invalid -> return True
	lazyList servers

	where
	lazyList c = do
		x <- readChan c
		case x of
			Just a	-> liftM (a:) (lazyList c)
			Nothing	-> return []

{-
findOrigin :: SockAddr -> [(SockAddr, Set SockAddr)] -> Maybe SockAddr
findOrigin _ [] 		= Nothing
findOrigin host ((k, v):xs)
	| S.member host v	= Just k
	| otherwise		= findOrigin host xs
-}

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
pollOne Delay{..} sockaddr = do
	s <- socket AF_INET Datagram defaultProtocol
	catch (f s) (err s)
	where
	f sock = do
		connect sock sockaddr
		send sock getStatus
		start	<- getMicroTime
		poll	<- timeout resendWait $ recv sock mtu
		stop	<- getMicroTime
		let gameping = fromInteger (stop - start) `div` 1000
		return $ (\x -> x {gameping}) <$> 
			(parseGameServer sockaddr =<< isProper =<< poll)
	err sock (_::IOError) = sClose sock >> return Nothing
	isProper = stripPrefix "\xFF\xFF\xFF\xFFstatusResponse"

getMicroTime :: IO Integer
getMicroTime = let f (TOD s p) = s*1000000 + p `div` 1000000 in f <$> getClockTime

whileTrue :: (Monad m) => m Bool -> m ()
whileTrue f = f >>= \c -> if c then whileTrue f else return ()

whileJust :: Monad m => a -> (a -> m (Maybe a)) -> m ()
whileJust x f  = f x >>= \c -> case c of
	Just a	-> whileJust a f
	Nothing	-> return ()

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust x f = case x of
	Just a	-> f a
	Nothing	-> return ()
