module Tremulous.Polling (
	pollMasters
	, pollOne
) where
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import System.Timeout
import System.IO.Unsafe

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Chan.Strict
import Control.Concurrent.MVar.Strict
import Control.Concurrent.STM
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

polltimeout, singlepolltimeout, resendPacketsTimes :: Int
polltimeout		= 400*1000
singlepolltimeout	= 800*1000
resendPacketsTimes	= 3

getStatus :: IsString s => s
getStatus = "\xFF\xFF\xFF\xFFgetstatus"
getServers :: Int -> ByteString
getServers proto = "\xFF\xFF\xFF\xFFgetservers " `append` pack (show proto) `append` " empty full"

pollMasters :: [MasterServer] -> IO [GameServer]
pollMasters masterservers = do
	sock		<- socket AF_INET Datagram defaultProtocol
	chan		<- newChan --Packets will be streamed here
	--mstate		<- newMVar S.empty --Current masterlist
	mstate		<- newMVar $ ((M.fromList $ map (\x -> (masterHost x, S.empty)) masterservers) :: (Map SockAddr (Set SockAddr)))
	tstate		<- newMVar S.empty
	recvThread	<- forkIO . forever $ (writeChan chan . Just) =<< recvFrom sock 1500
	servers		<- newChan --The incoming data will be sent here
	pingInit	<- atomically newEmptyTMVar

	-- Since the masterserver's response offers no indication if the result is complete,
	-- we play safe by sending a couple of requests
	forkIO . replicateM_ resendPacketsTimes $ do
		mapM_ (\(MasterServer protocol masterHost) -> sendTo sock (getServers protocol) masterHost) masterservers
		threadDelay (200*1000)

	forkIO . whileJust resendPacketsTimes $ \n -> do
		threadDelay polltimeout
		if n == 0 then do
			killThread recvThread
			writeChan chan Nothing
			return Nothing
		else do
			m <- M.elems <$> readMVar mstate
			t <- readMVar tstate
			let deltas = concatMap (S.toList) $ map (`S.difference` t) m
			mapM_ (sendTo sock getStatus) deltas
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
				when (S.size m' > S.size m) $ do
					mapM_ (sendTo sock getStatus) (S.difference x m)
				
				now <- getMicroTime
				atomically $ do
					tst <-isEmptyTMVar pingInit
					when tst $
						putTMVar pingInit now
					
				return True

			Just (Tremulous host x) -> do
				t <- takeMVar tstate
				now <- getMicroTime
				start <- atomically (readTMVar pingInit)
				let gameping = fromInteger (now - start) 
					`div` 1000
				if S.member host t
					then putMVar tstate t
					else do
						putMVar tstate $ S.insert host t
						writeChan servers (Just x {gameping})
				return True

			Just Invalid -> return True
	lazyList servers

	where
	lazyList c = unsafeInterleaveIO $ do
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

whileTrue :: (Monad m) => m Bool -> m ()
whileTrue f = f >>= \c -> if c then whileTrue f else return ()

whileJust :: Monad m => a -> (a -> m (Maybe a)) -> m ()
whileJust x f  = f x >>= \c -> case c of
	Just a	-> whileJust a f
	Nothing	-> return ()




data Packet = Master !SockAddr !(Set SockAddr) | Tremulous !SockAddr !GameServer | Invalid

parsePacket :: [SockAddr] -> (ByteString, SockAddr) -> Packet
parsePacket masters (content, host) = case B.stripPrefix "\xFF\xFF\xFF\xFF" content of
	Just (parseServer -> Just x) -> Tremulous host x
	Just (parseMaster -> Just x) | host `elem` masters -> Master host x
	_ -> Invalid
	where
	parseMaster x = S.fromList . parseMasterServer <$> stripPrefix "getserversResponse" x
	parseServer x = parseGameServer host =<< stripPrefix "statusResponse" x



pollOne :: SockAddr -> IO (Maybe GameServer)
pollOne sockaddr = do
	s <- socket AF_INET Datagram defaultProtocol
	catch (f s) (err s)
	where
	f sock = do
		connect sock sockaddr
		send sock getStatus
		start	<- getMicroTime
		poll	<- timeout singlepolltimeout $ recv sock 1500
		stop	<- getMicroTime
		let gameping = fromInteger (stop - start) `div` 1000
		return $ (\x -> x {gameping}) <$> 
			(parseGameServer sockaddr =<< isProper =<< poll)
	err sock (_::IOError) = sClose sock >> return Nothing
	isProper = stripPrefix "\xFF\xFF\xFF\xFFstatusResponse"

getMicroTime :: IO Integer
getMicroTime = let f (TOD s p) = s*1000000 + p `div` 1000000 in f <$> getClockTime
