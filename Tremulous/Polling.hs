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
	bindSocket sock (SockAddrInet 0 0)
	
	-- server addresses recieved by master
	mstate		<- newMVar S.empty
	-- Servers that has responded
	tstate		<- newMVar S.empty
	
	-- When first packet was sent to server (removed on proper response)
	pingstate	<- newMVar (M.empty :: Map SockAddr Integer)

	
	-- The incoming data will be sent here
	-- servers		<- newChan
	outchan		<- newChan :: IO (Chan (ByteString, SockAddr))
	-- Buffer used for gameserver requests
	outbuffer <- forkIO $ forever $ do
		(packet, to)	<- readChan outchan
		t		<- readMVar tstate

		-- only proceed if there is no response yet.
		when (not $ S.member to t) $ do
			-- set timestamp of sent packet
			now		<- getMicroTime
			x		<- takeMVar pingstate
			putMVar pingstate $ M.insertWith' (\_ b -> b) to now x
			sendTo sock packet to
			threadDelay outBufferDelay


	let bufferedSendTo a b = writeChan outchan (a, b)
	let directSendTo = sendTo sock

	-- Since the masterserver's response offers no indication if the result is complete,
	-- we play safe by sending a couple of requests
	forkIO . replicateM_ 3 $ do
		mapM_ (\(MasterServer protocol masterHost) -> directSendTo (getServers protocol) masterHost) masterservers
		threadDelay (100*1000)

	forkIO . whileJust resendTimes $ \n -> do
		threadDelay resendWait
		if n == 0 then do
			killThread outbuffer
			sClose sock
			return Nothing
		else do
			m <- readMVar mstate
			t <- readMVar tstate
			let delta = t `S.difference` m
			mapM_ (bufferedSendTo getStatus) delta
			return (Just (n-1))

	let buildResponse = do
		packet <- catch (Just <$> recvFrom sock mtu) (\(_ :: IOError) -> return Nothing)
		case parsePacket (masterHost <$> masterservers) <$> packet of
			-- The master responded, great! Now lets send requests to the new servers
			Just (Master _ x) -> do
				m <- takeMVar mstate
				let m' = S.union m x
				putMVar mstate  m'
				let delta = S.difference x m
				when (S.size m' > S.size m) $
					mapM_ (bufferedSendTo getStatus) delta

				buildResponse

			Just (Tremulous host x) -> do
				now <- getMicroTime
				t <- takeMVar tstate
				if S.member host t then do
					putMVar tstate t
					buildResponse
				else do	
					ps	<- takeMVar pingstate
					start	<- return $! M.lookup host ps
					putMVar pingstate $ M.delete host ps
					putMVar tstate $ S.insert host t
					-- This also servers as protection against
					-- receiving responses for requests never sent
					case start of
						Nothing -> buildResponse
						Just a	-> do
							let gameping = fromInteger (now - a) `div` 1000
							( x{ gameping } : ) `liftM` buildResponse			
			Just Invalid -> buildResponse
			--Time to stop parsing
			Nothing -> return []
	buildResponse

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
