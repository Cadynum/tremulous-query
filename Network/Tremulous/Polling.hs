module Network.Tremulous.Polling (
    pollMasters
    , pollOne
) where
import Prelude hiding (all, concat, mapM_, elem, sequence_, concatMap
    , catch, Maybe(..), maybe, foldr)
import qualified Data.Maybe as P

import Control.Monad (when)
import Control.Concurrent
import Control.Applicative
import Control.Exception

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.String
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.Tremulous.StrictMaybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Tremulous.Protocol
import Network.Tremulous.ByteStringUtils as B
import Network.Tremulous.MicroTime
import Network.Tremulous.Scheduler


data QType  = QMaster !Int !Int | QGame !Int | QJustWait

data State  = Pending MasterServer
            | Requested !MicroTime MasterServer
            | Responded
            | Broken
    deriving Eq

data Packet = Master !SockAddr ![SockAddr]
            | Tremulous !SockAddr !GameServer
            | Invalid

mtu :: Int
mtu = 2048

getStatus :: IsString s => s
getStatus = "\xFF\xFF\xFF\xFFgetstatus"

getServers :: Int -> ByteString
getServers proto = B.concat [ "\xFF\xFF\xFF\xFFgetservers "
                            , B.pack (show proto)
                            , " empty full" ]


pollMasters :: Delay -> [MasterServer] -> IO PollResult
pollMasters Delay{..} masterservers = do
    sock        <- socket AF_INET Datagram defaultProtocol
    bindSocket sock (SockAddrInet 0 0)

    finished    <- newEmptyMVar
    state       <- newMVar (M.empty :: Map SockAddr State)

    let finalizer = putMVar finished () >> sClose sock
    sched <- newScheduler throughputDelay
                (schedFunc sock state)
                (Just finalizer)

    addScheduledInstant sched $ map instantMaster masterservers

    let buildResponse = do
            packet <- ioForce finished $ recvFrom sock mtu
            case packet of
                Nothing -> return []
                Just a  -> do
                    res <- buildOne sched state a
                    case res of
                        Nothing -> buildResponse
                        Just b  -> (b:) <$> buildResponse

    xs      <- buildResponse
    s       <- takeMVar state

    let (nResp, nNot) = count (==Responded) s
    return (PollResult xs nResp (nResp+nNot))

    where
    ptimeout = fromIntegral packetTimeout

    schedFunc sock state sched host qtype = case qtype of
        QGame n -> do
            now <- getMicroTime
            -- The first packet sent, which is why it's okey to just
            -- insert and replace the current Pending value
            when (n == packetDuplication) $
                pureModifyMVar state $ M.update (\x -> case x of
                    Pending ms -> P.Just (Requested now ms)
                    _          -> P.Nothing)
                    host
            sendTo sock getStatus host
            addScheduled sched $ if n > 0
                then E (now + ptimeout) host (QGame (n-1))
                else E (now + ptimeout) host QJustWait

        QMaster n proto -> do
            now <- getMicroTime
            sendTo sock (getServers proto) host
            addScheduled sched $ if n > 0
                then E (now + ptimeout `quot` 2) host (QMaster (n-1) proto)
                else E (now + ptimeout) host QJustWait

        QJustWait -> return ()

    instantMaster MasterServer{..} =
        ( masterAddress
        , QMaster (packetDuplication*4) masterProtocol )

    buildOne sched state packet = do
        now <- getMicroTime
        case parsePacket packet of
            Master host xs -> do
                forM_ (findMaster host) $ \masterServer -> do
                    deleteScheduled sched host
                    s <- takeMVar state
                    let (s', delta) = foldr (masterRoll masterServer) (s, []) xs
                    addScheduledInstant sched $ map (,QGame packetDuplication) delta
                    putMVar' state s'
                return Nothing

            Tremulous host x -> do
                s <- takeMVar state
                case M.lookup host s of
                    P.Just (Requested start MasterServer{..}) -> do
                        deleteScheduled sched host
                        -- This one is for you, devhc
                        if protocol x == masterProtocol then do
                            putMVar' state $ M.insert host Responded s
                            let ping' = fromIntegral (now - start) `quot` 1000
                            return $ Just x{gameping = ping'}
                        else do
                            putMVar' state $ M.insert host Broken s
                            return Nothing
                    _ -> do
                        putMVar' state s
                        return Nothing

            Invalid -> return Nothing


    findMaster host = find (\x -> host == masterAddress x) masterservers
    masterRoll ms host ~(!m, xs)
      | M.member host m = (m, xs)
      | otherwise       = (M.insert host (Pending ms) m, host:xs)

count :: (Integral i, Foldable f) => (a -> Bool) -> f a -> (i, i)
count p = foldl' go (0, 0) where
    go (!a, !b) x | p x       = (a+1, b)
                  | otherwise = (a, b+1)


parsePacket :: (ByteString, SockAddr) -> Packet
parsePacket (content, host) = case stripPrefix "\xFF\xFF\xFF\xFF" content of
    Just a | Just x <- parseServer a    -> Tremulous host x
           | Just x <- parseMaster a    -> Master host x
    _                                   -> Invalid
    where
    parseMaster x = parseMasterServer <$> stripPrefix "getserversResponse" x
    parseServer x = parseGameServer host =<< stripPrefix "statusResponse" x

pollOne :: Delay -> SockAddr -> IO (Maybe GameServer)
pollOne Delay{..} sockaddr = mkS $ \sock -> do
    connect sock sockaddr
    tid <- forkIO $ whileJust packetDuplication $ \n -> do
        send sock getStatus
        threadDelay packetTimeout
        if n > 0 then
            return $ Just (n-1)
        else do
            sClose sock
            return Nothing

    start   <- getMicroTime
    poll    <- ioMaybe $ recv sock mtu <* killThread tid
    stop    <- getMicroTime
    let gameping = fromIntegral (stop - start) `quot` 1000
    return $ (\x -> x {gameping}) <$>
        (parseGameServer sockaddr =<< isProper =<< poll)
    where
    mkS = handle err . bracket (socket AF_INET Datagram defaultProtocol) sClose
    err (_ :: IOError) = return Nothing
    isProper = stripPrefix "\xFF\xFF\xFF\xFFstatusResponse"

ioMaybe :: IO a -> IO (Maybe a)
ioMaybe f = catch (Just <$> f) (\(_ :: IOError) -> return Nothing)

ioForce :: MVar m -> IO a -> IO (Maybe a)
ioForce m f = catch (Just <$> f) $ \(_ :: IOError) -> do
    b <- isEmptyMVar m
    if b then ioForce m f else return Nothing