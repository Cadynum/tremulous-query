module Network.Tremulous.Scheduler(
      Event(..), Scheduler
    , newScheduler, addScheduled, addScheduledBatch
    , addScheduledInstant, deleteScheduled
    , putMVar', pureModifyMVar
) where
import Prelude hiding (Maybe(..))
import Network.Tremulous.StrictMaybe
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Typeable
import Data.Foldable
import Network.Tremulous.MicroTime

data Interrupt = Interrupt
    deriving (Typeable, Show)

instance Exception Interrupt

data Scheduler id a = Ord id => Scheduler
    { sync      :: !(MVar ())
    , queue     :: !(MVar [Event id a])
    }

data Event id a = Ord id => E
    { time      :: !MicroTime
    , idn       :: !id
    , storage   :: !a
    }

newScheduler :: Ord a => Int -> (Scheduler a b -> a -> b -> IO ())
    -> Maybe (IO ()) -> IO (Scheduler a b)
newScheduler throughput func finalizer = do
    queue       <- newMVar []
    sync        <- newEmptyMVar
    let sched   =  Scheduler{..}

    uninterruptibleMask $ \restore -> forkIO $ do
        takeMVar sync
        runner sched restore =<< myThreadId

    return sched
    where
    runner sched@Scheduler{..} restore tid = loop where
        loop = do
            q <- takeMVar queue
            case q of
                [] -> do
                    putMVar' queue q
                    fromMaybe (takeMVar sync >> loop) finalizer

                E{time=0, ..} : qs -> do
                    putMVar' queue qs
                    func sched idn storage
                    limiter
                    loop

                E{..} : qs -> do
                    now <- getMicroTime
                    if now >= time then do
                        putMVar' queue qs
                        func sched idn storage
                        limiter
                    else do
                        putMVar' queue q
                        tryTakeMVar sync
                        syncid <- forkIO $ restore $ do
                            takeMVar sync
                            throwTo tid Interrupt
                        let wait = fromIntegral (time - now)
                        waited <- falseOnInterrupt $ restore $ threadDelay wait
                        when waited $ do
                            killThread syncid
                            pureModifyMVar queue $ deleteID idn
                            func sched idn storage
                            limiter
                    loop
        limiter | throughput > 0    = threadDelay throughput
                | otherwise         = return ()



signal :: MVar () -> IO ()
signal a = void $ tryPutMVar a ()

addScheduled :: Scheduler id a -> Event id a -> IO ()
addScheduled Scheduler{..} event = do
    pureModifyMVar queue $ insertTimed event
    signal sync

addScheduledBatch :: Foldable f => Scheduler id a -> f (Event id a) -> IO ()
addScheduledBatch Scheduler{..} events = do
    pureModifyMVar queue $ \q -> foldl' (flip insertTimed) q events
    signal sync

addScheduledInstant :: Scheduler id a -> [(id, a)] -> IO ()
addScheduledInstant Scheduler{..} events = do
    pureModifyMVar queue $ \q -> map (uncurry (E 0)) events ++ q
    signal sync

deleteScheduled :: Scheduler id a -> id -> IO ()
deleteScheduled Scheduler{..} ident = do
    pureModifyMVar queue $ deleteID ident
    signal sync


falseOnInterrupt :: IO a -> IO Bool
falseOnInterrupt f = handle (\Interrupt -> return False) (f >> return True)

insertTimed :: Event id a -> [Event id a] -> [Event id a]
insertTimed e []        = [e]
insertTimed e (x:xs)
    | time e >= time x  = x : insertTimed e xs
    | otherwise         = e : x : xs


deleteID :: Ord id => id -> [Event id a] -> [Event id a]
deleteID _     []       = []
deleteID match (x:xs)
    | idn x == match    = xs
    | otherwise         = x : deleteID match xs


putMVar' :: MVar a -> a -> IO ()
putMVar' m !a = putMVar m a

pureModifyMVar :: MVar a -> (a -> a) -> IO ()
pureModifyMVar m f = putMVar' m . f =<< takeMVar m

