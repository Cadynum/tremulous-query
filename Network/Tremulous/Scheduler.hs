module Network.Tremulous.Scheduler(
	  Event, Scheduler
	, newScheduler, startScheduler, addScheduled, addScheduledBatch
	, addScheduledInstant, deleteScheduled
) where
import Prelude hiding (drop)
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Typeable
import Data.Sequence
import Data.Foldable
import Network.Tremulous.MicroTime

data Interrupt = Interrupt
	deriving (Typeable, Show)
	
instance Exception Interrupt

data (Eq id, Ord id) => Scheduler id a = Scheduler
	{ sync		:: !(MVar ())
	, started	:: !(MVar ())
	, queue		:: !(MVar (Seq (Event id a)))
	}

type Event id a = (MicroTime, id, a)

-- ugly ugly ugly!
threadBlock :: IO ()
threadBlock = forever $ threadDelay maxBound

pureModifyMVar ::MVar a -> (a -> a) -> IO ()
pureModifyMVar m f = do
	x <- takeMVar m
	putMVar m (f x)

newScheduler :: (Eq a, Ord a) => Int -> (Scheduler a b -> a -> b -> IO ()) -> Maybe (IO ()) -> IO (Scheduler a b)
newScheduler throughput func finalizer = do 
	queue		<- newMVar empty
	sync		<- newEmptyMVar
	started		<- newEmptyMVar
	let sched	=  Scheduler{..}
	uninterruptibleMask $ \a -> forkIO $ runner sched a
	return sched
	
	where
	runner sched@Scheduler{..} restore = do
		takeMVar started
		tid <- myThreadId
		let loop = do
			q <- takeMVar queue
			case viewl q of
				EmptyL -> case finalizer of
					Nothing -> do
						putMVar queue q
						ignoreException $ restore threadBlock
						loop
					Just a -> a
					
				(0, idn, storage) :< qs -> do
					putMVar queue qs
					func sched idn storage
					when (throughput > 0)
						(threadDelay throughput)
					loop
				
				(time, idn, storage) :< qs -> do
					now <- getMicroTime
					let wait = fromIntegral (time - now)
					if wait <= 0 then do
						putMVar queue qs
						func sched idn storage
						when (throughput > 0)
							(threadDelay throughput)
					else do
						putMVar queue q
						tryTakeMVar sync
						syncid <- forkIOUnmasked $ takeMVar sync >> throwTo tid Interrupt
						waited <- falseOnException $ restore $ threadDelay wait
						when waited $ do
							killThread syncid
							pureModifyMVar queue $ deleteID idn
							func sched idn storage
							when (throughput > 0)
								(threadDelay throughput)
					loop
			in loop

signal :: MVar () -> IO ()
signal a = tryPutMVar a () >> return ()

startScheduler :: (Ord id, Eq id, Show id) => Scheduler id a -> IO ()
startScheduler Scheduler{..} = putMVar started ()

addScheduled :: (Ord id, Eq id, Show id) => Scheduler id a -> Event id a -> IO ()
addScheduled Scheduler{..} event = do
	pureModifyMVar queue $ insertTimed event
	signal sync
	
addScheduledBatch :: (Ord id, Eq id, Foldable f) => Scheduler id a -> f (Event id a) -> IO ()
addScheduledBatch Scheduler{..} events = do
	pureModifyMVar queue $ \q -> foldl' (flip insertTimed) q events
	signal sync

addScheduledInstant :: (Ord id, Eq id, Foldable f) => Scheduler id a -> f (id, a) -> IO ()
addScheduledInstant Scheduler{..} events = do
	pureModifyMVar queue $ \q -> foldl' (\acc (a, b) -> (0, a, b) <| acc) q events
	signal sync

deleteScheduled :: (Ord id, Eq id) => Scheduler id a -> id -> IO ()
deleteScheduled Scheduler{..} ident = do
	pureModifyMVar queue $ deleteID ident
	signal sync

ignoreException :: IO () -> IO ()
ignoreException = handle (\Interrupt -> return ())

falseOnException :: IO a -> IO Bool
falseOnException f = handle (\Interrupt -> return False) (f >> return True)


insertTimed :: (Ord id, Eq id) => Event id a -> Seq (Event id a) -> Seq (Event id a)
insertTimed x@(a,_,_) q = (s1 |> x) >< s2 where
	(s1, s2) = spanl (\(b,_,_) -> a >= b) q


deleteID :: (Ord id, Eq id) => id -> Seq (Event id a) -> Seq (Event id a)
deleteID ident q = s1 >< s2' where
	(s1, s2) = spanl (\(_,a,_) -> a /= ident) q
	s2' = drop 1 s2 
