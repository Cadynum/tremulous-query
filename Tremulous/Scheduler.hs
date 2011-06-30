module Tremulous.Scheduler where
import Prelude hiding (drop)
import System.Time
import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Exception
import Data.Typeable
import Data.Sequence
import Data.Foldable
import Control.Monad

data Interrupt = Interrupt
	deriving (Typeable, Show)
	
instance Exception Interrupt

data (Eq id, Ord id) => Scheduler id a = Scheduler
	{ pid		:: !(Maybe ThreadId)
	, queue		:: !(MVar (Seq (Integer, id, a)))
	, finished	:: !(MVar ())
	}

type Event id a = (Integer, id, a)

-- ugly ugly ugly!
threadBlock :: IO ()
threadBlock = forever $ threadDelay maxBound

pureModifyMVar ::MVar a -> (a -> a) -> IO ()
pureModifyMVar m f = do
	x <- takeMVar m
	putMVar m (f x)

newScheduler :: (Eq a, Ord a) => Int -> (Scheduler a b -> a -> b -> IO c) -> IO (Scheduler a b)
newScheduler throughput func = do
	queue		<- newMVar empty
	finished	<- newEmptyMVar
	pid		<- mask $ \a -> forkIO $ runner queue finished a
	
	return Scheduler{pid=Just pid, ..}
	where
	runner queue finished restore = loop
		where
		sched = Scheduler{pid=Nothing, ..}
		loop = do
		q <- readMVar queue
		case viewl q of
			EmptyL -> putMVar finished ()

			(time, idn, storage) :< q' -> do
				now <- getMicroTime
				let wait = max throughput (fromInteger (time - now))
				waited <- liftM (wait <= 0 || ) (falseOnException $ restore (threadDelay wait))
				when waited $ do
					swapMVar queue q'
					func sched idn storage
					return ()
				loop

addScheduled :: (Ord id, Eq id) => Scheduler id a -> Event id a -> IO ()
addScheduled Scheduler{..} event = do
	pureModifyMVar queue $ insertTimed event
	whenJust pid $ \a -> throwTo a Interrupt
	
addScheduledBatch :: (Ord id, Eq id, Foldable f) => Scheduler id a -> f (Event id a) -> IO ()
addScheduledBatch Scheduler{..} events = do
	pureModifyMVar queue $ \q -> foldl' (flip insertTimed) q events
	whenJust pid $ \a -> throwTo a Interrupt

addScheduledInstant :: (Ord id, Eq id, Foldable f) => Scheduler id a -> f (id, a) -> IO ()
addScheduledInstant Scheduler{..} events = do
	pureModifyMVar queue $ \q -> foldl' (\acc (a, b) -> (-1, a, b) <| acc) q events
	whenJust pid $ \a -> throwTo a Interrupt


deleteScheduled :: (Ord id, Eq id) => Scheduler id a -> id -> IO ()
deleteScheduled Scheduler{..} ident = do
	pureModifyMVar queue $ deleteID ident
	whenJust pid $ \a -> throwTo a Interrupt

waitForSchedulerFinish :: (Ord id, Eq id) => Scheduler id a -> IO ()
waitForSchedulerFinish Scheduler{..} = takeMVar finished
				
getMicroTime :: IO Integer
getMicroTime = let f (TOD s p) = s*1000000 + p `div` 1000000 in f <$> getClockTime

ignoreException :: IO () -> IO ()
ignoreException f = handle (\Interrupt -> return ()) f

falseOnException :: IO a -> IO Bool
falseOnException f = handle (\Interrupt -> return False) (f >> return True)

insertTimed :: (Ord id, Eq id) => (Event id a) -> Seq (Event id a) -> Seq (Event id a)
insertTimed x@(a,_,_) q = (s1 |> x) >< s2 where
	(s1, s2) = spanl (\(b,_,_) -> a >= b) q


deleteID :: (Ord id, Eq id) => id -> Seq (Event id a) -> Seq (Event id a)
deleteID ident q = s1 >< s2' where
	(s1, s2) = spanl (\(_,a,_) -> a /= ident) q
	s2' = drop 1 s2 

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust x f = case x of
	Just a	-> f a
	Nothing	-> return ()