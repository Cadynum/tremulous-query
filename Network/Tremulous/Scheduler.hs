module Network.Tremulous.Scheduler(
	  Event, Scheduler
	, newScheduler, addScheduled, addScheduledBatch
	, addScheduledInstant, deleteScheduled
) where
import Prelude
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Typeable
import Data.Foldable
import Network.Tremulous.MicroTime

data Interrupt = Interrupt
	deriving (Typeable, Show)
	
instance Exception Interrupt

data (Eq id, Ord id) => Scheduler id a = Scheduler
	{ sync		:: !(MVar ())
	, queue		:: !(MVar [Event id a])
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
	queue		<- newMVar []
	sync		<- newEmptyMVar
	let sched	=  Scheduler{..}
	uninterruptibleMask $ \a -> forkIO $ runner sched a
	return sched
	
	where
	runner sched@Scheduler{..} restore = do
		takeMVar sync
		tid <- myThreadId
		let loop = do
			q <- takeMVar queue
			case q of
				[] -> putMVar queue q >> case finalizer of
					Nothing -> do
						ignoreInterrupt $ restore threadBlock
						loop
					Just a -> a
					
				(0, idn, storage) : qs -> do
					putMVar queue qs
					func sched idn storage
					when (throughput > 0)
						(threadDelay throughput)
					loop
				
				(time, idn, storage) : qs -> do
					now <- getMicroTime
					if now >= time then do
						putMVar queue qs
						func sched idn storage
						when (throughput > 0)
							(threadDelay throughput)
					else do
						putMVar queue q
						tryTakeMVar sync
						syncid <- forkIOUnmasked $ takeMVar sync >> throwTo tid Interrupt
						let wait = fromIntegral (time - now)
						waited <- falseOnInterrupt $ restore $ threadDelay wait
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

addScheduled :: (Ord id, Eq id) => Scheduler id a -> Event id a -> IO ()
addScheduled Scheduler{..} event = do
	pureModifyMVar queue $ insertTimed event
	signal sync
	
addScheduledBatch :: (Ord id, Eq id, Foldable f) => Scheduler id a -> f (Event id a) -> IO ()
addScheduledBatch Scheduler{..} events = do
	pureModifyMVar queue $ \q -> foldl' (flip insertTimed) q events
	signal sync

addScheduledInstant :: (Ord id, Eq id) => Scheduler id a -> [(id, a)] -> IO ()
addScheduledInstant Scheduler{..} events = do
	pureModifyMVar queue $ \q -> (map (\(a,b) -> (0,a,b)) events) ++ q
	signal sync

deleteScheduled :: (Ord id, Eq id) => Scheduler id a -> id -> IO ()
deleteScheduled Scheduler{..} ident = do
	pureModifyMVar queue $ deleteID ident
	signal sync

ignoreInterrupt :: IO () -> IO ()
ignoreInterrupt = handle (\Interrupt -> return ())

falseOnInterrupt :: IO a -> IO Bool
falseOnInterrupt f = handle (\Interrupt -> return False) (f >> return True)


insertTimed :: (Ord id, Eq id) => Event id a -> [Event id a] -> [Event id a]
insertTimed x@(a, _, _) q = s1 ++ x : s2 where
	(s1, s2) = span (\(b,_,_) -> a >= b) q

deleteID :: (Ord id, Eq id) => id -> [Event id a] -> [Event id a]
deleteID idn xss = case xss of
	x@(_,a,_):xs	| a == idn	-> xs
			| otherwise	-> x : deleteID idn xs
	[]				-> []
