{-# LANGUAGE CPP #-}
-- This module is based on code from the "time" package, and is licensed BSD3.
module Network.Tremulous.MicroTime (MicroTime, getMicroTime) where

#ifdef mingw32_HOST_OS
import Data.Word
import System.Win32.Time
#else
import Foreign
import Foreign.C
#endif

type MicroTime = Word64
getMicroTime :: IO MicroTime

#ifdef mingw32_HOST_OS
getMicroTime = do
	FILETIME ft <- System.Win32.Time.getSystemTimeAsFileTime
	return ((ft - win32_epoch_adjust) `quot` 10)

win32_epoch_adjust :: Word64
win32_epoch_adjust = 116444736000000000

#else
data CTimeval = MkCTimeval !CLong !CLong

instance Storable CTimeval where
	sizeOf _	= sizeOf (undefined :: CLong) * 2
	alignment _	= alignment (undefined :: CLong)
	peek p = do
		s   <- peekElemOff (castPtr p) 0
		mus <- peekElemOff (castPtr p) 1
		return (MkCTimeval s mus)
	poke p (MkCTimeval s mus) = do
		pokeElemOff (castPtr p) 0 s
		pokeElemOff (castPtr p) 1 mus

foreign import ccall unsafe "time.h gettimeofday" gettimeofday :: Ptr CTimeval -> Ptr () -> IO CInt

-- | Get the current POSIX time from the system clock.
getMicroTime = with (MkCTimeval 0 0) $ \ptval -> do
	result <- gettimeofday ptval nullPtr
	if result == 0
		then f `fmap` peek ptval
		else fail ("error in gettimeofday: " ++ show result)
	where f (MkCTimeval a b) = fromIntegral a * 1000000 + fromIntegral b
#endif
