module Network.Tremulous.ByteStringUtils where
import qualified Prelude as P
import Prelude hiding (length, Maybe(..), null, dropWhile, drop)
import Data.ByteString.Char8
import Data.Char
import Network.Tremulous.StrictMaybe
import Data.ByteString.Internal
import Foreign

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix p xs
	| p `isPrefixOf` xs	= Just $ drop (length p) xs
	| otherwise		= Nothing

maybeInt :: ByteString -> Maybe Int
maybeInt x = case readInt x of
	P.Nothing	-> Nothing
	P.Just (a, _)	-> Just a

splitlines :: ByteString -> [ByteString]
splitlines = splitfilter '\n'

stripw :: ByteString -> ByteString
stripw = dropWhile isSpace

splitfilter :: Char -> ByteString -> [ByteString]
splitfilter f = P.filter (not . null) . split f


rebuild :: Int -> (Word8 -> ByteString -> (Word8, ByteString)) -> ByteString -> ByteString
rebuild i f x0
	| i < 0     = empty
	| otherwise = unsafePerformIO $ createAndTrim i $ \p -> go p x0 0
	where
        go !p !(PS x s l) !n
		| l == 0 || n == i = return n
		| otherwise = do
			w <- withForeignPtr x (`peekByteOff` s)
			let ps'		= PS x (s+1) (l-1)
			let (w', ps'')	= f w ps'
			poke p w'
			go (p `plusPtr` 1) ps'' (n+1)

rebuildC :: Int -> (Char -> ByteString -> (Char, ByteString)) -> ByteString -> ByteString
rebuildC i f x0 = rebuild i f' x0
	where
	f' w ps = let (a, b) = f (w2c w) ps in (c2w a, b)
