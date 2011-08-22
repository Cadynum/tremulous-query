module Network.Tremulous.NameInsensitive (
	TI(..), mk, mkColor, mkAlphaNum
) where
import Prelude hiding (length, map, filter)
import Data.ByteString.Char8
import Data.Char
import Data.Ord
import Network.Tremulous.ByteStringUtils

data TI = TI {
	  original	:: !ByteString
	, cleanedCase	:: !ByteString
	}

instance Eq TI where
	a == b = cleanedCase a == cleanedCase b

instance Ord TI where
	compare = comparing cleanedCase

instance Show TI where
	show = show . original

mk :: ByteString -> TI
mk xs = TI bs (map toLower bs)
	where bs = clean xs

mkColor :: ByteString -> TI
mkColor xs = TI bs (removeColors bs)
	where bs = clean xs

mkAlphaNum :: ByteString -> TI
mkAlphaNum xs = TI bs (map toLower $ filter (\x -> isAlphaNum x || isSpace x) bs)
	where bs = clean xs

clean :: ByteString -> ByteString
clean = stripw . filter (\c -> c >= '\x20' && c <= '\x7E')

removeColors :: ByteString -> ByteString
removeColors bss = rebuildC (length bss) f bss where
	f '^' xs | Just (x2, xs2) <- uncons xs
		 , isAlphaNum x2
		 , Just (x3, xs3) <- uncons xs2
		 = f x3 xs3
	f x xs = (toLower x, xs)


