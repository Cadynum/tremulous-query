module Network.Tremulous.NameInsensitive (
	TI(..), mk, mkColor, unpackorig
) where
import Data.ByteString.Char8 as B
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

unpackorig :: TI -> String
unpackorig = B.unpack . original

mk :: ByteString -> TI
mk xs = TI bs (B.map toLower bs)
	where bs = clean xs

mkColor :: ByteString -> TI
mkColor xs = TI bs (removeColors bs)
	where bs = clean xs

clean :: ByteString -> ByteString
clean = stripw . B.filter (\c -> c > '\x1F' && c < '\x80')

removeColors :: ByteString -> ByteString
removeColors bss = fst (B.unfoldrN (B.length bss) f bss) where
	f bs = case B.uncons bs of
		Nothing -> Nothing
		Just (x, xs)
			| x == '^'
			, Just (x2, xs2) <- B.uncons xs
			, isAlphaNum x2			-> f xs2
			| otherwise			-> Just (toLower x, xs)


