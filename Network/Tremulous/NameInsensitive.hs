module Network.Tremulous.NameInsensitive (
	TI(..), mk, mkColor, mkAlphaNum, unpackorig
) where
import Control.DeepSeq
import Data.ByteString.Char8 as B
import Data.Char
import Data.Ord

data TI = TI { 
	  original :: !ByteString
	, cleanedCase :: !ByteString
	}

-- Already strict
instance NFData TI

instance Eq TI where
	a == b = cleanedCase a == cleanedCase b
	
instance Ord TI where
	compare = comparing cleanedCase
	
instance Show TI where
	show = show . original

unpackorig :: TI -> String
unpackorig = B.unpack . original
	
mk :: ByteString -> TI
mk bs = TI bs (B.map toLower bs)

mkColor :: ByteString -> TI
mkColor bs = TI bs (B.map toLower $ removeColors bs)

mkAlphaNum :: ByteString -> TI
mkAlphaNum bs = TI bs (B.map toLower $ B.filter isAlphaNum bs)

removeColors :: ByteString -> ByteString
removeColors = B.unfoldr f where
	f bs	| Just ('^', xs) <- t
		, Just (x, xs2)  <- B.uncons xs
		, isAlphaNum x		= B.uncons xs2
		| otherwise		= t
		where t = B.uncons bs


