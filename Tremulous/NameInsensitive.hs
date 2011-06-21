module Tremulous.NameInsensitive (
	TI(..), mk, mkColor, mkAlphaNum, mkColorAlpha, unpackorig
) where
import Data.ByteString.Char8 as B
import Data.Char
import Control.DeepSeq
import Data.Function (on)

data TI = TI { 
	  original :: !ByteString
	, cleanedCase :: !ByteString
	}

-- Already strict
instance NFData TI

instance Eq TI where
	a == b = cleanedCase a == cleanedCase b
	
instance Ord TI where
	compare = compare `on` cleanedCase
	
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

mkColorAlpha :: ByteString -> TI
mkColorAlpha bs = TI bs (B.map toLower $ B.filter isPrint $ removeColors bs)

removeColors :: ByteString -> ByteString
removeColors = pack . Prelude.foldr f [] . unpack where
	f '^' (x:xs) | x /= '^'	= xs
	f x xs			= x : xs
