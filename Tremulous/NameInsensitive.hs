module Tremulous.NameInsensitive (
	TI(..), mk, mkColor, mkAlphaNum, mkColorAlpha, unpackorig
) where
import Data.ByteString.Char8 as B
import Data.Char
import Control.DeepSeq
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

mkColorAlpha :: ByteString -> TI
mkColorAlpha bs = TI bs (B.map toLower $ B.filter isPrint $ removeColors bs)

removeColors :: ByteString -> ByteString
removeColors = B.pack . rc . B.unpack

rc :: String -> String
rc ('^' : x : xs) | isAlphaNum x	= rc xs
rc (x : xs)				= x : rc xs
rc [] 					= []


