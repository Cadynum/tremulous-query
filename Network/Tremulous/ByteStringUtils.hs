module Network.Tremulous.ByteStringUtils where
import Prelude as P
import Control.Applicative
import Control.DeepSeq
import Data.ByteString.Char8 as B
import Data.Char

instance NFData ByteString 

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix p xs
	| p `isPrefixOf` xs	= Just $ B.drop (B.length p) xs
	| otherwise		= Nothing	

maybeInt :: ByteString -> Maybe Int
maybeInt x = fst <$> readInt x

splitlines :: ByteString -> [ByteString]
splitlines = splitfilter '\n'

stripw :: ByteString -> ByteString
stripw = B.dropWhile isSpace

splitfilter :: Char -> ByteString -> [ByteString]
splitfilter f = P.filter (not . B.null) . split f