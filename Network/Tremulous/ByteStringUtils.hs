module Network.Tremulous.ByteStringUtils where
import qualified Prelude as P
import Prelude ((.), otherwise, not, ($))
import Data.Int
import Data.ByteString.Char8
import Data.Char
import Network.Tremulous.StrictMaybe


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
