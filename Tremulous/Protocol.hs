module Tremulous.Protocol (
	  module Tremulous.NameInsensitive
	, Delay(..), Team(..),  GameServer(..), Player(..), MasterServer(..)
	, parseGameServer, proto2string, string2proto, parseMasterServer
	, B.unpack
) where
import Prelude as P hiding (foldl)
import Network.Socket
import Control.Applicative
import Control.DeepSeq
import Data.Attoparsec.Char8 as A
import Data.Attoparsec (anyWord8)
import Data.ByteString.Char8 as B

import Data.Maybe
import Data.String
import Data.Char as C
import Data.Bits
import Data.Word

import Tremulous.ByteStringUtils as B
import Tremulous.SocketExtensions ()
import Tremulous.NameInsensitive

data Delay = Delay {
	  resendWait
	, resendTimes
	, outBufferDelay	:: !Int
	} deriving (Show, Read)

data MasterServer = MasterServer {
	  protocol	:: !Int
	, masterHost	:: !SockAddr
	} deriving Eq
	
data GameServer = GameServer {
	  address	:: !SockAddr
	, gameping	:: !Int
	, gameproto	:: !Int
	, gamemod	:: !(Maybe TI)
	, hostname	:: !TI
	, mapname	:: !TI
	, slots
	, privslots	:: !Int
	, protected	:: !Bool
	, timelimit
	, suddendeath	:: !(Maybe Int)
	, unlagged	:: !(Maybe Bool)
	, nplayers	:: !Int
	, players	:: ![Player]
	}

	
data Team = Spectators | Aliens | Humans | Unknown deriving (Eq, Show)

data Player = Player {
	  team	:: !Team
	, kills
	, ping	:: !Int
	, name	:: !TI
	}


instance NFData MasterServer where
	rnf (MasterServer a b) = rnf a `seq` rnf b

instance NFData Team

instance NFData GameServer where
	rnf (GameServer a b c d e f g h i j k l m n) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
		`seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j 
		`seq` rnf k `seq` rnf l `seq` rnf m `seq` rnf n

instance NFData Player where
	rnf (Player a b c d)  = rnf a `seq` rnf b `seq` rnf c `seq` rnf d


-- Protocol version
proto2string :: IsString s => Int ->  s
proto2string x = case x of
	69 -> "1.1"
	70 -> "gpp"
	_  -> "?"

string2proto :: (IsString s, Eq s) => s -> Maybe Int
string2proto x = case x of
	"vanilla"	-> Just 69
	"1.1"		-> Just 69
	"gpp"		-> Just 70
	"1.2"		-> Just 70
	_		-> Nothing

parsePlayer :: Team -> ByteString -> Maybe Player
parsePlayer team = parseMaybe $ do
	kills <- signed decimal
	skipSpace
	ping <- signed decimal
	skipSpace
	name <- mkColor <$> quoted
	return Player {..}
	
-- cvar P
parseP :: ByteString -> [Team]
parseP = foldr' f [] 
	where
	f '-' xs = xs
	f  a  xs = readTeam a : xs
	
	readTeam x = case x of
		'0'	-> Spectators
		'1'	-> Aliens
		'2'	-> Humans
		_	-> Unknown

parsePlayers :: ByteString -> [ByteString] -> [Player]
parsePlayers p xs = catMaybes $ P.zipWith parsePlayer (parseP p ++ repeat Unknown) xs

parseCVars :: ByteString -> [(ByteString, ByteString)]
parseCVars xs = f (splitfilter '\\' xs) where 
	f (k:v:cs)	= (k, v) : f cs
	f _		= []

parseGameServer :: SockAddr -> ByteString -> Maybe GameServer
parseGameServer address xs = case splitlines xs of
	(rawcvars:rawplayers) -> do
		let players	=  parsePlayers (fromMaybe "" $ look "P") rawplayers
		    nplayers	=  P.length players
		    
		gameproto	<- look "protocol" >>= maybeInt 
		let gamemod	=  mkMod $ look "gamename"
		
		hostname	<- mkColorAlpha <$> look "sv_hostname"
		
		let mapname	=  mk $ clean $ fromMaybe "" $ look "mapname"
		
		let protected	=  fromMaybe False $ (/="0") <$> look "g_needpass"
		
		let privslots	= fromMaybe 0 (look "sv_privateClients" >>= maybeInt)
		slots		<- subtract privslots <$> (maybeInt =<< look "sv_maxclients")

		let timelimit	= maybeInt =<< look "timelimit"
		let suddendeath	= maybeInt =<< look "g_suddenDeathTime"
		let unlagged	= (/="0") <$> look "g_unlagged"
		
		return GameServer { gameping = -1, ..}
		where
		cvars		= parseCVars rawcvars
		look x		= lookup x cvars
	_ -> Nothing
	where
	mkMod x = case x of
		Nothing     -> Nothing
		Just "base" -> Nothing
		Just a      -> Just (mk a)
		
	clean = stripw . B.filter isPrint

-- Should be replaced
parseMasterServer :: ByteString -> [SockAddr]	
parseMasterServer = fromMaybe [] . parseMaybe attoIP
	where
	attoIP = A.many (char '\\' *> addr)
	(.<<.) :: Bits a => a -> Int -> a
	(.<<.) = shiftL		
	wg :: Integral i => Parser i
	wg = fromIntegral <$> anyWord8	
	addr = do
		-- try $ string "EOT\0\0\0" *> endOfInput
		i0 <- wg
		i1 <- wg
		i2 <- wg
		i3 <- wg
		p0 <- wg
		p1 <- wg
		let ip = (i3 .<<. 24) .|. (i2 .<<. 16) .|. (i1 .<<. 8) .|. i0 :: Word32
		let port = (p0 .<<. 8) .|. p1 :: Word16
		return $ SockAddrInet (fromIntegral port) ip

-- /// Attoparsec utils ////////////////////////////////////////////////////////////////////////////

quoted :: Parser ByteString
quoted = do
	char '"'
	x <- takeTill (=='"')
	char '"'
	return x

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe f xs = case parseOnly f xs of
	Right a	-> Just a
	Left _	-> Nothing
