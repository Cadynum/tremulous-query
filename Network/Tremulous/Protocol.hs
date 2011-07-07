module Network.Tremulous.Protocol (
	  module Network.Tremulous.NameInsensitive
	, Delay(..), Team(..),  GameServer(..), Player(..), MasterServer(..), PollResult(..)
	, defaultDelay, parseGameServer, proto2string, string2proto, parseMasterServer
	, B.unpack
) where
import Prelude as P
import Control.Applicative
import Control.DeepSeq
import Control.Monad.State.Strict

import Data.Attoparsec.Char8 as A hiding (option)
import Data.Attoparsec (anyWord8)
import Data.ByteString.Char8 as B
import Data.Maybe
import Data.String
import Data.Char
import Data.Bits
import Data.Word
import Data.Set (Set)

import Network.Socket
import Network.Tremulous.ByteStringUtils as B
import Network.Tremulous.SocketExtensions ()
import Network.Tremulous.NameInsensitive
import Network.Tremulous.TupleReader

data Delay = Delay {
	  packetTimeout
	, packetDuplication
	, throughputDelay	:: !Int
	} deriving (Show, Read)

data MasterServer = MasterServer {
	   masterAddress	:: !SockAddr
	 , masterProtocol	:: !Int
	} deriving Eq
	
data GameServer = GameServer {
	  address	:: !SockAddr
	, gameping	:: !Int
	, protocol	:: !Int
	, gamemod	:: !(Maybe TI)
	, hostname	:: !TI
	, mapname	:: !TI
	, slots
	, privslots	:: !Int
	, protected	:: !Bool
	, timelimit
	, suddendeath	:: !(Maybe Int)
	, unlagged	:: !Bool
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

data PollResult = PollResult {
	  polled		:: ![GameServer]
	, serversResponded
	, serversRequested	:: !Int
	, respondedCache	:: !(Set SockAddr)
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

defaultDelay :: Delay
defaultDelay = Delay {
	  packetTimeout		= 400 * 1000
	, packetDuplication	= 2
	, throughputDelay	= 1 * 1000
	}
	
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

parsePlayers :: ByteString -> [ByteString] -> Maybe [Player]
parsePlayers p = zipWithM parsePlayer (parseP p ++ repeat Unknown)

parseCVars :: ByteString -> [(ByteString, ByteString)]
parseCVars xs = f (splitfilter '\\' xs) where 
	f (k:v:cs)	= (k, v) : f cs
	f _		= []

parseGameServer :: SockAddr -> ByteString -> Maybe GameServer
parseGameServer address xs = case splitlines xs of
	(cvars:players)	-> mkGameServer address players (parseCVars cvars)
	_		-> Nothing

mkGameServer :: SockAddr -> [ByteString] -> [(ByteString, ByteString)] -> Maybe GameServer
mkGameServer address rawplayers = tupleReader $ do
	timelimit	<- option Nothing maybeInt "timelimit"
	hostname	<- mkColorAlpha <$> require "sv_hostname"
	protocol	<- requireWith maybeInt "protocol"
	mapname		<- option (TI "" "") (mk . clean) "mapname"
	gamemod		<- option Nothing mkMod "gamename"
	p		<- option "" id "P"
	players		<- lift $ parsePlayers p rawplayers
	protected	<- option False (/="0") "g_needpass"
	privslots	<- option 0 (fromMaybe 0 . maybeInt) "sv_privateClients"
	slots		<- requireWith maybeInt "sv_maxclients"
	suddendeath	<- option Nothing maybeInt "g_suddenDeathTime"
	unlagged	<- option False (/="0") "g_unlagged"
	
	return GameServer { gameping = -1, nplayers = P.length players, .. }
	where
	mkMod "base"	= Nothing
	mkMod a		= Just (mk a)
	clean = stripw . B.filter isPrint


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
quoted = char '"' *> takeTill (=='"') <* char '"'

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe f xs = case parseOnly f xs of
	Right a	-> Just a
	Left _	-> Nothing
