module Network.Tremulous.Protocol (
	  module Network.Tremulous.NameInsensitive
	, Delay(..), Team(..),  GameServer(..), Player(..), MasterServer(..), PollResult(..)
	, defaultDelay, parseGameServer, proto2string, string2proto, parseMasterServer
) where
import Prelude as P hiding (Maybe(..))
import Control.Applicative hiding (many)
import Control.Monad.State.Strict

import Data.Attoparsec.Char8 hiding (option)
import Data.Attoparsec (anyWord8)
import Data.ByteString.Char8 as B
import Network.Tremulous.StrictMaybe
import Data.String
import Data.Set (Set)
import Network.Socket
import Network.Tremulous.ByteStringUtils as B
import Network.Tremulous.SocketExtensions
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
	hostname	<- mkColor <$> require "sv_hostname"
	protocol	<- requireWith maybeInt "protocol"
	mapname		<- option (TI "" "") (mk) "mapname"
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


parseMasterServer :: ByteString -> [SockAddr]
parseMasterServer = fromMaybe [] . parseMaybe (many addr)
	where
	wg = anyWord8
	addr = do
		char '\\'
		a <- getIPv4 <$> wg <*> wg <*> wg <*> wg <*> wg <*> wg
		case a of
			SockAddrInet (PortNum p) i | p == 0 || i == 0	-> addr
			_						-> return a

-- /// Attoparsec utils ////////////////////////////////////////////////////////////////////////////

quoted :: Parser ByteString
quoted = char '"' *> takeTill (=='"') <* char '"'

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe f xs = case parseOnly f xs of
	Right a	-> Just a
	Left _	-> Nothing
