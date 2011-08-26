module Network.Tremulous.Protocol (
	  module Network.Tremulous.NameInsensitive
	, Delay(..), Team(..),  GameServer(..), Player(..), MasterServer(..), PollResult(..)
	, defaultDelay, parseGameServer, proto2string, string2proto, parseMasterServer
) where
import Prelude as P hiding (Maybe(..), maybe)
import Control.Applicative hiding (many)
import Control.Monad.State.Strict

import Data.Attoparsec.Char8 hiding (option)
import Data.Attoparsec (anyWord8)
import Data.ByteString.Char8 as B
import Network.Tremulous.StrictMaybe
import Data.String
import Data.Bits
import Data.Word
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
	, gameping
	, protocol	:: !Int
	, hostname	:: !TI
	, gamemod
	, version
	, mapname	:: !(Maybe TI)
	, slots		:: !Int
	, privslots	:: !(Maybe Int)
	, protected
	, unlagged	:: !Bool
	, timelimit
	, suddendeath	:: !(Maybe Int)

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

parsePlayers :: (Maybe ByteString) -> [ByteString] -> Maybe [Player]
parsePlayers Nothing  xs = mapM (parsePlayer Unknown) xs
parsePlayers (Just p) xs = zipWithM parsePlayer (parseP p ++ repeat Unknown) xs

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
	timelimit	<- optionWith maybeInt		"timelimit"
	hostname	<- mkColor <$> require		"sv_hostname"
	protocol	<- requireWith maybeInt		"protocol"
	mapname		<- optionWith (Just . mk)	"mapname"
	version		<- optionWith (Just . mk)	"version"
	gamemod		<- optionWith mkMod		"gamename"
	p		<- option		"P"
	players		<- lift $ parsePlayers p rawplayers
	protected	<- maybe False (/="0") <$> option "g_needpass"
	privslots	<- optionWith maybeInt "sv_privateClients"
	slots		<- maybe id subtract privslots <$> requireWith maybeInt "sv_maxclients"
	suddendeath	<- optionWith maybeInt "g_suddenDeathTime"
	unlagged	<- maybe False (/="0") <$> option "g_unlagged"

	return GameServer { gameping = -1, nplayers = P.length players, .. }
	where
	mkMod "base"	= Nothing
	mkMod a		= Just (mk a)


parseMasterServer :: ByteString -> [SockAddr]
parseMasterServer = fromMaybe [] . parseMaybe (many addr)
	where
	wg = anyWord8
	f :: (Integral a, Integral b) => a -> b
	f = fromIntegral
	addr = do
		char '\\'
		i3 <- wg
		i2 <- wg
		i1 <- wg
		i0 <- wg
		p1 <- wg
		p0 <- wg
		let ip   = (f i3 .<<. 24) .|. (f i2 .<<. 16) .|. (f i1 .<<. 8) .|. f i0 :: Word32
		    port = (f p1 .<<. 8) .|. f p0 :: Word16
		if port == 0 || ip == 0
			then addr
			else return $ SockAddrInet (PortNum (htons port)) (htonl ip)
(.<<.) :: Bits a => a -> Int -> a
(.<<.) = shiftL
-- /// Attoparsec utils ////////////////////////////////////////////////////////////////////////////

quoted :: Parser ByteString
quoted = char '"' *> takeTill (=='"') <* char '"'

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe f xs = case parseOnly f xs of
	Right a	-> Just a
	Left _	-> Nothing
