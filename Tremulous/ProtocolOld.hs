module Tremulous.Protocol(
	module Helpers, Team(..), CVar, GameServer(..), PlayerInfo(..), MasterServer(..)
	, cycleoutIP, pollFormat, proto2string, string2proto
)where
import Control.DeepSeq
import Control.Applicative
import Data.Bits
import Data.Maybe
import Text.Read hiding (look)
import Control.Monad
import Network.Socket
import Helpers


data Team = Spectators | Aliens | Humans | Unknown deriving (Eq, Show)

readTeam :: Char -> Team
readTeam x = case x of
	'0'	-> Spectators
	'1'	-> Aliens
	'2'	-> Humans
	_	-> Unknown

type CVar = (Nocase, String)

data GameServer = GameServer {
	  address	:: !SockAddr
	, gameping	:: !Int
	, cvars		:: ![CVar]
	, gameproto	:: !Int
	, gamemod	:: !(Maybe Nocase)
	, hostname	:: !String
	, hostnamec
	, mapname	:: !Nocase
	, slots
	, privslots	:: !Int
	, protected	:: Bool
	, nplayers	:: !Int
	, players	:: ![PlayerInfo]
	}

data PlayerInfo	= PlayerInfo {
	  team	:: !Team
	, kills
	, ping	:: !Int
	, name	:: !String
	}

data MasterServer = MasterServer {
	  mident	:: !String
	, protocol	:: !Int
	, masterHost	:: !SockAddr
	} deriving Eq

instance (Read PlayerInfo) where
	readPrec = do
		Int kills	<- lexP
		Int ping	<- lexP
		String name	<- lexP
		return $ PlayerInfo Unknown (fromInteger kills) (fromInteger ping) name

instance NFData MasterServer where
	rnf (MasterServer a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData Team

instance NFData GameServer where
	rnf (GameServer a b c d e f g h i j k l m) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
		`seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j 
		`seq` rnf k `seq` rnf l `seq` rnf m

instance NFData PlayerInfo where
	rnf (PlayerInfo a b c d)  = rnf a `seq` rnf b `seq` rnf c `seq` rnf d


deriving instance Ord SockAddr

instance NFData SockAddr where
	rnf (SockAddrInet (PortNum p) h) 	= rnf p `seq` rnf h
	rnf (SockAddrInet6 (PortNum p) f h s)	= rnf p `seq` rnf f `seq` rnf h `seq` rnf s
	rnf (SockAddrUnix s)			= rnf s

proto2string :: Int -> String
proto2string x = case x of
	69 -> "1.1"
	70 -> "gpp"
	_  -> "?"

string2proto :: String -> Maybe Int
string2proto x = case x of
	"vanilla"	-> Just 69
	"1.1"		-> Just 69
	"gpp"		-> Just 70
	"1.2"		-> Just 70
	_		-> Nothing
	
-- Hacky function to get the IP numbers from the master
cycleoutIP :: String -> [SockAddr]
cycleoutIP ('\\' :'E':'O':'T':'\0':'\0':'\0':[]) = []
cycleoutIP ('\\' : i0:i1:i2:i3 : p0:p1 : xs) = SockAddrInet port ip : cycleoutIP xs
	where	ip	= fromIntegral $ (ord i3 .<<. 24) .|. (ord i2 .<<. 16) .|. (ord i1 .<<. 8) .|. ord i0
		port	= fromIntegral $ (ord p0 .<<. 8) .|. ord p1
		(.<<.)	= shiftL
cycleoutIP _ = []

pollFormat :: SockAddr -> String -> Maybe GameServer
pollFormat address line = case splitlines line of
	(cvars_:players_) -> do
		gameproto	<- look "protocol" >>= mread
		hostname	<- look "sv_hostname"
		let hostnamec	= Nocase $ stripw $ filter isPrint hostname
		let protected	= fromMaybe False $ (/="0") <$> look "g_needpass"
		let gamemod	= case look "gamename" of
					Nothing     -> Nothing
					Just "base" -> Nothing
					Just x      -> Just $ Nocase x
		let privslots	= fromMaybe 0 $ (look "sv_privateclients" >>= mread)
		let mapname	= Nocase $ filter isPrint $ fromMaybe "" $ look "mapname"
		slots		<- subtract privslots <$> (mread =<< look "sv_maxclients")
		let nplayers	= length players
		return GameServer {gameping = -1, ..}
		where
		cvars	= cvarstuple . split (=='\\') $ cvars_
		players	= case lookup (Nocase "P") cvars of
				Nothing -> mapMaybe mread players_
				Just a	-> playerList players_ a	
		look x	= lookup (Nocase x) cvars
		
	_ -> Nothing

playerList :: [String] -> [Char] -> [PlayerInfo]
playerList []		_	= []
playerList (p:ps)	[]	= mread p /: playerList ps []
playerList ps		('-':ls)= playerList ps ls
playerList (p:ps)	(l:ls)	= (\x -> x {team = readTeam l}) `liftM` mread p /: playerList ps ls

cvarstuple :: [String] -> [CVar]
cvarstuple (c:v:ss)	= (Nocase c, v) : cvarstuple ss
cvarstuple _		= []

(/:) :: Maybe t -> [t] -> [t]
(Just x) /: xs = x:xs
Nothing /: xs = xs
