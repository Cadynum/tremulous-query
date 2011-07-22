module Network.Tremulous.Util (
	serverByAddress
	, elemByAddress
	, search
	, makePlayerList
	, makePlayerNameList
	, stats
	, partitionTeams
	, removeColors
) where
import Data.List hiding (foldl')
import Data.Foldable (foldl')
import Data.Char
import qualified Data.ByteString.Char8 as B

import Network.Socket
import Network.Tremulous.Protocol as T


serverByAddress :: SockAddr -> [GameServer] -> Maybe GameServer
serverByAddress add = find (\x -> add == address x)

elemByAddress :: SockAddr -> [GameServer] -> Bool
elemByAddress add = any (\x -> add == address x)

search :: String -> [GameServer] -> [(TI, GameServer)]
search ""	= makePlayerNameList 
search rawstr	= filter (\(a, _) -> str `B.isInfixOf` cleanedCase a ) . makePlayerNameList 
	where
	str	= B.pack $ map toLower rawstr
	
makePlayerNameList :: [GameServer] -> [(TI, GameServer)]
makePlayerNameList = concatMap $ \x -> map (\a -> (name a, x)) (players x)

makePlayerList :: [GameServer] -> [(Player, GameServer)]
makePlayerList = concatMap $ \x -> map (\a -> (a, x)) (players x)

stats :: [GameServer] -> (Int, Int, Int)
stats polled = (tot, players, bots) where
	tot		= length polled
	(players, bots) = foldl' trv (0, 0) (playerList polled)
	trv (!p, !b) x	= if ping x == 0 then (p, b+1) else (p+1, b)
	playerList	= foldr ((++) . T.players) []

partitionTeams :: [Player] -> ([Player], [Player], [Player], [Player])
partitionTeams = foldr f ([], [], [], []) where
	f x ~(s, a, h, u) = case team x of
		Spectators	-> (x:s, a, h, u)
		Aliens		-> (s, x:a, h, u)
		Humans		-> (s, a, x:h, u)
		Unknown		-> (s, a, h, x:u)

removeColors :: String -> String
removeColors ('^' : x : xs) | isAlphaNum x	= removeColors xs
removeColors (x : xs)				= x : removeColors xs
removeColors [] 				= []
