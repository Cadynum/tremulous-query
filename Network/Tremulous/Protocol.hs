module Network.Tremulous.Protocol (
      module Network.Tremulous.NameInsensitive
    , Delay(..)
    , Team(..)
    , GameServer(..)
    , Player(..)
    , MasterServer(..)
    , PollResult(..)
    , defaultDelay
    , parseGameServer
    , parseMasterServer
) where
import Prelude as P hiding (Maybe(..), maybe)
import Control.Applicative as A
import Control.Monad.State.Strict

import Data.Attoparsec.Char8 hiding (option)
import Data.Attoparsec (anyWord8)
import Data.ByteString.Char8 as B
import Network.Tremulous.StrictMaybe
import Data.Bits
import Data.Word
import Network.Socket
import Network.Tremulous.ByteStringUtils as B
import Network.Tremulous.SocketExtensions
import Network.Tremulous.NameInsensitive
import Network.Tremulous.TupleReader

data Delay = Delay
    { packetTimeout
    , packetDuplication
    , throughputDelay   :: !Int
    }

data MasterServer = MasterServer
    { masterAddress     :: !SockAddr
    , masterProtocol    :: !Int
    } deriving Eq

data GameServer = GameServer
    { address       :: !SockAddr
    , gameping
    , protocol      :: !Int
    , hostname      :: !TI
    , gamemod
    , version
    , mapname       :: !(Maybe TI)
    , slots         :: !Int
    , privslots     :: !Int
    , protected
    , unlagged      :: !Bool
    , timelimit
    , suddendeath   :: !(Maybe Int)
    -- nplayers excluding bots. A player is a bot if ping == 0
    , nplayers      :: !Int
    , players       :: ![Player]
    }

data Team = Spectators | Aliens | Humans | Unknown deriving (Eq, Show)

data Player = Player
    { team      :: !Team
    , kills
    , ping      :: !Int
    , name      :: !TI
    }

data PollResult = PollResult
    { polled                :: ![GameServer]
    , serversResponded
    , serversRequested      :: !Int
    }

defaultDelay :: Delay
defaultDelay = Delay
    { packetTimeout         = 400 * 1000
    , packetDuplication     = 2
    , throughputDelay       = 1 * 1000
    }

parsePlayer :: Team -> ByteString -> Maybe Player
parsePlayer team = parseMaybe $ do
    kills <- signed decimal
    skipSpace
    ping  <- signed decimal
    skipSpace
    name  <- mkColor <$> quoted
    return Player {..}

-- cvar P
parseP :: ByteString -> [Team]
parseP = foldr' f []
    where
    f '-' xs = xs
    f  a  xs = readTeam a : xs

    readTeam x = case x of
        '0' -> Spectators
        '1' -> Aliens
        '2' -> Humans
        _   -> Unknown

parsePlayers :: Maybe ByteString -> [ByteString] -> Maybe [Player]
parsePlayers Nothing  xs = mapM (parsePlayer Unknown) xs
parsePlayers (Just p) xs = zipWithM parsePlayer (parseP p ++ repeat Unknown) xs

parseCVars :: ByteString -> [(ByteString, ByteString)]
parseCVars xs = f (splitfilter '\\' xs) where
    f (k:v:cs)  = (k, v) : f cs
    f _         = []

parseGameServer :: SockAddr -> ByteString -> Maybe GameServer
parseGameServer address xs = case splitlines xs of
    (cvars:players) -> mkGameServer address players (parseCVars cvars)
    _               -> Nothing

mkGameServer :: SockAddr
             -> [ByteString]
             -> [(ByteString, ByteString)]
             -> Maybe GameServer
mkGameServer address rawplayers = tupleReader $ do
    timelimit       <- optionWith maybeInt      "timelimit"
    hostname        <- mkColor <$> require      "sv_hostname"
    protocol        <- requireWith maybeInt     "protocol"
    mapname         <- optionWith (Just . mk)   "mapname"
    version         <- optionWith (Just . mk)   "version"
    gamemod         <- optionWith mkMod         "gamename"
    p               <- option                   "P"
    players         <- lift $ parsePlayers p rawplayers
    protected       <- mkBool <$> option        "g_needpass"
    privslots       <- fromMaybe 0 <$> optionWith maybeInt
                                                "sv_privateClients"
    slots           <- subtract privslots <$> requireWith maybeInt
                                                "sv_maxclients"
    suddendeath     <- optionWith maybeInt      "g_suddenDeathTime"
    unlagged        <- mkBool <$> option        "g_unlagged"
    return GameServer
        { gameping  = -1
        , nplayers  = P.length $ P.filter (\x -> ping x > 0) players
        , ..
        }
    where
    mkMod "base"    = Nothing
    mkMod a         = Just (mk a)
    mkBool          = maybe False (/="0")


parseMasterServer :: ByteString -> [SockAddr]
parseMasterServer = fromMaybe [] . parseMaybe (A.many addr)
    where
    addr = do
        char '\\'
        ip <- parseUInt32N
        port <- parseUInt16N
        if port == 0 || ip == 0
            then addr
            else return $ SockAddrInet (PortNum (htons port)) (htonl ip)

parseUInt32N :: Parser Word32
parseUInt32N = do
    b3 <- wg
    b2 <- wg
    b1 <- wg
    b0 <- wg
    return $ (b3 << 24) .|. (b2 << 16) .|. (b1 << 8) .|. b0
    where
    wg = fromIntegral <$> anyWord8
    (<<) = unsafeShiftL

parseUInt16N :: Parser Word16
parseUInt16N = do
    b1 <- wg
    b0 <- wg
    return $ (b1 << 8) .|. b0
    where
    wg = fromIntegral <$> anyWord8
    (<<) = unsafeShiftL


-- /// Attoparsec utils ////////////////////////////////////////////////////////

quoted :: Parser ByteString
quoted = char '"' *> takeTill (=='"') <* char '"'

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe f xs = case parseOnly f xs of
    Right a -> Just a
    Left _  -> Nothing
