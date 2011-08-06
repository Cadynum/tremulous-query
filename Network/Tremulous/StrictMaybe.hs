{- Drop in replacement for Data.Maybe
-- It was easier to just use this than messing around with seq
-}
module Network.Tremulous.StrictMaybe where
import Prelude hiding(Maybe(..), maybe)

data Maybe a = Nothing | Just !a
	deriving (Eq, Ord, Show, Read)

instance Functor Maybe where
	fmap _ Nothing  = Nothing
	fmap f (Just x) = Just (f x)

instance Monad Maybe where
	Nothing >>= _	= Nothing
	Just a  >>= f	= f a
	return		= Just
	fail _		= Nothing

isJust :: Maybe a -> Bool
isJust Nothing		= False
isJust _		= True

isNothing :: Maybe a -> Bool
isNothing Nothing	= True
isNothing _		= False

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing	= x
fromMaybe _ (Just y)	= y

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing	= x
maybe _ f (Just y)	= f y

whenJust :: Monad m => Maybe t -> (t -> m ())-> m ()
whenJust (Just a) f	= f a
whenJust Nothing  _	= return ()

whileJust :: Monad m => a -> (a -> m (Maybe a)) -> m ()
whileJust x f  = f x >>= \c -> case c of
	Just a	-> whileJust a f
	Nothing	-> return ()
