module Network.Tremulous.TupleReader (
	TupleReader, tupleReader, require, requireWith, option
) where
import Control.Monad.State.Strict
import Network.Tremulous.StrictMaybe
import Prelude (Eq(..), otherwise, ($))

type TupleReader k v a = StateT [(k, v)] Maybe a

tupleReader :: TupleReader k v a -> [(k, v)] -> Maybe a
tupleReader = evalStateT

lookupDelete :: (Eq k) => k -> [(k, v)] -> (Maybe v, [(k, v)])
lookupDelete key = roll where
	roll []			= (Nothing, [])
	roll (x@(a, b):xs)
		| key == a	= (Just b, xs)
		| otherwise	= let ~(may, xs') = roll xs in (may, x:xs')

require :: Eq k => k -> TupleReader k v v
require key = with key >>= lift

requireWith :: Eq k => (v -> Maybe a) -> k -> TupleReader k v a
requireWith f key = do
	e <- with key
	lift $ f =<< e

option :: Eq k => a -> (v -> a) -> k -> TupleReader k v a
option def f key = maybe def f `fmap` with key

with :: Eq k => k -> TupleReader k v (Maybe v)
with key = do
	s <- get
	let (e, s') = lookupDelete key s
	put s'
	return e
