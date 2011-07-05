module Network.Tremulous.TupleReader (
	TupleReader, tupleReader, require, requireWith, option
) where
import Control.Monad.State.Strict

type TupleReader k v a = StateT [(k, v)] Maybe a

tupleReader :: TupleReader k v a -> [(k, v)] -> Maybe a
tupleReader = evalStateT

lookupDelete :: (Eq k) => k -> [(k, v)] -> (Maybe v, [(k, v)])
lookupDelete key = roll where
	roll []			= (Nothing, [])
	roll (x@(a, b):xs)
		| key == a	= (Just b, xs)
		| otherwise	= let (may, xs') = roll xs in (may, x:xs')

require :: Eq k => k -> TupleReader k v v
require key = get >>= \s -> case lookupDelete key s of
	(Nothing, _)	-> lift Nothing
	(Just a, s')	-> put s' >> return a

requireWith :: Eq k => (v -> Maybe a) -> k -> TupleReader k v a
requireWith f key = get >>= \s -> case lookupDelete key s of
	(Nothing, _)	-> lift Nothing
	(Just a, s')	-> put s' >> lift (f a)

option :: Eq k => a -> (v -> a) -> k -> TupleReader k v a
option def f key = get >>= \s -> case lookupDelete key s of
	(Nothing, _)	-> return def
	(Just a, s')	-> put s' >> return (f a)
