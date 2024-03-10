{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Generator
    ( Generator
    , Dir (..)
    , generate
    , each
    , dropUntil
    , collect
    )
where

import Data.Bifunctor (Bifunctor (..), first)
import Data.List.NonEmpty (NonEmpty (..))

-- | Infinite effectful stream of `a` values
data Generator m a = Elem a (Generator m a) | Effect (m (Generator m a)) | Done

-- | Direction of the generator
data Dir = Up | Down

next :: (Enum b) => Dir -> b -> b
next Up = pred
next Down = succ

-- | Generate an infinite stream of values from an indexed monadic action
generate
    :: (Monad m, Enum b)
    => (b -> m (Maybe a))
    -- ^ Get a line by number
    -> b
    -- ^ Starting line number
    -> Dir
    -> Generator m (b, a)
generate pick from dir = go from
  where
    go i = Effect $ do
        mline <- pick i
        pure $ case mline of
            Nothing -> Done
            Just line -> Elem (i, line) $ go $ next dir i

-- | Given a function that can split a value into a non-empty list of values,
-- apply it to each value in the generator and add generator layers for each
each
    :: forall m a c
     . (Monad m)
    => (a -> NonEmpty c)
    -> Generator m a
    -> Generator m c
each f (Elem x g) =
    let loop :: NonEmpty c -> Generator m c
        loop (y :| []) = Elem y $ each f g
        loop (y :| y' : ys) =
            let g'' = loop (y' :| ys)
             in Elem y g''
     in loop $ f x
each f (Effect m) = Effect $ each f <$> m
each _ Done = Done

-- | Drop values from the generator until a predicate is satisfied
dropUntil :: (Monad m) => (a -> Bool) -> Generator m a -> Generator m a
dropUntil p e@(Elem x g) = if p x then e else dropUntil p g
dropUntil p (Effect m) = Effect $ dropUntil p <$> m
dropUntil _ Done = Done

-- | Extract a finite number of values from the generator and the next one
collect
    :: forall m a
     . (Monad m)
    => Int
    -> Generator m a
    -> m ([a], Maybe a)
collect n g = go n g
  where
    go :: Int -> Generator m a -> m ([a], Maybe a)
    go m (Elem x g') =
        case m of
            0 -> pure ([], Just x)
            _ -> first (x :) <$> go (m - 1) g'
    go m (Effect e) = e >>= go m
    go _ Done = pure ([], Nothing)
