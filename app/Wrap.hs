module Wrap where

import Control.Monad.Fix (fix)
import Data.List
import Data.List.NonEmpty (NonEmpty (..), cons)
import Data.Maybe (catMaybes)

-- wrap :: Int -> String -> [String]
wrap :: (a -> Bool) -> Int -> [a] -> NonEmpty [a]
wrap f n xs =
    let
        breaks = zip (inits xs) $ tails xs
        breaksAtSpace = filter (canSplit f . snd . snd) $ zip [0 ..] breaks
        withlast = catMaybes $ zipWith (cross n) breaksAtSpace $ tail breaksAtSpace
     in
        case headMaybe withlast of
            Just (ys, zs) -> ys `cons` wrap f n (dropWhile f zs)
            Nothing -> xs :| mempty

canSplit :: (a -> Bool) -> [a] -> Bool
canSplit _ [] = True
canSplit f (x : _xs) = f x

cross :: Int -> (Int, a) -> (Int, b) -> Maybe a
cross n (x, xs) (y, _ys)
    | x <= n && y > n = Just xs
    | otherwise = Nothing

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

data Wraps a = Wraps
    { prev :: Maybe (Wraps a)
    , current :: a
    , next :: Maybe (Wraps a)
    }
    deriving (Show)

mkWraps :: (a -> Bool) -> Int -> [a] -> Wraps [a]
mkWraps f n xs = go Nothing (wrap f n xs)
  where
    go past (ys :| yss) = fix $ \w ->
        Wraps
            { prev = past
            , current = ys
            , next = case yss of
                z : zs -> Just $ go (Just w) (z :| zs)
                [] -> Nothing
            }