module Wrap (wrap) where

import Data.List (inits, tails)
import Data.List.NonEmpty (NonEmpty (..), cons)
import Data.Maybe (catMaybes)

-- | Wrap a list of items to a given line width
wrap
    :: (a -> Bool)
    -- ^ valid break points
    -> Int
    -- ^ line width
    -> [a]
    -- ^ input
    -> NonEmpty [a]
    -- ^ wrapped output
wrap f n xs =
    let
        breaks = zip (inits xs) $ tails xs
        breaksAtSpace = filter (canSplit f . snd . snd) $ zip [0 ..] breaks
        withlast =
            catMaybes
                $ zipWith (cross n) breaksAtSpace
                $ drop 1 breaksAtSpace
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
