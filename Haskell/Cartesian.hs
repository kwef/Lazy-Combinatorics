{-# LANGUAGE TupleSections #-}

module Cartesian where

import Data.List
import Data.Maybe
import Control.Arrow
import Safe

cartesian :: [a] -> [b] -> [(a,b)]
cartesian = curry $ concat . diagonalize . uncurry pairSpace

pairSpace :: [a] -> [b] -> [[(a,b)]]
pairSpace xs ys = map (\x -> map (x,) ys) xs

diagonalize :: [[a]] -> [[a]]
diagonalize =
   unfoldr (\(n,xss) ->
      toMaybe (not . null . fst)
      . second (n + 1,)
      $ stripN n xss)
   . (1,)

stripN :: Int -> [[a]] -> ([a],[[a]])
stripN n xss =
   (mapMaybe headMay firstN,
    mapMaybe tailMay firstN ++ rest)
   where (firstN,rest) = splitAt n xss

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p x | p x = Just x
toMaybe _ _       = Nothing
