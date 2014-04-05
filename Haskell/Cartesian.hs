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
   unfoldr (uncurry $ \n ->
      toMaybe (not . null . fst)
      . second (n + 1,) . stripN n)
   . (1,)

stripN :: Int -> [[a]] -> ([a],[[a]])
stripN n list =
   (mapMaybe headMay firstN, mapMaybe tailMay firstN ++ rest)
   where (firstN,rest) = splitAt n list

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe predicate item | predicate item = Just item
toMaybe _         _    | otherwise      = Nothing
