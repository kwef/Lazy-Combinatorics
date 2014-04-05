{-# LANGUAGE TupleSections #-}

module Cartesian where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Arrow
import Safe

diagonalize :: [[a]] -> [[a]]
diagonalize =
   unfoldr (uncurry $ \n ->
      mfilter (not . null . fst) . Just
      . second (n + 1,) . stripN n)
   . (1,)

stripN :: Int -> [[a]] -> ([a],[[a]])
stripN n list =
   let (xs,rest) = splitAt n list
   in (mapMaybe headMay xs, mapMaybe tailMay xs ++ rest)

pairSpace :: [a] -> [b] -> [[(a,b)]]
pairSpace xs ys = map (\x -> map (x,) ys) xs
--pairSpace = flip (map . flip (map . (,)))

cartesian :: [a] -> [b] -> [(a,b)]
cartesian = curry $ concat . diagonalize . uncurry pairSpace
