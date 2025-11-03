{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Solutions.P3691 where
import Data.List (sortBy, inits, tails)
import Data.Function (on)
import Data.Map hiding (null, filter, take, map)
import Data.Ord (comparing, Down (..))
-- Solution implementation here
max_total_subarray_value :: [Int] -> Int -> Int
max_total_subarray_value xs k = sum
    $ take k $ sortBy (comparing Down)
    $ elems  $ fromList
    $ sortBy (compare `on` snd) 
    $ map (\x -> (x,maximum x - minimum x)) 
    $ filter (not . null) (tails xs ++ inits xs)
