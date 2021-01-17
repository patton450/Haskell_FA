module Utils
( startswith,
contains,
endswith,
thrd,
) where

import Data.List

--startswith A B: where A,B are lists and returns True if list B starts with list A
startswith :: (Eq a) => [a] -> [a] -> Bool
startswith [] _ = True
startswith _ [] = False
startswith (a:a') (b:b') = ( a == b ) && startswith a' b'

--endswith A B: where A,B are lists and returns True if list B ends with list A
endswith :: (Eq a) => [a] -> [a] -> Bool
endswith [] _ = True
endswith _ [] = False
endswith x y = startswith (reverse x) (reverse y)

--contains A B: where A, B are lists and returns True if list B contains list A
contains :: (Eq a) => [a] -> [a] -> Bool
contains [] _ = True
contains _ [] = False
contains (a:a') (b:b')
    | (a == b)  = contains a' b'
    | otherwise = contains (a:a') b'

--third elem of tuple
thrd :: (a, b, c) -> c
thrd (_, _, c) = c
