module Tools
(
    ltrInStr,
    deepMap,
    dirToCrds
) where

import Datatypes

ltrInStr :: Char -> String -> Bool
ltrInStr ltr str = length (filter (\x -> x == ltr) str) == 1

deepMap :: (a -> b) -> [[a]] -> [[b]]
deepMap func mappable = map (\e -> map func e) mappable

dirToCrds :: Direction -> Coords -> Coords
dirToCrds dir (x, y)
    | dir == 'h' = (x, y - 1)
    | dir == 'j' = (x + 1, y)
    | dir == 'k' = (x - 1, y)
    | dir == 'l' = (x, y + 1)
    | dir == 'y' = (x - 1, y - 1)
    | dir == 'u' = (x - 1, y + 1)
    | dir == 'b' = (x + 1, y - 1)
    | dir == 'n' = (x + 1, y + 1)
    | otherwise  = (x, y)
