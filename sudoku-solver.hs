import Prelude
import System.Environment
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

data Field a = Field { r :: Int -- row index
                     , c :: Int -- col index
                     , b :: Int -- box index
                     , d :: [a] -- data
                     } deriving(Eq)

showsField :: (Show a) => Field a -> ShowS
showsField f = shows (r f, c f, b f, d f)

instance Show a => Show (Field a) where
    showsPrec _ x = showsField x

readsField :: (Read a) => ReadS (Field a)
readsField s = [(Field {r=ri,c=ci,b=bi,d=df},t) | ((ri, ci, bi, df),t) <- reads s]

instance Read a => Read (Field a) where
    readsPrec _ s = readsField s

type Board a = [Field a]

sortByRowIndex :: Board a -> Board a
sortByRowIndex = sortBy (comparing r)

sortByColIndex :: Board a -> Board a
sortByColIndex = sortBy (comparing c)

sortByBoxIndex :: Board a -> Board a
sortByBoxIndex = sortBy (comparing b)

sortByPosition :: Board a -> Board a
sortByPosition = sortBy (comparing r) . sortBy (comparing c)

--sortByDataSize :: Board a -> Board a
--sortByDataSize = sortBy (comparing (\x -> (length . d) x))

hasClosure :: (Foldable t, Eq (t a)) => t a -> [t a] -> Bool
hasClosure x xs = if xLength < xClosures
                  then error ("Closure size exceeds field size.")
                  else xLength == xClosures
                  where (xLength, xClosures) = (length x, length $ filter (== x) xs)

excludeClosure :: (Eq a) => [a] -> [[a]] -> [[a]]
excludeClosure _ [] = []
excludeClosure x (y:ys) = (if x == y then y else filter (`notElem` x) y) : (excludeClosure x ys)

_excludeClosures :: Eq a => [[a]] -> [[a]] -> [[a]]
_excludeClosures [] ys = ys
_excludeClosures (x:xs) ys = if hasClosure x ys && ys /= zs
                             then _excludeClosures zs zs
                             else _excludeClosures xs ys
                             where zs = excludeClosure x ys

excludeClosures :: (Eq a) => [[a]] -> [[a]]
excludeClosures xs = _excludeClosures xs xs

--aaa :: [Field a] -> [Field a]
--aaa fs = zipWith (\f df -> Field {r = r f, c = c f, b = b f, d = df}) fs dfs
         where dfs = (excludeClosures . map d) fs

--solvePermutations :: Eq a => ([a] -> [a] -> Bool) -> [[a]] -> [[a]]
--solvePermutations f = concat . map (\x -> excludeClosures (d x)) . groupBy f

--solve board = if board == newBoard
--              then board
--              else solve newBoard
--              where newBoard = (
--                               solvePermutations (\x -> r x) . sortByRowIndex .
--                               solvePermutations (\x -> c x) . sortByColIndex .
--                               solvePermutations (\x -> b x) . sortByBoxIndex
--                               ) board

main = do
    args <- getArgs
    boardString <- readFile $ head args
    --board = (read boardString)::(Board Int)
    putStr boardString
