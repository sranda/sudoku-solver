import Prelude
import System.Environment
import Data.List (sortBy)
import Data.Ord (comparing)

data Field a = Field { r :: Int -- row index
                     , c :: Int -- col index
                     , b :: Int -- box index
                     , d :: [a] -- data
                     }

type Board a = [Field a]

sortByPosition :: [Field a] -> [Field a]
sortByPosition = sortBy (comparing r) . sortBy (comparing c)

sortByDataSize :: [Field a] -> [Field a]
sortByDataSize = sortBy (comparing (\x -> (length . d) x))

--sortAscendingByRow :: [Field a] -> [Field a]
--sortAscendingByRow = sortBy (comparing r)

--sortAscendingByCol :: [Field a] -> [Field a]
--sortAscendingByCol = sortBy (comparing c)

--sortAscendingByBox :: [Field a] -> [Field a]
--sortAscendingByBox = sortBy (comparing b)

showsBoard :: (Show a) => Board a -> ShowS
showsBoard = shows . map d . sortByPosition

-- TODO
--readsBoard :: (Read a) => ReadS (Board a)
--readsBoard =

--hasClosure x = (==) ((length x) - 1) . length . filter (== x)
hasClosure x = (==) (length x) . length . filter (== x)

excludeClosure x y = if x == y then y else filter (`notElem` x) y

excludeClosures :: (Eq a) => [[a]] -> [[a]]
excludeClosures [] = []
excludeClosures (x:xs) = x :
                       if hasClosure x xs
                       then excludeClosures (map (excludeClosure x) xs)
                       else excludeClosures xs

--main = do
--    args <- getArgs
--    sudokuBoard <- readFile $ head args
--    putStr sudokuBoard
