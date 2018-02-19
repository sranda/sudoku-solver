import Prelude
import System.Environment
import Data.List (sortBy)
import Data.Ord (comparing)

data Field a = Field { r :: Int -- row index
                     , c :: Int -- col index
                     , b :: Int -- box index
                     , d :: [a] -- data
                     } deriving(Show)

sortAscendingByRow :: [Field a] -> [Field a]
sortAscendingByRow = sortBy (comparing r)

sortAscendingByCol :: [Field a] -> [Field a]
sortAscendingByCol = sortBy (comparing c)

sortAscendingByBox :: [Field a] -> [Field a]
sortAscendingByBox = sortBy (comparing b)

sortAscendingByDataSize :: [Field a] -> [Field a]
sortAscendingByDataSize = sortBy (comparing (\x -> (length . d) x))

hasClosure x = (==) ((length x) - 1) . length . filter (==x)

exclude x = filter (`notElem` x)

excludeClosures :: (Eq a) => [[a]] -> [[a]]
excludeClosures [] = []
excludeClosures (x:xs) = x :
                       if hasClosure x xs
                       then excludeClosures (map (exclude x) xs)
                       else excludeClosures xs

--main = do
--    args <- getArgs
--    sudokuBoard <- readFile $ head args
--    putStr sudokuBoard
