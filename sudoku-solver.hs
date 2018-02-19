import Prelude
import System.Environment

data Field a = Field { r :: Int -- row index
                     , c :: Int -- col index
                     , b :: Int -- box index
                     , d :: [a] -- data
                     } deriving(Show)

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
