import Prelude
import System.Environment

--data Field =
--    Field Int |
--    Field Int Int |
--    Field Int Int Int |
--    Field Int Int Int Int |
--    Field Int Int Int Int Int |
--    Field Int Int Int Int Int Int |
--    Field Int Int Int Int Int Int Int |
--    Field Int Int Int Int Int Int Int Int |
--    Field Int Int Int Int Int Int Int Int Int
--data Permutation = Permutation Field Field Field Field Field Field Field Field Field

main = do
    args <- getArgs
    sudokuBoard <- readFile $ head args
    putStr sudokuBoard
