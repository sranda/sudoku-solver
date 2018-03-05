import Prelude
import System.Environment
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

hasClosure :: (Foldable t, Eq (t a)) => t a -> [t a] -> Bool
hasClosure x xs = if xLength < xClosures
                  then error ("Closure size exceeds field size.")
                  else xLength == xClosures
                  where (xLength, xClosures) = (length x, (length . filter (== x)) xs)

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

sortByAccessor :: Ord b => (Field a -> b) -> Board a -> Board a
sortByAccessor accessor = sortBy (comparing accessor)

sortByPosition :: Board a -> Board a
sortByPosition = sortByAccessor r . sortByAccessor c

equals :: (Eq a) => Board a -> Board a -> Bool
equals b1 b2 = sortByPosition b1 == sortByPosition b2

excludeClosuresInFields :: Eq a => [Field a] -> [Field a]
excludeClosuresInFields fs = zipWith (\f df -> Field {r = r f, c = c f, b = b f, d = df}) fs dfs
                             where dfs = (excludeClosures . map d) fs

selectPermutations :: Ord b => (Field a -> b) -> Board a -> [[Field a]]
selectPermutations accessor = groupBy (\f1 f2 -> accessor f1 == accessor f2) . sortByAccessor accessor

solvePermutations :: (Ord b, Eq b, Eq a) => (Field a -> b) -> [Field a] -> [Field a]
solvePermutations accessor = concat . map excludeClosuresInFields . selectPermutations accessor

generateSolutionSteps :: (Ord b, Eq a) => Board a -> [Field a -> b] -> [Board a]
generateSolutionSteps previousBoard [] = []
generateSolutionSteps previousBoard (accessor:accessors) =
    if equals previousBoard newBoard
    then generateSolutionSteps newBoard accessors
    else newBoard : generateSolutionSteps newBoard accessors
    where newBoard = solvePermutations accessor previousBoard

generateBoardSolutions :: Eq a => [Board a] -> [Board a]
generateBoardSolutions boardHistory =
    if solutionSteps == []
    then boardHistory
    else generateBoardSolutions (boardHistory ++ solutionSteps)
    where solutionSteps = generateSolutionSteps (last boardHistory) [r, c, b]

solve board = generateBoardSolutions [board]

-- Beware! Here be the lair of bad ugly monads. --

newLines = "\n" : newLines

main = do
    args <- getArgs
    let fileName = head args
    boardString <- readFile fileName
    let board = (read boardString)::(Board Int)
    let solutionString = (foldl (++) "" . zipWith (++) ("" : newLines) . map show) (solve board)
    writeFile (fileName ++ "_solution") solutionString
