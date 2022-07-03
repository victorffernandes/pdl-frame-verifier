import System.Environment
import Data.List
import Data.Text (splitOn)
import GHC.TypeLits (ErrorMessage(Text))

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

inputConverter :: [Char] -> [Char] -> [Char] -> [[[Char]]]
inputConverter vertex relation pdl = [split ' ' vertex, split ' ' relation, split ' ' pdl]

--getNextState :: Char -> [[Char]] -> [Char]
--getNextState actualState relations = filter ()


listMyRelations :: [[Char]] -> Char -> [[Char]]
listMyRelations relations vertex = filter (\(c:_) -> c == vertex) relations

getCharInPos :: [[Char]] -> Int -> Char
getCharInPos list pos = list!!pos!!0



main = do
    args <- getArgs                  -- IO [String]
    putStrLn "The arguments are:"
    putStrLn (head args)
    putStrLn (args!!1)
    putStrLn (args!!2)
    let inputs = ((inputConverter (args!!0) (args!!1) (args!!2)))
    print (inputs!!0)
    -- print ((inputs!!0)!!0!!0)
    let myChar = (getCharInPos (inputs!!0) 0)
    print(listMyRelations (inputs!!1) myChar)



-- graphParser textGraph = 






-- wordCount input = show (length (lines input)) ++ "\n" ++ show (length (lines input2)) ++ "\n"
-- wordCount2 input2 = show (length (lines input)) ++ "\n"

-- main = interact wordCount