import System.Environment
import Data.List
import Data.Text (splitOn)
import GHC.TypeLits (ErrorMessage(Text))

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

inputConverter :: [Char] -> [Char] -> [Char] -> [[[Char]]]
inputConverter vertex relation pdl = [split ' ' vertex, split ' ' relation, split ' ' pdl]

main = do
    args <- getArgs                  -- IO [String]
    putStrLn "The arguments are:"
    putStrLn (head args)
    putStrLn (args!!1)
    putStrLn (args!!2)
    print(inputConverter (args!!0) (args!!1) (args!!2))
    -- inputConverter args!!0 args!!1 args!!2



-- graphParser textGraph = 






-- wordCount input = show (length (lines input)) ++ "\n" ++ show (length (lines input2)) ++ "\n"
-- wordCount2 input2 = show (length (lines input)) ++ "\n"

-- main = interact wordCount