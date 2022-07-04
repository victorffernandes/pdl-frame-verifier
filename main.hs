import System.Environment
import Data.List
import Data.Text (splitOn)
import GHC.TypeLits (ErrorMessage(Text))


listMyRelations :: [[Char]] -> Char -> [[Char]]
listMyRelations relations vertex = filter (\(c:_) -> c == vertex) relations

getCharInPos :: [[Char]] -> Int -> Char
getCharInPos list pos = list!!pos!!0

axiom :: Char -> [[Char]] -> [[Char]]
axiom state relations = map tail (listMyRelations relations state)

sequential :: Char -> [Char] -> [[Char]] -> [[Char]]
sequential state pdl relations = concat (map (\x -> f state x relations) (split ';' pdl))
  

f :: Char -> [Char] -> [[Char]] -> [[Char]] -- retorna o erro e os estados possíveis
f state pdl relations
  | length pdl == 1 = axiom state relations
  | ';' `elem` pdl = sequential state pdl relations
  | otherwise = [" "]

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

inputConverter :: [Char] -> [Char] -> [Char] -> [[[Char]]]
inputConverter vertex relation pdl = [split ' ' vertex, split ' ' relation, split ' ' pdl]

main = do
    args <- getArgs                  -- IO [String]
    putStrLn "The arguments are:"
    let inputs = ((inputConverter (args!!0) (args!!1) (args!!2)))
    let states = (inputs!!0)
    print(states)
    

    concat(map (\x -> f (x) (inputs!!2!!0) (inputs!!1)) (['a', 'b']))
    print(f (inputs!!0!!0!!0) (inputs!!2!!0) (inputs!!1))






-- graphParser textGraph = 






-- wordCount input = show (length (lines input)) ++ "\n" ++ show (length (lines input2)) ++ "\n"
-- wordCount2 input2 = show (length (lines input)) ++ "\n"

-- main = interact wordCount