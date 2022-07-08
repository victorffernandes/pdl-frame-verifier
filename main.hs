import System.Environment
import Data.List
import Data.Maybe
import GHC.TypeLits (ErrorMessage(Text))
import Data.Set (foldr)
import Data.Char


listMyRelations :: [[Char]] -> Char -> [[Char]]
listMyRelations relations vertex = filter (\(c:_) -> c == vertex) relations

getCharInPos :: [[Char]] -> Int -> Char
getCharInPos list pos = list!!pos!!0

graphToMatrixAux :: [Char] -> [[Char]] -> [[Int]]
graphToMatrixAux rel all = map(\x -> relationToNumber all [rel, x]) (listMyRelations all (rel!!1))

relationToNumber :: [[Char]] -> [[Char]] -> [Int]
relationToNumber all cell = [(fromMaybe 0 (findIndex(== cell!!0) all)) + 1, (fromMaybe 0 (findIndex(== cell!!1) all)) + 1]

graphToMatrix :: [[Char]] -> [[Int]]
graphToMatrix [] = []
graphToMatrix graph = foldMap (\rel -> graphToMatrixAux rel graph) graph

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

inputConverter :: [Char] -> [Char] -> [Char] -> [[[Char]]]
inputConverter vertex relation pdl = [split ' ' vertex, split ' ' relation, split ' ' pdl]

isOperator :: [Char] -> Int
isOperator ";" = 1
isOperator "*" = 2
isOperator a = 3

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

associateLastNext :: [Char] -> [Char] -> [Int]
associateLastNext last next
    | length last == 3 = [digitToInt (last!!2), digitToInt (next!!0)]
    | length last /= 3 = [digitToInt (last!!0), digitToInt (next!!0)]


pdlToMatrixAux :: Int -> [Char] -> [[Char]] -> [Int]
pdlToMatrixAux index actual all
    | length actual == 1 && isOperator(actual) == 3 = [] -- Cenário de programa isolado (1)
    | length actual == 3 = [digitToInt (actual!!0), digitToInt (actual!!2)] -- Cenário de programas sequenciais (1;3)
    | length actual == 2 = [digitToInt (actual!!0), digitToInt (actual!!0)] -- Cenário de laço não determinísticos (1*)
    | length actual == 1 && isOperator(actual) == 2 = reverseList (pdlToMatrixAux (index - 1) (all!!(index -1)) all) -- Cenário de conectivo de laço não determinístico (*)
    | length actual == 1 && isOperator(actual) == 1 = associateLastNext (all!!(index-1)) (all!!(index+1)) -- Cenário de conectivo sequencial (;)

pdlToMatrix :: [[Char]] -> [[Int]]
pdlToMatrix pdl = map(\elem -> pdlToMatrixAux (fromMaybe 0 (findIndex(== elem) pdl)) elem pdl) pdl





main = do 
  print (graphToMatrixAux "ab" ["ab", "bc", "cd"])
  print (graphToMatrix ["ab", "bc", "cd", "bk"])
  print (pdlToMatrix ["1;2", "*", "3*"])
