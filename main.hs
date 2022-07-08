{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Environment
import Data.List
import Data.Maybe
import GHC.TypeLits (ErrorMessage(Text))
import Data.Set (foldr)
import Data.Char


listMyRelations :: [String] -> Char -> [String]
listMyRelations relations vertex = filter (\(c:_) -> c == vertex) relations

getCharInPos :: [String] -> Int -> Char
getCharInPos list pos = head (list!!pos)

graphToMatrixAux :: String -> [String] -> [[Int]]
graphToMatrixAux rel all = map(\x -> relationToNumber all [rel, x]) (listMyRelations all (rel!!1))

relationToNumber :: [String] -> [String] -> [Int]
relationToNumber all cell = [fromMaybe 0 (findIndex(== cell!!0) all) + 1, fromMaybe 0 (findIndex(== cell!!1) all) + 1]

graphToMatrix :: [String] -> [[Int]]
graphToMatrix [] = []
graphToMatrix graph = foldMap (`graphToMatrixAux` graph) graph

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

inputConverter :: String -> String -> String -> [[String]]
inputConverter vertex relation pdl = [split ' ' vertex, split ' ' relation, split ' ' pdl]

isOperator :: String -> Int
isOperator ";" = 1
isOperator "*" = 2
isOperator a = 3

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

associateLastNext :: String -> String -> [Int]
associateLastNext [] [] = []
associateLastNext last next
    | length last == 3 = [digitToInt (last!!2), digitToInt (head next)]
    | length last /= 3 = [digitToInt (head last), digitToInt (head next)]


pdlToMatrixAux :: Int -> String -> [String] -> [Int]
pdlToMatrixAux index actual all
    | length actual == 1 && isOperator actual == 3 = [] -- Cenário de programa isolado (1)
    | length actual == 3 = [digitToInt (head actual), digitToInt (actual!!2)] -- Cenário de programas sequenciais (1;3)
    | length actual == 2 = [digitToInt (head actual), digitToInt (head actual)] -- Cenário de laço não determinísticos (1*)
    | length actual == 1 && isOperator actual == 2 = reverseList (pdlToMatrixAux (index - 1) (all!!(index -1)) all) -- Cenário de conectivo de laço não determinístico (*)
    | length actual == 1 && isOperator actual == 1 = associateLastNext (all!!(index-1)) (all!!(index+1)) -- Cenário de conectivo sequencial (;)

pdlToMatrix :: [String] -> [[Int]]
pdlToMatrix pdl = map(\elem -> pdlToMatrixAux (fromMaybe 0 (elemIndex elem pdl)) elem pdl) pdl

compareGraphToPdl :: [[Int]] -> [[Int]] -> Bool
compareGraphToPdl graphMatrix pdlMatrix = length (graphMatrix `intersect` pdlMatrix) == length graphMatrix

getFirstFault :: [[Int]] -> [[Int]] -> [Int]
getFirstFault graphMatrix pdlMatrix = (graphMatrix \\ (graphMatrix `intersect` pdlMatrix))!!0

translateToGraphNotation :: [[Int]] -> [[Int]] -> [[Char]] -> [[Char]]
translateToGraphNotation graphMatrix pdlMatrix graph = [graph!!((getFirstFault graphMatrix pdlMatrix)!!0-1),graph!!((getFirstFault graphMatrix pdlMatrix)!!1-1)]

main = do
  let graph = ["ab", "ba", "cc"]
  let graphMatrix = graphToMatrix graph
  let pdlMatrix = pdlToMatrix ["1;2", "*", "3*", ";", "4;5"]
  let isFault = not (compareGraphToPdl graphMatrix pdlMatrix)
  if isFault then print ("Falhou na etapa", translateToGraphNotation graphMatrix pdlMatrix graph)
  else print "Sucesso, o grafo esta contido no pdl apresentado!"
