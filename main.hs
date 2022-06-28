import System.Environment
import Data.List

main = do
    args <- getArgs                  -- IO [String]
    putStrLn "The arguments are:"
    putStrLn (head args)
    putStrLn (last args)









--wordCount input = show (length (lines input)) ++ "\n" ++ show (length (lines input2)) ++ "\n"
--wordCount2 input2 = show (length (lines input)) ++ "\n"

-- main = interact wordCount