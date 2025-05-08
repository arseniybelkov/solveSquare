module Main where
import System.Environment
import Solver (solveSquare)

 
main :: IO ()
main = do
    args <- getArgs
    if length args < 3
      then putStrLn "Please provide three numbers as arguments."
      else do
        let [s1, s2, s3] = take 3 args
            a = read s1 :: Double
            b = read s2 :: Double
            c = read s3 :: Double
        putStrLn $ "Roots for " ++ show (a, b, c) ++ " are " ++ show (solveSquare a b c)