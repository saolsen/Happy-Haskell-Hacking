module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "Enter a name: "
  name <- getLine
  putStrLn ("That name is: " ++ name)

main3 = do
  args <- getArgs
  putStrLn ("Added: " ++ result (args !! 0) (args !! 1))
  where
    result a b = show $ read a + read b

main1 = do
  args <- getArgs
  putStrLn ("hello, " ++ (args !! 0))

main2 = do
  args <- getArgs
  putStrLn ("hello, " ++ (args !! 0) ++ (args !! 1))