module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

--parseNumber = many1 digit >>= (return . Number . read)

--parseNumber = do digits <- many1 digit
--                 return $ Number (read digits)

parseString :: Parser LispVal
parseString = do { char '"'
                 ; x <- many (do { char '\\'
                                  ; char '"'
                                  ; return '"' }
                              <|> (noneOf "\""))
                 ; char '"'
                 ; return $ String x }

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseAtom
        <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))