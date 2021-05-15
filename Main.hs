module Main where
  import Types
  import Prelude hiding (words)
  import Data.Char (toLower)
  import Text.ParserCombinators.Parsec hiding (spaces)

  words :: Parse String
  words = many1 (oneOf (['a'..'z'] ++ ['A'..'Z']))

  spaces :: Parse ()
  spaces = skipMany (char ' ')

  newlines :: Parse ()
  newlines = skipMany (char '\n')

  parseReg :: Parse AssStruct
  parseReg = do
    char 'r'
    num <- many1 digit
    return $ Reg ((read num) :: Int)

  parseLitInt :: Parse AssStruct
  parseLitInt = do
    num <- many1 digit
    return $ LitInt ((read num) :: Int)

  parseLitStr :: Parse AssStruct
  parseLitStr = do
    str <- words
    return $ LitStr str

  parseArg :: Parse AssStruct
  parseArg = do
    arg <- parseReg <|> parseLitInt <|> parseLitStr
    return $ arg

  parseGlobal :: Parse AssStruct
  parseGlobal = do
    spaces
    newlines
    string "global"
    spaces
    char '_'
    global <- words
    return $ GlobalStart global

  parseText :: Parse AssStruct
  parseText = do
    spaces
    string ".text"
    spaces
    newlines
    instructs <- many1 (parseOneArg <|> parseTwoArg <|> parseThreeArg)
    return $ DataSection instructs

  parseOneArg :: Parse AssStruct
  parseOneArg = return Null

  parseTwoArg :: Parse AssStruct
  parseTwoArg = do
    instStr <- (map toLower) <$> words
    spaces
    arg1 <- parseArg
    spaces
    arg2 <- parseArg
    spaces
    let instruct = case instStr of
                     "mov" -> Mov arg1 arg2
                     "cmp" -> Cmp arg1 arg2
                     _ -> Null
    return instruct
    
  parseThreeArg :: Parse AssStruct
  parseThreeArg = do
    instStr <- (map toLower) <$> words
    spaces
    arg1 <- parseArg
    spaces
    arg2 <- parseArg
    spaces
    arg3 <- parseArg
    spaces
    let instruct = case instStr of
                     _ -> Null
    return instruct

  parseAss :: Parse (AssStruct, ParserState)
  parseAss = do
    n <- getState
    return (Null, n)

  main :: IO ()
  main = do
    f <- readFile "test.ass"
    case runParser parseAss initParserState "ERROR" f of
      Left err -> putStrLn $ (show err) ++ "\nYou did something bad. Try typing it again."
      Right val -> putStrLn $ show val