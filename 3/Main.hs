{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}

module Main where

import           Control.Applicative
import           Data.Maybe
import           System.IO

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
    deriving (Functor)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    Parser p1 <*> Parser p2 = Parser $ \input -> do
        (f, input') <- p1 input
        (x, input'') <- p2 input'
        Just (f x, input'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input

skipManyTill :: Alternative m => m a -> m end -> m end
skipManyTill p end = go
  where
    go = end <|> (p *> go)

parseAnyChar :: Parser Char
parseAnyChar = Parser $ \case
    c:cs -> Just (c, cs)
    _    -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \case
    c':cs | c == c' -> Just (c, cs)
    _               -> Nothing

parseString :: String -> Parser String
parseString = traverse parseChar

parseInt :: Parser Int
parseInt = Parser $ \input -> case reads input of
    [(x, input')] -> Just (x, input')
    _             -> Nothing

data Expression
    = Mul Int Int
    | Do
    | Dont
    deriving (Show, Eq)

parseMul :: Parser Expression
parseMul = do
    _ <- parseString "mul("
    a <- parseInt
    _ <- parseChar ','
    b <- parseInt
    _ <- parseChar ')'
    return $ Mul a b

parseDo :: Parser Expression
parseDo = do
    _ <- parseString "do()"
    return Do

parseDont :: Parser Expression
parseDont = do
    _ <- parseString "don't()"
    return Dont

parseLine :: Parser [Expression]
parseLine = many $ skipManyTill parseAnyChar (parseMul <|> parseDo <|> parseDont)

evalExpressionsPart1 :: [Expression] -> Int
evalExpressionsPart1 (Mul a b : xs) = a * b + evalExpressionsPart1 xs
evalExpressionsPart1 (_ : xs)       = evalExpressionsPart1 xs
evalExpressionsPart1 []             = 0

lastDoOrDontExpression :: [Expression] -> Expression
lastDoOrDontExpression (x:xs) = if x == Do || x == Dont then x else lastDoOrDontExpression xs
lastDoOrDontExpression [] = Do -- enabled by default

evalExpressionsPart2 :: [Expression] -> Int
evalExpressionsPart2 xs = evalExpressionsPart2' xs []
    where
        evalExpressionsPart2' :: [Expression] -> [Expression] -> Int
        evalExpressionsPart2' (x:xs) stack =
            case x of
              Mul a b ->
                  if lastDoOrDontExpression stack == Do then
                      a * b + evalExpressionsPart2' xs stack
                  else
                      evalExpressionsPart2' xs stack
              e -> evalExpressionsPart2' xs (e : stack)
        evalExpressionsPart2' [] _ = 0

main :: IO ()
main = do
    contents <- openFile "input.txt" ReadMode >>= hGetContents
    let (memory, _) = unzip $ mapMaybe (runParser parseLine) $ lines contents
    -- Part 1
    print $ evalExpressionsPart1 (concat memory)
    -- Part 2
    print $ evalExpressionsPart2 (concat memory)
