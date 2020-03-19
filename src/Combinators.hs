module Combinators where

import           Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f (Parser p) = Parser $ \input ->
      case p input of
          Failure err -> Failure err
          Success rest x -> Success rest $ f x

instance Applicative (Parser error input) where
  pure a = Parser $ \input -> Success input a
  (Parser p) <*> (Parser q) = Parser $ \input ->
      case p input of
          Failure err -> Failure err
          Success rest f ->
              case q rest of
                  Failure err -> Failure err
                  Success rest2 x -> Success rest2 $ f x

instance Monad (Parser error input) where

  (Parser p) >>= f = Parser $ \input ->
      case p input of
        Success i r -> runParser (f r) i
        Failure e   -> Failure e

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure mempty

  p <|> q = Parser $ \input ->
      case runParser p input of
        Failure _ -> runParser q input
        x         -> x

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (many (sep *> elem))

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    []           -> Failure $ "Empty string"
    (x:xs)       -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success = pure

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure

strEq :: String -> Parser String String String
strEq = foldr (\c rest -> (:) <$> symbol c <*> rest) (success [])