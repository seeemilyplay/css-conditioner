module CSSParser where

import Control.Applicative ((<$>))
import Data.Char
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.Parsec

data RuleSet = RuleSet [Selector] [Declaration]
  deriving Show

data Selector = Selector [[SelectorTerm]]
  deriving Show

data SelectorTerm = NamedElement String
                    | WildcardElement
                    | Id String
                    | Class String
                    | Pseudo String
  deriving Show

data Declaration = Declaration Property [Term]
  deriving Show

data Property = Property String
  deriving Show

data Term =   NumberTerm Double
            | Percentage Double
            | PixelLength Double
            | PointLength Double
            | Ems Double
            | StringTerm String
            | IdentTerm String
            | URI String
            | RGBColor Integer Integer Integer
  deriving Show

stylesheet :: GenParser Char st [RuleSet]
stylesheet = many ruleset

ruleset :: GenParser Char st RuleSet
ruleset = do
  spaces
  sels <- selectors
  spaces
  _ <- char '{'
  spaces
  decs <- declarations
  spaces
  _ <- char '}'
  spaces
  return $ RuleSet sels decs

selectors :: GenParser Char st [Selector]
selectors = try (sepBy1 selector comma) <|> endBy1 selector comma
  where
    comma = char ',' >> spaces

selector :: GenParser Char st Selector
selector = Selector <$> (
  try (sepBy1 simpleSelector spaces)
  <|> endBy1 simpleSelector spaces)

simpleSelector :: GenParser Char st [SelectorTerm]
simpleSelector = choice [withElement, withoutElement]
  where
    withElement = do
      el <- elementName
      rest <- many $ choice [identity, clazz, pseudo]
      return $ el : rest
    withoutElement = many1 $ choice [identity, clazz, pseudo]

elementName :: GenParser Char st SelectorTerm
elementName = choice [named, wildcard]
  where
    named = do
      s <- identString
      return $ NamedElement s
    wildcard = do
      _c <- char '*'
      return WildcardElement

identity :: GenParser Char st SelectorTerm
identity = do
  _ <- char '#'
  s <- many1 $ choice [oneOf "_-", alphaNum]
  return $ Id s

clazz :: GenParser Char st SelectorTerm
clazz = do
  _ <- char '.'
  s <- identString
  return $ Class s

pseudo :: GenParser Char st SelectorTerm
pseudo = do
  _ <- char ':'
  s <- identString
  return $ Pseudo s

declarations :: GenParser Char st [Declaration]
declarations = try (sepBy declaration semicolon) <|> endBy declaration semicolon
  where
    semicolon = char ';' >> spaces

declaration :: GenParser Char st Declaration
declaration = do
  prop <- property
  spaces
  _ <- char ':'
  spaces
  terms <- expr
  return $ Declaration prop terms

property :: GenParser Char st Property
property = do
  s <- identString
  return $ Property s

expr :: GenParser Char st [Term]
expr = sepBy1 term (optional operator >> spaces)

operator :: GenParser Char st Char
operator = char ','

term :: GenParser Char st Term
term = choice
  [ numericTerm
  , stringTerm
  , try uri <|> identTerm
  , identTerm
  , hexcolor ]

numericTerm :: GenParser Char st Term
numericTerm = do
  n <- num
  try (choice [ percentage n
              , try (pixelLength n) <|> pointLength n
              , ems n ]) <|> number n
  where
    percentage n = do
      _ <- char '%'
      return $ Percentage n
    pixelLength n = do
      _ <- string "px"
      return $ PixelLength n
    pointLength n = do
      _ <- string "pt"
      return $ PointLength n
    ems n = do
      _ <- string "em"
      return $ Ems n
    number n = return $ NumberTerm n

stringTerm :: GenParser Char st Term
stringTerm = StringTerm <$> choice [singleQuoted, doubleQuoted]
  where
    singleQuoted =
      between (char '\'') (char '\'') (many1 $ noneOf "\n\r\f\\\'")
    doubleQuoted =
      between (char '"') (char '"') (many1 $ noneOf "\n\r\f\\\"")

identTerm :: GenParser Char st Term
identTerm = do
  s <- identString
  return $ IdentTerm s

uri :: GenParser Char st Term
uri = URI <$> between (string "url(") (char ')') (many1 . satisfy $ (/=) ')')

hexcolor :: GenParser Char st Term
hexcolor = do
  _ <- char '#'
  try hexcolor6digits <|> hexcolor3digits

hexcolor3digits :: GenParser Char st Term
hexcolor3digits = do
  [r, g, b] <- map base16 <$> count 3 hexdigit
  return $ RGBColor r g b
  where
    base16 :: Integer -> Integer
    base16 x = x * 16 + 15

hexcolor6digits :: GenParser Char st Term
hexcolor6digits = do
  [r1, r2, g1, g2, b1, b2] <- count 6 hexdigit
  return $ RGBColor (base16 r1 r2) (base16 g1 g2) (base16 b1 b2)
  where
    base16 :: Integer -> Integer -> Integer
    base16 x1 x2 = x1 * 16 + x2

hexdigit :: GenParser Char st Integer
hexdigit = toInt . toLower <$> hexDigit
  where
    toInt :: Char -> Integer
    toInt '0' = 0
    toInt '1' = 1
    toInt '2' = 2
    toInt '3' = 3
    toInt '4' = 4
    toInt '5' = 5
    toInt '6' = 6
    toInt '7' = 7
    toInt '8' = 8
    toInt '9' = 9
    toInt 'a' = 10
    toInt 'b' = 11
    toInt 'c' = 12
    toInt 'd' = 13
    toInt 'e' = 14
    toInt 'f' = 15
    toInt c = error $ "Internal parser error, expected valid hex char got " ++ [c]

num :: GenParser Char st Double
num = do
  c <- option ' ' (char '-')
  let power = if c == '-' then (-1) else 1
  (*) power <$> (try asDecimal <|> asWhole)
  where
    asWhole = do
      s <- many1 digit
      returnNumberOrError s
    asDecimal = do
      s1 <- many digit
      _ <- char '.'
      s2 <- many1 digit
      returnNumberOrError $ s1 ++ "." ++ s2
    returnNumberOrError s =
      case maybeRead s of
        Just r -> return r
        Nothing -> error $ "Unable to parse as number: " ++ s
    maybeRead :: Read a => String -> Maybe a
    maybeRead = fmap fst . listToMaybe . reads

identString :: GenParser Char st String
identString = do
  c <- choice [char '_', letter]
  rest <- many $ choice [oneOf "_-", alphaNum]
  return $ c : rest

tryout :: FilePath -> IO ()
tryout file = do
  csscontents <- readFile file
  let css = parse stylesheet "Parse Error" csscontents
  print css
  return ()