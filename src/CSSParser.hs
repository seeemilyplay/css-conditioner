{- |
    An incomplete css parser which pretty much follows this
    http://www.w3.org/TR/CSS21/grammar.html
-}
module CSSParser where

import Control.Applicative ((<$>))
import Data.Char
import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

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

data Declaration = Declaration Property [Term] (Maybe Priority)
  deriving Show

data Property = Property String
  deriving Show

data Priority = Priority String
  deriving Show

data Term =   NumberTerm Double (Maybe Unit)
            | StringTerm String
            | IdentTerm String
            | URI String
            | RGBColor Integer Integer Integer
            | FunctionTerm String [Term]
  deriving Show

data Unit = Percentage
          | Pixel
          | Centimeter
          | Millimeter
          | Inch
          | Point
          | Pica
          | Ems
          | Exs
          | Degree
          | Radian
          | Grad
          | Millisecond
          | Second
          | Hertz
          | Kilohertz
  deriving Show

stylesheet :: GenParser Char st [RuleSet]
stylesheet = do
  (whiteSpace lexer)
  many ruleset

ruleset :: GenParser Char st RuleSet
ruleset = do
  sels <- selectors
  decs <- braces lexer declarations
  return $ RuleSet sels decs

selectors :: GenParser Char st [Selector]
selectors = do
  s <- selector
  mcomma <- optionMaybe (comma lexer)
  case mcomma of
    Just _ -> do
      sels <- selectors
      return $ s : sels
    Nothing -> return [s]

selector :: GenParser Char st Selector
selector = Selector <$> many1 simpleSelector

simpleSelector :: GenParser Char st [SelectorTerm]
simpleSelector = lexeme lexer $ choice [withElement, withoutElement]
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
      s <- map toLower <$> unlexemedIdentifier
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
  s <- unlexemedIdentifier
  return $ Class s

pseudo :: GenParser Char st SelectorTerm
pseudo = do
  _ <- char ':'
  s <- map toLower <$> unlexemedIdentifier
  return $ Pseudo s

unlexemedIdentifier :: GenParser Char st String
unlexemedIdentifier = do
  start <- identStart cssDef
  rest <- many $ identLetter cssDef
  return $ start : rest

declarations :: GenParser Char st [Declaration]
declarations = do
  mdec <- optionMaybe declaration
  msemi <- optionMaybe (semi lexer)
  case (mdec, msemi) of
    (Just dec, Nothing) -> return [dec]
    (Just dec, Just _) -> do
      decs <- declarations
      return $ dec : decs
    (Nothing, Just _) -> declarations
    (Nothing, Nothing) -> return []

declaration :: GenParser Char st Declaration
declaration = do
  prop <- property
  _ <- (colon lexer)
  terms <- expr
  mprior <- optionMaybe priority
  return $ Declaration prop terms mprior

property :: GenParser Char st Property
property = Property . map toLower <$> identifier lexer

priority :: GenParser Char st Priority
priority = do
  _ <- char '!'
  Priority . map toLower <$> identifier lexer

expr :: GenParser Char st [Term]
expr = sepBy1 term (optional op)

op :: GenParser Char st String
op = comma lexer <|> forwardSlash
  where
    forwardSlash = lexeme lexer $ do
      c <- char '/'
      return [c]

term :: GenParser Char st Term
term = choice
  [ numericTerm
  , stringTerm
  , try uri <|> (try functionTerm <|> identTerm)
  , try functionTerm <|> identTerm
  , identTerm
  , hexcolor ]

numericTerm :: GenParser Char st Term
numericTerm = do
  n <- num
  munit <- optionMaybe unit
  return $ NumberTerm n munit

unit :: GenParser Char st Unit
unit = choice
  [ percentage
  , centimeter
  , inch
  , degree
  , radian
  , grad
  , second
  , hertz
  , kilohertz
  , try pixel <|> (try point <|> pica)
  , try millimeter <|> millisecond
  ]

percentage :: GenParser Char st Unit
percentage = lexeme lexer $ do
  _ <- char '%'
  return Percentage

centimeter :: GenParser Char st Unit
centimeter = lexeme lexer $ do
  _ <- stringIgnoreCase "cm"
  return Centimeter

inch :: GenParser Char st Unit
inch = lexeme lexer $ do
  _ <- stringIgnoreCase "in"
  return Inch

degree :: GenParser Char st Unit
degree = lexeme lexer $ do
  _ <- stringIgnoreCase "deg"
  return Degree

radian :: GenParser Char st Unit
radian = lexeme lexer $ do
  _ <- stringIgnoreCase "rad"
  return Radian

grad :: GenParser Char st Unit
grad = lexeme lexer $ do
  _ <- stringIgnoreCase "grad"
  return Grad

second :: GenParser Char st Unit
second = lexeme lexer $ do
  _ <- char 's'
  return Second

hertz :: GenParser Char st Unit
hertz = lexeme lexer $ do
  _ <- stringIgnoreCase "hz"
  return Hertz

kilohertz :: GenParser Char st Unit
kilohertz = lexeme lexer $ do
  _ <- stringIgnoreCase "khz"
  return Kilohertz

pixel :: GenParser Char st Unit
pixel = lexeme lexer $ do
  _ <- stringIgnoreCase "px"
  return Pixel

point :: GenParser Char st Unit
point = lexeme lexer $ do
  _ <- stringIgnoreCase "pt"
  return Point

pica :: GenParser Char st Unit
pica = lexeme lexer $ do
  _ <- stringIgnoreCase "pc"
  return Pica

millimeter :: GenParser Char st Unit
millimeter = lexeme lexer $ do
  _ <- stringIgnoreCase "mm"
  return Millimeter

millisecond :: GenParser Char st Unit
millisecond = lexeme lexer $ do
  _ <- stringIgnoreCase "ms"
  return Millisecond

stringTerm :: GenParser Char st Term
stringTerm = StringTerm <$> quotedString

identTerm :: GenParser Char st Term
identTerm = do
  s <- identifier lexer
  return . IdentTerm $ map toLower s

uri :: GenParser Char st Term
uri = URI <$> do
  _ <- stringIgnoreCase "url"
  parens lexer url
  where
    url = quotedString <|> unquotedString
    unquotedString = lexeme lexer $ manyTill anyChar ending
    ending = lookAhead $ do
      (whiteSpace lexer)
      char ')'

stringIgnoreCase :: String -> GenParser Char st String
stringIgnoreCase [] = return ""
stringIgnoreCase (c:cs) = do
  x <- charIgnoreCase c
  xs <- stringIgnoreCase cs
  return $ x : xs

charIgnoreCase :: Char -> GenParser Char st Char
charIgnoreCase c =
  char (toUpper c) <|> char (toLower c)

quotedString :: GenParser Char st String
quotedString = lexeme lexer $ singleQuoted <|> doubleQuoted
  where
    singleQuoted =
      between (char '\'') (char '\'') (many1 $ noneOf "\n\r\f\\\'")
    doubleQuoted =
      between (char '"') (char '"') (many1 $ noneOf "\n\r\f\\\"")

hexcolor :: GenParser Char st Term
hexcolor = lexeme lexer $ do
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

functionTerm :: GenParser Char st Term
functionTerm = do
  f <- map toLower <$> identifier lexer
  args <- parens lexer expr
  return $ FunctionTerm f args

num :: GenParser Char st Double
num = do
  c <- option '+' (char '-' <|> char '+')
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

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser cssDef

cssDef :: GenLanguageDef String u Identity
cssDef =
  emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = ""
    , nestedComments = False
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> oneOf "_-"
    , caseSensitive = False
    }

tryout :: FilePath -> IO ()
tryout file = do
  csscontents <- readFile file
  let css = parse stylesheet file csscontents
  print css
  return ()