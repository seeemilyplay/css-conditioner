{- |
    An incomplete css parser which pretty much follows this
    http://www.w3.org/TR/CSS21/grammar.html.
    It tries not to be too strict, and it doesn't understand CSS3.

    missing stuff:  correct char handling (escape & non-ascii chars)
                    stuff that can go at the top of your stylesheet
                    @font-face stuff (not in spec but hey)
-}
module CSSParser where

import Control.Applicative ((<$>))
import Data.Char
import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Stylesheet = Stylesheet (Maybe CharSet) [RuleSet]
  deriving (Eq, Read, Show)

data CharSet = CharSet String
  deriving (Eq, Read, Show)

data Media = Media [String] [RuleSet]
  deriving (Eq, Read, Show)

data RuleSet = RuleSet [Selector] [Declaration]
  deriving (Eq, Read, Show)

data Selector = Selector [SelectorPart]
  deriving (Eq, Read, Show)

data SelectorPart =  NearestChildSelector [SelectorTerm]
                   | PlusSelector [SelectorTerm]
                   | SimpleSelector [SelectorTerm]
  deriving (Eq, Read, Show)

data SelectorTerm = NamedElement String
                    | WildcardElement
                    | Id String
                    | Class String
                    | Pseudo String
                    | Attribute String (Maybe AttributeMatch)
  deriving (Eq, Read, Show)

data AttributeMatch =   EqualMatch String
                      | IncludesMatch String
                      | DashMatch String
  deriving (Eq, Read, Show)

data Declaration = Declaration Property [Term] (Maybe Priority)
  deriving (Eq, Read, Show)

data Property = Property String (Maybe PropertyHack)
  deriving (Eq, Read, Show)

data PropertyHack = PropertyHack Char
  deriving (Eq, Read, Show)

data Priority = Priority String
  deriving (Eq, Read, Show)

data Term =   NumericTerm Double (Maybe Unit)
            | StringTerm String
            | IdentTerm String
            | URI String
            | RGBColor Integer Integer Integer
            | OpacityHack Double
            | FunctionTerm String [Term]
  deriving (Eq, Read, Show)

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
  deriving (Eq, Read, Show)

stylesheet :: GenParser Char st Stylesheet
stylesheet = do
  (whiteSpace lexer)
  mcharset <- optionMaybe charset
  rulesets <- many ruleset
  _ <- eof
  return $ Stylesheet mcharset rulesets

charset :: GenParser Char st CharSet
charset = (do
  _ <- lexeme lexer $ stringIgnoreCase "@charset "
  cs <- map toLower <$> quotedString
  _ <- semi lexer
  return $ CharSet cs) <?> "@charset"

media :: GenParser Char set Media
media = (do
  _ <- lexeme lexer $ stringIgnoreCase "@media "
  m <- map toLower <$> identifier lexer
  ms <- many (do
    _ <- comma lexer
    map toLower <$> identifier lexer)
  rs <- braces lexer $ many ruleset
  return $ Media (m:ms) rs
  ) <?> "@media"

ruleset :: GenParser Char st RuleSet
ruleset = (do
  sels <- selectors
  decs <- braces lexer declarations
  return $ RuleSet sels decs) <?> "rule set"

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
selector = (Selector <$> do
  s <- simpleSelector
  ss <- many selectorPart
  return $ s : ss) <?> "selector"

selectorPart :: GenParser Char st SelectorPart
selectorPart = nearestChildSelector <|> plusSelector <|> simpleSelector

nearestChildSelector :: GenParser Char st SelectorPart
nearestChildSelector = (do
  _ <- lexeme lexer $ char '>'
  NearestChildSelector <$> selectorTerms) <?> ""

plusSelector :: GenParser Char st SelectorPart
plusSelector = (do
  _ <- lexeme lexer $ char '+'
  PlusSelector <$> selectorTerms) <?> ""

simpleSelector :: GenParser Char st SelectorPart
simpleSelector = SimpleSelector <$> selectorTerms

selectorTerms :: GenParser Char st [SelectorTerm]
selectorTerms = lexeme lexer $ choice [withElement, withoutElement]
  where
    withElement = do
      el <- elementName
      rest <- many $ choice [identity, clazz, pseudoElement, attribute]
      return $ el : rest
    withoutElement = many1 $ choice [identity, clazz, pseudoElement, attribute]

elementName :: GenParser Char st SelectorTerm
elementName = (named <|> wildcard) <?> "element-name"
  where
    named = do
      s <- map toLower <$> unlexemedIdentifier
      return $ NamedElement s
    wildcard = do
      _c <- char '*'
      return WildcardElement

attribute :: GenParser Char st SelectorTerm
attribute = brackets lexer (do
  attName <- identifier lexer
  mmatch <- optionMaybe match
  return $ Attribute attName mmatch) <?> "[attribute]"
  where
    match = (equalMatch <|> includesMatch <|> dashMatch) <?> "att val"
    equalMatch = do
      _ <- lexeme lexer $ char '='
      val <- identifier lexer <|> quotedString
      return $ EqualMatch val
    includesMatch = do
      _ <- lexeme lexer $ string "~="
      val <- identifier lexer <|> quotedString
      return $ IncludesMatch val
    dashMatch = do
      _ <- lexeme lexer $ string "|="
      val <- identifier lexer <|> quotedString
      return $ DashMatch val

identity :: GenParser Char st SelectorTerm
identity = (do
  _ <- char '#'
  s <- many1 $ choice [oneOf "_-", alphaNum]
  return $ Id s) <?> "#id"

clazz :: GenParser Char st SelectorTerm
clazz = (do
  _ <- char '.'
  s <- unlexemedIdentifier
  return $ Class s) <?> ".class"

pseudoElement :: GenParser Char st SelectorTerm
pseudoElement = (do
  _ <- char ':'
  s <- map toLower <$> unlexemedIdentifier
  return $ Pseudo s) <?> ":pseudo-element"

unlexemedIdentifier :: GenParser Char st String
unlexemedIdentifier = (do
  start <- identStart cssDef
  rest <- many $ identLetter cssDef
  return $ start : rest) <?> "identifier"

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
declaration = (do
  prop <- property
  _ <- (colon lexer)
  terms <- expr
  mprior <- optionMaybe priority
  return $ Declaration prop terms mprior) <?> "declaration"

property :: GenParser Char st Property
property = (hackedProperty <|> unhackedProperty) <?> "property"

unhackedProperty :: GenParser Char st Property
unhackedProperty = do
  prop <- map toLower <$> identifier lexer
  return $ Property prop Nothing

hackedProperty :: GenParser Char st Property
hackedProperty = do
  hack <- PropertyHack <$> oneOf "#_*"
  prop <- map toLower <$> identifier lexer
  return $ Property prop (Just hack)

priority :: GenParser Char st Priority
priority = (do
  _ <- lexeme lexer $ char '!'
  Priority . map toLower <$> identifier lexer) <?> "priority"

expr :: GenParser Char st [Term]
expr = sepBy term (optional termSeparator)

termSeparator :: GenParser Char st String
termSeparator = (comma lexer <|> forwardSlash) <?> "term separator"
  where
    forwardSlash = lexeme lexer $ do
      c <- char '/'
      return [c]

term :: GenParser Char st Term
term = choice
  [ numericTerm
  , stringTerm
  , try uri <|> (try opacityHack <|> (try functionTerm <|> identTerm))
  , try opacityHack <|> (try functionTerm <|> identTerm)
  , try functionTerm <|> identTerm
  , identTerm
  , hexcolor ]

numericTerm :: GenParser Char st Term
numericTerm = (do
  n <- num
  munit <- optionMaybe unit
  return $ NumericTerm n munit) <?> "numeric term"

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
  , try ems <|> exs
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
  _ <- charIgnoreCase 's'
  return Second

hertz :: GenParser Char st Unit
hertz = lexeme lexer $ do
  _ <- stringIgnoreCase "hz"
  return Hertz

kilohertz :: GenParser Char st Unit
kilohertz = lexeme lexer $ do
  _ <- stringIgnoreCase "khz"
  return Kilohertz

ems :: GenParser Char st Unit
ems = lexeme lexer $ do
  _ <- stringIgnoreCase "em"
  return Ems

exs :: GenParser Char st Unit
exs = lexeme lexer $ do
  _ <- stringIgnoreCase "ex"
  return Exs

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
uri = (URI <$> do
  _ <- stringIgnoreCase "url"
  parens lexer url) <?> "uri"
  where
    url = quotedString <|> unquotedString
    unquotedString = lexeme lexer (manyTill anyChar ending) <?> "unquoted string"
    ending = lookAhead $ do
      (whiteSpace lexer)
      char ')'

opacityHack :: GenParser Char st Term
opacityHack = (OpacityHack <$> do
  _ <- stringIgnoreCase "alpha"
  parens lexer opacity) <?> ""
  where
    opacity = do
      _ <- lexeme lexer $ stringIgnoreCase "opacity"
      _ <- lexeme lexer $ char '='
      num

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
quotedString = lexeme lexer (singleQuoted <|> doubleQuoted) <?> "quoted string"
  where
    singleQuoted =
      between (char '\'') (char '\'') (many $ noneOf "\n\r\f\\\'")
    doubleQuoted =
      between (char '"') (char '"') (many $ noneOf "\n\r\f\\\"")

hexcolor :: GenParser Char st Term
hexcolor = lexeme lexer (do
  _ <- char '#'
  col <- try hexcolor6digits <|> hexcolor3digits
  mdig <- lookAhead $ optionMaybe hexdigit
  case mdig of
    Just _ -> fail "3 or 6 digits in hexcolor"
    Nothing -> return col) <?> "hexcolor"

hexcolor3digits :: GenParser Char st Term
hexcolor3digits = do
  [r, g, b] <- map base16 <$> count 3 hexdigit
  return $ RGBColor r g b
  where
    base16 :: Integer -> Integer
    base16 x = x * 16 + x

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
functionTerm = (do
  f <- map toLower <$> identifier lexer
  args <- parens lexer expr
  return $ FunctionTerm f args) <?> "function"

num :: GenParser Char st Double
num = lexeme lexer (do
  power <- sign
  (*) power <$> (try decimalNum <|> wholeNum)) <?> "number"
  where
    sign :: GenParser Char st Double
    sign = do
      c <- option '+' (char '-' <|> char '+')
      return $ if c == '-' then (-1) else 1
    wholeNum = do
      s <- many1 digit
      returnNumberOrError s
    decimalNum = do
      s1 <- option "0" (many1 digit)
      _ <- char '.'
      s2 <- many1 digit
      returnNumberOrError $ s1 ++ "." ++ s2
    returnNumberOrError s =
      case fmap fst . listToMaybe $ reads s of
        Just r -> return r
        Nothing -> fail $ "Unable to parse as number: " ++ s

lexer :: GenTokenParser String st Identity
lexer = makeTokenParser cssDef

cssDef :: GenLanguageDef String u Identity
cssDef =
  emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = ""
    , nestedComments = False
    , identStart = letter <|> oneOf "_-"
    , identLetter = alphaNum <|> oneOf "_-"
    , caseSensitive = False
    }

parseFile :: FilePath -> IO ()
parseFile file = do
  csscontents <- readFile file
  let css = parse stylesheet file csscontents
  print css
  return ()