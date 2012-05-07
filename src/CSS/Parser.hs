{- |
    An incomplete css parser which pretty much follows this
    http://www.w3.org/TR/CSS21/grammar.html.
    It tries not to be too strict, and it doesn't understand CSS3.

    missing stuff:  correct char handling (escape & non-ascii chars)
                    stuff that can go at the top of your stylesheet
                    @font-face stuff (not in spec but hey)
                    attributes expect string literals
                    double :: from css3
                    and for css3
-}
module CSS.Parser (
    parseCSSFile
  , stylesheet
  , charset
  , item
  , media
  , ruleset
  , selectors
  , selector
  , selectorPart
  , nearestChildSelector
  , simpleSelector
  , selTerms
  , anySelTerm
  , nonElementSelTerm
  , element
  , CSS.Parser.id
  , clazz
  , pseudoElement
  , attribute
  , declarations
  , declaration
  , declarationHack
  , property
  , priority
  , important
  , expression
  , term
  , measure
  , function
  , uri
  , opacityHack
  , unlexemedIdentifier
  , lexemedIdentifier
  , CSS.Parser.stringLiteral
  , hexcolorLiteral
  , numberLiteral) where

import Control.Applicative ((<$>))
import Data.Char
import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import CSS.Model

parseCSSFile :: FilePath -> IO (Either ParseError Stylesheet)
parseCSSFile file = do
  csscontents <- readFile file
  let css = parse stylesheet file csscontents
  return css

stylesheet :: GenParser Char st Stylesheet
stylesheet = withParseData $ do
  (whiteSpace lexer)
  mcharset <- optionMaybe charset
  items <- many item
  _ <- eof
  return $ Stylesheet mcharset items

charset :: GenParser Char st CharSet
charset = withParseData (do
  _ <- lexeme lexer $ stringIgnoreCase "@charset "
  cs <- CSS.Parser.stringLiteral
  _ <- semi lexer
  return $ CharSet cs) <?> "@charset"

item :: GenParser Char st Item
item = withParseData $ choice
  [ MediaItem <$> media
  , RuleSetItem <$> ruleset
  ]

media :: GenParser Char st Media
media = withParseData (do
  _ <- lexeme lexer $ stringIgnoreCase "@media "
  m <- lexemedIdentifier
  ms <- many (do
    _ <- comma lexer
    lexemedIdentifier)
  rs <- braces lexer $ many ruleset
  return $ Media (m:ms) rs
  ) <?> "@media"

ruleset :: GenParser Char st RuleSet
ruleset = withParseData (do
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
selector = withParseData (do
  s <- (withParseData $ SimpleSelectorPart <$> simpleSelector)
  ss <- many selectorPart
  return $ Selector (s:ss)) <?> "selector"

selectorPart :: GenParser Char st SelectorPart
selectorPart = withParseData $ choice [
    NearestChildSelectorPart <$> nearestChildSelector
  , SiblingSelectorPart <$> siblingSelector
  , SimpleSelectorPart <$> simpleSelector
  ]

nearestChildSelector :: GenParser Char st NearestChildSelector
nearestChildSelector = withParseData $ do
  _ <- lexeme lexer $ char '>'
  NearestChildSelector <$> selTerms

siblingSelector :: GenParser Char st SiblingSelector
siblingSelector = withParseData $ do
  _ <- lexeme lexer $ char '+'
  SiblingSelector <$> selTerms

simpleSelector :: GenParser Char st SimpleSelector
simpleSelector = withParseData $ SimpleSelector <$> selTerms

selTerms :: GenParser Char st [SelTerm]
selTerms = lexeme lexer $ do
  mt <- optionMaybe anySelTerm
  case mt of
    Nothing -> many1 nonElementSelTerm
    Just t -> do
      ts <- many nonElementSelTerm
      return $ t:ts

anySelTerm :: GenParser Char st SelTerm
anySelTerm =
  withParseData (ElementSelTerm <$> element) <|> nonElementSelTerm

nonElementSelTerm :: GenParser Char st SelTerm
nonElementSelTerm = withParseData $ choice [
    IdSelTerm <$> CSS.Parser.id
  , ClassSelTerm <$> clazz
  , PseudoElementSelTerm <$> pseudoElement
  , AttributeSelTerm <$> attribute
  ]

element :: GenParser Char st Element
element = withParseData (
  (NameElement <$> nameElement) <|> (WildcardElement <$> wildcard)) <?> "element"
  where
    nameElement = withParseData $
      Name <$> unlexemedIdentifier
    wildcard = withParseData $ do
      _ <- char '*'
      return Wildcard

id :: GenParser Char st Id
id = withParseData (do
  _ <- char '#'
  Id <$> unlexemedIdentifier) <?> "#id"

clazz :: GenParser Char st Class
clazz = withParseData (do
  _ <- char '.'
  Class <$> unlexemedIdentifier) <?> ".class"

pseudoElement :: GenParser Char st PseudoElement
pseudoElement = withParseData (do
  _ <- char ':'
  PseudoElement <$> unlexemedIdentifier) <?> ":pseudo-element"

attribute :: GenParser Char st Attribute
attribute = withParseData (brackets lexer $
  try (AttributeLike <$> likeMatcher)
      <|> (try (AttributeIncludes <$> includesMatcher)
      <|> (try (AttributeEquals <$> equalsMatcher)
      <|> (AttributeHas <$> hasMatcher)))) <?> "[attribute]"
  where
    likeMatcher = withParseData $ do
      att <- lexemedIdentifier
      _ <- lexeme lexer $ string "|="
      val <- CSS.Parser.stringLiteral
      return $ LikeMatcher att val
    includesMatcher = withParseData $ do
      att <- lexemedIdentifier
      _ <- lexeme lexer $ string "~="
      val <- CSS.Parser.stringLiteral
      return $ IncludesMatcher att val
    equalsMatcher = withParseData $ do
      att <- lexemedIdentifier
      _ <- lexeme lexer $ char '='
      val <- CSS.Parser.stringLiteral
      return $ EqualsMatcher att val
    hasMatcher = withParseData $
      HasMatcher <$> lexemedIdentifier

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
declaration = withParseData (do
  mhack <- optionMaybe declarationHack
  prop <- property
  _ <- (colon lexer)
  expr <- expression
  prior <- priority
  return $ Declaration mhack prop expr prior) <?> "declaration"

declarationHack :: GenParser Char st DeclarationHack
declarationHack = withParseData (choice
  [ AsterixDeclarationHack <$> asterixHack
  , HashDeclarationHack <$> hashHack
  , UnderscoreDeclarationHack <$> underscoreHack
  ]) <?> ""
  where
    asterixHack = withParseData $ do
      _ <- lexeme lexer $ char '*'
      return AsterixHack
    hashHack = withParseData $ do
      _ <- lexeme lexer $ char '#'
      return HashHack
    underscoreHack = withParseData $ do
      _ <- lexeme lexer $ char '_'
      return UnderscoreHack

property :: GenParser Char st Property
property = withParseData (Property <$> lexemedIdentifier) <?> "property"

priority :: GenParser Char st Priority
priority = withParseData (do
  mimportant <- optionMaybe important
  case mimportant of
    (Just imp) -> return $ ImportantPriority imp
    Nothing -> return NormalPriority) <?> ""

important :: GenParser Char st Important
important = withParseData (do
  _ <- stringIgnoreCase "!important"
  return Important) <?> "!important"

expression :: GenParser Char st Expression
expression = withParseData
  (Expression <$> sepBy term (optional termSeparator)) <?> "expression"
  where
    termSeparator :: GenParser Char st String
    termSeparator = (comma lexer <|> forwardSlash) <?> ""
      where
        forwardSlash = lexeme lexer $ do
          c <- char '/'
          return [c]

term :: GenParser Char st Term
term = withParseData (choice
  [ HexcolorLiteralTerm <$> hexcolorLiteral
  , MeasureTerm <$> measure
  , StringLiteralTerm <$> CSS.Parser.stringLiteral
  , try (URITerm <$> uri)
      <|> (try (OpacityHackTerm <$> opacityHack)
      <|> (try (FunctionTerm <$> function)
      <|> (IdentifierTerm <$> lexemedIdentifier)))
  , try (OpacityHackTerm <$> opacityHack)
      <|> (try (FunctionTerm <$> function)
      <|> (IdentifierTerm <$> lexemedIdentifier))
  , try (FunctionTerm <$> function)
      <|> (IdentifierTerm <$> lexemedIdentifier)
  , IdentifierTerm <$> lexemedIdentifier
  ]) <?> "term"

measure :: GenParser Char st Measure
measure = (do
  startpos <- getPosition
  num <- numberLiteral
  parseUnit startpos num) <?> "measure"
  where
    parseUnit startpos num = choice
      [ unit PercentageMeasure Percentage (char '%')
      , unit CentimeterMeasure Centimeter (stringIgnoreCase "cm")
      , unit InchMeasure Inch (stringIgnoreCase "in")
      , unit DegreeMeasure Degree (stringIgnoreCase "deg")
      , unit RadianMeasure Radian (stringIgnoreCase "rad")
      , unit GradMeasure Grad (stringIgnoreCase "grad")
      , unit SecondMeasure Second (charIgnoreCase 's')
      , unit HertzMeasure Hertz (stringIgnoreCase "hz")
      , unit KilohertzMeasure Kilohertz (stringIgnoreCase "khz")
      , try (unit EmsMeasure Ems (stringIgnoreCase "em"))
        <|> unit ExsMeasure Exs (stringIgnoreCase "ex")
      , try (unit MillimeterMeasure Millimeter (stringIgnoreCase "mm"))
        <|> unit MillisecondMeasure Millisecond (stringIgnoreCase "ms")
      , try (unit PixelMeasure Pixel (stringIgnoreCase "px"))
        <|> (try (unit PointMeasure Point (stringIgnoreCase "pt"))
             <|> unit PicaMeasure Pica (stringIgnoreCase "pc"))
      , unit NumberOnlyMeasure NumberOnly (return ())
      ]
      where
        unit m c p = do
          _ <- lexeme lexer p
          endpos <- getPosition
          let pd = ParseData startpos endpos
          return $ m (c num pd) pd

function :: GenParser Char st Function
function = withParseData (do
  f <- lexemedIdentifier
  args <- parens lexer expression
  return $ Function f args) <?> "function"

uri :: GenParser Char st URI
uri = withParseData (URI <$> do
  _ <- stringIgnoreCase "url"
  parens lexer url) <?> "uri"
  where
    url = CSS.Parser.stringLiteral <|> unquotedString
    unquotedString =
      lexemeWithParseData (StringLiteral <$> manyTill anyChar ending) <?> ""
    ending = lookAhead $ do
      (whiteSpace lexer)
      char ')'

opacityHack :: GenParser Char st OpacityHack
opacityHack = withParseData (OpacityHack <$> do
  _ <- stringIgnoreCase "alpha"
  parens lexer opacity) <?> ""
  where
    opacity = do
      _ <- lexeme lexer $ stringIgnoreCase "opacity"
      _ <- lexeme lexer $ char '='
      numberLiteral

unlexemedIdentifier :: GenParser Char st Identifier
unlexemedIdentifier = withParseData (do
  start <- identStart cssDef
  rest <- many $ identLetter cssDef
  return . Identifier $ start : rest) <?> "identifier"

lexemedIdentifier :: GenParser Char st Identifier
lexemedIdentifier =
  withParseData (Identifier <$> identifier lexer) <?> "identifier"

stringLiteral :: GenParser Char st StringLiteral
stringLiteral = withParseData (do
    s <- singleQuoted <|> doubleQuoted
    return $ StringLiteral s) <?> "string"
  where
    singleQuoted =
      between (char '\'') (char '\'') (many $ noneOf "\n\r\f\\\'")
    doubleQuoted =
      between (char '"') (char '"') (many $ noneOf "\n\r\f\\\"")

hexcolorLiteral :: GenParser Char st HexcolorLiteral
hexcolorLiteral = lexemeWithParseData (do
  _ <- char '#'
  col <- try hexcolorLiteral6digits <|> hexcolorLiteral3digits
  mdig <- lookAhead $ optionMaybe hexdigit
  case mdig of
    Just _ -> fail "3 or 6 digits in hexcolor"
    Nothing -> return col) <?> "hexcolor"

hexcolorLiteral3digits :: GenParser Char st (ParseData -> HexcolorLiteral)
hexcolorLiteral3digits = do
  [r, g, b] <- map base16 <$> count 3 hexdigit
  return $ HexcolorLiteral r g b
  where
    base16 :: Integer -> Integer
    base16 x = x * 16 + x

hexcolorLiteral6digits :: GenParser Char st (ParseData -> HexcolorLiteral)
hexcolorLiteral6digits = do
  [r1, r2, g1, g2, b1, b2] <- count 6 hexdigit
  return $ HexcolorLiteral (base16 r1 r2) (base16 g1 g2) (base16 b1 b2)
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

numberLiteral :: GenParser Char st NumberLiteral
numberLiteral = lexemeWithParseData (do
  power <- sign
  val <- (*) power <$> (try decimalNum <|> wholeNum)
  return $ NumberLiteral val) <?> "number"
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

stringIgnoreCase :: String -> GenParser Char st String
stringIgnoreCase [] = return ""
stringIgnoreCase (c:cs) = do
  x <- charIgnoreCase c
  xs <- stringIgnoreCase cs
  return $ x : xs

charIgnoreCase :: Char -> GenParser Char st Char
charIgnoreCase c =
  char (toUpper c) <|> char (toLower c)

withParseData :: ParsecT String st Identity (ParseData -> b)
                 -> ParsecT String st Identity b
withParseData p = do
  start <- getPosition
  r <- p
  end <- getPosition
  return $ r (ParseData start end)

lexemeWithParseData :: ParsecT String st Identity (ParseData -> b)
                       -> ParsecT String st Identity b
lexemeWithParseData = withParseData . lexeme lexer

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