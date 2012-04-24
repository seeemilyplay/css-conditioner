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

data ParseData = ParseData SourcePos SourcePos
  deriving (Eq, Show)

class (Eq a, Show a) => ParseNode a where
  parseData :: a -> ParseData

data Stylesheet = Stylesheet (Maybe CharSet) [Item] ParseData
  deriving (Eq, Show)

instance ParseNode Stylesheet where
  parseData (Stylesheet _ _ pd) = pd

data CharSet = CharSet StringLiteral ParseData
  deriving (Eq, Show)

instance ParseNode CharSet where
  parseData (CharSet _ pd) = pd

data Item =   MediaItem Media ParseData
            | RuleSetItem RuleSet ParseData
  deriving (Eq, Show)

instance ParseNode Item where
  parseData (MediaItem _ pd) = pd
  parseData (RuleSetItem _ pd) = pd

data Media = Media [Identifier] [RuleSet] ParseData
  deriving (Eq, Show)

instance ParseNode Media where
  parseData (Media _ _ pd) = pd

data RuleSet = RuleSet [Selector] [Declaration] ParseData
  deriving (Eq, Show)

instance ParseNode RuleSet where
  parseData (RuleSet _ _ pd) = pd

data Selector = Selector [SelectorPart] ParseData
  deriving (Eq, Show)

instance ParseNode Selector where
  parseData (Selector _ pd) = pd

data SelectorPart =   NearestChildSelectorPart NearestChildSelector ParseData
                    | SiblingSelectorPart SiblingSelector ParseData
                    | SimpleSelectorPart SimpleSelector ParseData
  deriving (Eq, Show)

instance ParseNode SelectorPart where
  parseData (NearestChildSelectorPart _ pd) = pd
  parseData (SiblingSelectorPart _ pd) = pd
  parseData (SimpleSelectorPart _ pd) = pd

data NearestChildSelector = NearestChildSelector [SelTerm] ParseData
  deriving (Eq, Show)

instance ParseNode NearestChildSelector where
  parseData (NearestChildSelector _ pd) = pd

data SiblingSelector = SiblingSelector [SelTerm] ParseData
  deriving (Eq, Show)

instance ParseNode SiblingSelector where
  parseData (SiblingSelector _ pd) = pd

data SimpleSelector = SimpleSelector [SelTerm] ParseData
  deriving (Eq, Show)

instance ParseNode SimpleSelector where
  parseData (SimpleSelector _ pd) = pd

data SelTerm =   ElementSelTerm Element ParseData
               | IdSelTerm Id ParseData
               | ClassSelTerm Class ParseData
               | PseudoElementSelTerm PseudoElement ParseData
               | AttributeSelTerm Attribute ParseData
  deriving (Eq, Show)

instance ParseNode SelTerm where
  parseData (ElementSelTerm _ pd) = pd
  parseData (IdSelTerm _ pd) = pd
  parseData (ClassSelTerm _ pd) = pd
  parseData (PseudoElementSelTerm _ pd) = pd
  parseData (AttributeSelTerm _ pd) = pd

data Element =   NameElement Name ParseData
               | WildcardElement Wildcard ParseData
  deriving (Eq, Show)

instance ParseNode Element where
  parseData (NameElement _ pd) = pd
  parseData (WildcardElement _ pd) = pd

data Name = Name Identifier ParseData
  deriving (Eq, Show)

instance ParseNode Name where
  parseData (Name _ pd) = pd

data Wildcard = Wildcard ParseData
  deriving (Eq, Show)

instance ParseNode Wildcard where
  parseData (Wildcard pd) = pd

data Id = Id Identifier ParseData
  deriving (Eq, Show)

instance ParseNode Id where
  parseData (Id _ pd) = pd

data Class = Class Identifier ParseData
  deriving (Eq, Show)

instance ParseNode Class where
  parseData (Class _ pd) = pd

data PseudoElement = PseudoElement Identifier ParseData
  deriving (Eq, Show)

instance ParseNode PseudoElement where
  parseData (PseudoElement _ pd) = pd

data Attribute =   AttributeHas HasMatcher ParseData
                 | AttributeEquals EqualsMatcher ParseData
                 | AttributeIncludes IncludesMatcher ParseData
                 | AttributeLike LikeMatcher ParseData
  deriving (Eq, Show)

instance ParseNode Attribute where
  parseData (AttributeHas _ pd) = pd
  parseData (AttributeEquals _ pd) = pd
  parseData (AttributeIncludes _ pd) = pd
  parseData (AttributeLike _ pd) = pd

data HasMatcher = HasMatcher Identifier ParseData
  deriving (Eq, Show)

instance ParseNode HasMatcher where
  parseData (HasMatcher _ pd) = pd

data EqualsMatcher = EqualsMatcher Identifier StringLiteral ParseData
  deriving (Eq, Show)

instance ParseNode EqualsMatcher where
  parseData (EqualsMatcher _ _ pd) = pd

data IncludesMatcher = IncludesMatcher Identifier StringLiteral ParseData
  deriving (Eq, Show)

instance ParseNode IncludesMatcher where
  parseData (IncludesMatcher _ _ pd) = pd

data LikeMatcher = LikeMatcher Identifier StringLiteral ParseData
  deriving (Eq, Show)

instance ParseNode LikeMatcher where
  parseData (LikeMatcher _ _ pd) = pd

data Declaration = Declaration (Maybe DeclarationHack)
                               Property
                               Expression
                               Priority
                               ParseData
  deriving (Eq, Show)

instance ParseNode Declaration where
  parseData (Declaration _ _ _ _ pd) = pd

data DeclarationHack =   AsterixDeclarationHack AsterixHack ParseData
                       | HashDeclarationHack HashHack ParseData
                       | UnderscoreDeclarationHack UnderscoreHack ParseData
  deriving (Eq, Show)

instance ParseNode DeclarationHack where
  parseData (AsterixDeclarationHack _ pd) = pd
  parseData (HashDeclarationHack _ pd) = pd
  parseData (UnderscoreDeclarationHack _ pd) = pd

data AsterixHack = AsterixHack ParseData
  deriving (Eq, Show)

instance ParseNode AsterixHack where
  parseData (AsterixHack pd) = pd

data HashHack = HashHack ParseData
  deriving (Eq, Show)

instance ParseNode HashHack where
  parseData (HashHack pd) = pd

data UnderscoreHack = UnderscoreHack ParseData
  deriving (Eq, Show)

instance ParseNode UnderscoreHack where
  parseData (UnderscoreHack pd) = pd

data Property = Property Identifier ParseData
  deriving (Eq, Show)

instance ParseNode Property where
  parseData (Property _ pd) = pd

data Priority =   ImportantPriority Important ParseData
                | NormalPriority ParseData
  deriving (Eq, Show)

instance ParseNode Priority where
  parseData (ImportantPriority _ pd) = pd
  parseData (NormalPriority pd) = pd

data Important = Important ParseData
  deriving (Eq, Show)

instance ParseNode Important where
  parseData (Important pd) = pd

data Expression = Expression [Term] ParseData
  deriving (Eq, Show)

instance ParseNode Expression where
  parseData (Expression _ pd) = pd

data Term =   HexcolorLiteralTerm HexcolorLiteral ParseData
            | MeasureTerm Measure ParseData
            | StringLiteralTerm StringLiteral ParseData
            | URITerm URI ParseData
            | OpacityHackTerm OpacityHack ParseData
            | FunctionTerm Function ParseData
            | IdentifierTerm Identifier ParseData
  deriving (Eq, Show)

instance ParseNode Term where
  parseData (HexcolorLiteralTerm _ pd) = pd
  parseData (MeasureTerm _ pd) = pd
  parseData (StringLiteralTerm _ pd) = pd
  parseData (URITerm _ pd) = pd
  parseData (OpacityHackTerm _ pd) = pd
  parseData (FunctionTerm _ pd) = pd
  parseData (IdentifierTerm _ pd) = pd

data Measure =   PercentageMeasure Percentage ParseData
               | CentimeterMeasure Centimeter ParseData
               | InchMeasure Inch ParseData
               | DegreeMeasure Degree ParseData
               | RadianMeasure Radian ParseData
               | GradMeasure Grad ParseData
               | SecondMeasure Second ParseData
               | HertzMeasure Hertz ParseData
               | KilohertzMeasure Kilohertz ParseData
               | EmsMeasure Ems ParseData
               | ExsMeasure Exs ParseData
               | MillimeterMeasure Millimeter ParseData
               | MillisecondMeasure Millisecond ParseData
               | PixelMeasure Pixel ParseData
               | PointMeasure Point ParseData
               | PicaMeasure Pica ParseData
               | NumberOnlyMeasure NumberOnly ParseData
  deriving (Eq, Show)

instance ParseNode Measure where
  parseData (PercentageMeasure _ pd) = pd
  parseData (CentimeterMeasure _ pd) = pd
  parseData (InchMeasure _ pd) = pd
  parseData (DegreeMeasure _ pd) = pd
  parseData (RadianMeasure _ pd) = pd
  parseData (GradMeasure _ pd) = pd
  parseData (SecondMeasure _ pd) = pd
  parseData (HertzMeasure _ pd) = pd
  parseData (KilohertzMeasure _ pd) = pd
  parseData (EmsMeasure _ pd) = pd
  parseData (ExsMeasure _ pd) = pd
  parseData (MillimeterMeasure _ pd) = pd
  parseData (MillisecondMeasure _ pd) = pd
  parseData (PixelMeasure _ pd) = pd
  parseData (PointMeasure _ pd) = pd
  parseData (PicaMeasure _ pd) = pd
  parseData (NumberOnlyMeasure _ pd) = pd

data Function = Function Identifier Expression ParseData
  deriving (Eq, Show)

instance ParseNode Function where
  parseData (Function _ _ pd) = pd

data Percentage = Percentage NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Percentage where
  parseData (Percentage _ pd) = pd

data Centimeter = Centimeter NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Centimeter where
  parseData (Centimeter _ pd) = pd

data Inch = Inch NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Inch where
  parseData (Inch _ pd) = pd

data Degree = Degree NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Degree where
  parseData (Degree _ pd) = pd

data Radian = Radian NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Radian where
  parseData (Radian _ pd) = pd

data Grad = Grad NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Grad where
  parseData (Grad _ pd) = pd

data Second = Second NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Second where
  parseData (Second _ pd) = pd

data Hertz = Hertz NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Hertz where
  parseData (Hertz _ pd) = pd

data Kilohertz = Kilohertz NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Kilohertz where
  parseData (Kilohertz _ pd) = pd

data Ems = Ems NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Ems where
  parseData (Ems _ pd) = pd

data Exs = Exs NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Exs where
  parseData (Exs _ pd) = pd

data Millimeter = Millimeter NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Millimeter where
  parseData (Millimeter _ pd) = pd

data Millisecond = Millisecond NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Millisecond where
  parseData (Millisecond _ pd) = pd

data Pixel = Pixel NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Pixel where
  parseData (Pixel _ pd) = pd

data Point = Point NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Point where
  parseData (Point _ pd) = pd

data Pica = Pica NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode Pica where
  parseData (Pica _ pd) = pd

data NumberOnly = NumberOnly NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode NumberOnly where
  parseData (NumberOnly _ pd) = pd

data URI = URI StringLiteral ParseData
  deriving (Eq, Show)

instance ParseNode URI where
  parseData (URI _ pd) = pd

data OpacityHack = OpacityHack NumberLiteral ParseData
  deriving (Eq, Show)

instance ParseNode OpacityHack where
  parseData (OpacityHack _ pd) = pd

data Identifier = Identifier String ParseData
  deriving (Eq, Show)

instance ParseNode Identifier where
  parseData (Identifier _ pd) = pd

data StringLiteral = StringLiteral String ParseData
  deriving (Eq, Show)

instance ParseNode StringLiteral where
  parseData (StringLiteral _ pd) = pd

data HexcolorLiteral = HexcolorLiteral Integer Integer Integer ParseData
  deriving (Eq, Show)

instance ParseNode HexcolorLiteral where
  parseData (HexcolorLiteral _ _ _ pd) = pd

data NumberLiteral = NumberLiteral Double ParseData
  deriving (Eq, Show)

instance ParseNode NumberLiteral where
  parseData (NumberLiteral _ pd) = pd

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
  cs <- CSSParser.stringLiteral
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
  (withParseData $ ElementSelTerm <$> element) <|> nonElementSelTerm

nonElementSelTerm :: GenParser Char st SelTerm
nonElementSelTerm = withParseData $ choice [
    IdSelTerm <$> CSSParser.id
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
attribute = withParseData (brackets lexer $ choice
  [ AttributeLike <$> likeMatcher
  , AttributeIncludes <$> includesMatcher
  , AttributeEquals <$> equalsMatcher
  , AttributeHas <$> hasMatcher
  ]) <?> "[attribute]"
  where
    likeMatcher = withParseData $ do
      att <- lexemedIdentifier
      _ <- lexeme lexer $ string "|="
      val <- CSSParser.stringLiteral
      return $ LikeMatcher att val
    includesMatcher = withParseData $ do
      att <- lexemedIdentifier
      _ <- lexeme lexer $ string "~="
      val <- CSSParser.stringLiteral
      return $ IncludesMatcher att val
    equalsMatcher = withParseData $ do
      att <- lexemedIdentifier
      _ <- lexeme lexer $ char '='
      val <- CSSParser.stringLiteral
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
  , StringLiteralTerm <$> CSSParser.stringLiteral
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
        <|> (unit ExsMeasure Exs (stringIgnoreCase "ex"))
      , try (unit MillimeterMeasure Millimeter (stringIgnoreCase "mm"))
        <|> (unit MillisecondMeasure Millisecond (stringIgnoreCase "ms"))
      , try (unit PixelMeasure Pixel (stringIgnoreCase "px"))
        <|> (try (unit PointMeasure Point (stringIgnoreCase "pt"))
             <|> (unit PicaMeasure Pica (stringIgnoreCase "pc")))
      , unit NumberOnlyMeasure NumberOnly (return ())
      ]
      where
        unit m c p = do
          _ <- lexeme lexer $ p
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
    url = CSSParser.stringLiteral <|> unquotedString
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