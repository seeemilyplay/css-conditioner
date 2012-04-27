module CSS.Model where

import Text.Parsec

data ParseData = ParseData SourcePos SourcePos
  deriving (Eq, Show)

data Stylesheet = Stylesheet (Maybe CharSet) [Item] ParseData
  deriving (Eq, Show)

data CharSet = CharSet StringLiteral ParseData
  deriving (Eq, Show)

data Item =   MediaItem Media ParseData
            | RuleSetItem RuleSet ParseData
  deriving (Eq, Show)

data Media = Media [Identifier] [RuleSet] ParseData
  deriving (Eq, Show)

data RuleSet = RuleSet [Selector] [Declaration] ParseData
  deriving (Eq, Show)

data Selector = Selector [SelectorPart] ParseData
  deriving (Eq, Show)

data SelectorPart =   NearestChildSelectorPart NearestChildSelector ParseData
                    | SiblingSelectorPart SiblingSelector ParseData
                    | SimpleSelectorPart SimpleSelector ParseData
  deriving (Eq, Show)

data NearestChildSelector = NearestChildSelector [SelTerm] ParseData
  deriving (Eq, Show)

data SiblingSelector = SiblingSelector [SelTerm] ParseData
  deriving (Eq, Show)

data SimpleSelector = SimpleSelector [SelTerm] ParseData
  deriving (Eq, Show)

data SelTerm =   ElementSelTerm Element ParseData
               | IdSelTerm Id ParseData
               | ClassSelTerm Class ParseData
               | PseudoElementSelTerm PseudoElement ParseData
               | AttributeSelTerm Attribute ParseData
  deriving (Eq, Show)

data Element =   NameElement Name ParseData
               | WildcardElement Wildcard ParseData
  deriving (Eq, Show)

data Name = Name Identifier ParseData
  deriving (Eq, Show)

data Wildcard = Wildcard ParseData
  deriving (Eq, Show)

data Id = Id Identifier ParseData
  deriving (Eq, Show)

data Class = Class Identifier ParseData
  deriving (Eq, Show)

data PseudoElement = PseudoElement Identifier ParseData
  deriving (Eq, Show)

data Attribute =   AttributeHas HasMatcher ParseData
                 | AttributeEquals EqualsMatcher ParseData
                 | AttributeIncludes IncludesMatcher ParseData
                 | AttributeLike LikeMatcher ParseData
  deriving (Eq, Show)

data HasMatcher = HasMatcher Identifier ParseData
  deriving (Eq, Show)

data EqualsMatcher = EqualsMatcher Identifier StringLiteral ParseData
  deriving (Eq, Show)

data IncludesMatcher = IncludesMatcher Identifier StringLiteral ParseData
  deriving (Eq, Show)

data LikeMatcher = LikeMatcher Identifier StringLiteral ParseData
  deriving (Eq, Show)

data Declaration = Declaration (Maybe DeclarationHack)
                               Property
                               Expression
                               Priority
                               ParseData
  deriving (Eq, Show)

data DeclarationHack =   AsterixDeclarationHack AsterixHack ParseData
                       | HashDeclarationHack HashHack ParseData
                       | UnderscoreDeclarationHack UnderscoreHack ParseData
  deriving (Eq, Show)

data AsterixHack = AsterixHack ParseData
  deriving (Eq, Show)

data HashHack = HashHack ParseData
  deriving (Eq, Show)

data UnderscoreHack = UnderscoreHack ParseData
  deriving (Eq, Show)

data Property = Property Identifier ParseData
  deriving (Eq, Show)

data Priority =   ImportantPriority Important ParseData
                | NormalPriority ParseData
  deriving (Eq, Show)

data Important = Important ParseData
  deriving (Eq, Show)

data Expression = Expression [Term] ParseData
  deriving (Eq, Show)

data Term =   HexcolorLiteralTerm HexcolorLiteral ParseData
            | MeasureTerm Measure ParseData
            | StringLiteralTerm StringLiteral ParseData
            | URITerm URI ParseData
            | OpacityHackTerm OpacityHack ParseData
            | FunctionTerm Function ParseData
            | IdentifierTerm Identifier ParseData
  deriving (Eq, Show)

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

data Function = Function Identifier Expression ParseData
  deriving (Eq, Show)

data Percentage = Percentage NumberLiteral ParseData
  deriving (Eq, Show)

data Centimeter = Centimeter NumberLiteral ParseData
  deriving (Eq, Show)

data Inch = Inch NumberLiteral ParseData
  deriving (Eq, Show)

data Degree = Degree NumberLiteral ParseData
  deriving (Eq, Show)

data Radian = Radian NumberLiteral ParseData
  deriving (Eq, Show)

data Grad = Grad NumberLiteral ParseData
  deriving (Eq, Show)

data Second = Second NumberLiteral ParseData
  deriving (Eq, Show)

data Hertz = Hertz NumberLiteral ParseData
  deriving (Eq, Show)

data Kilohertz = Kilohertz NumberLiteral ParseData
  deriving (Eq, Show)

data Ems = Ems NumberLiteral ParseData
  deriving (Eq, Show)

data Exs = Exs NumberLiteral ParseData
  deriving (Eq, Show)

data Millimeter = Millimeter NumberLiteral ParseData
  deriving (Eq, Show)

data Millisecond = Millisecond NumberLiteral ParseData
  deriving (Eq, Show)

data Pixel = Pixel NumberLiteral ParseData
  deriving (Eq, Show)

data Point = Point NumberLiteral ParseData
  deriving (Eq, Show)

data Pica = Pica NumberLiteral ParseData
  deriving (Eq, Show)

data NumberOnly = NumberOnly NumberLiteral ParseData
  deriving (Eq, Show)

data URI = URI StringLiteral ParseData
  deriving (Eq, Show)

data OpacityHack = OpacityHack NumberLiteral ParseData
  deriving (Eq, Show)

data Identifier = Identifier String ParseData
  deriving (Eq, Show)

data StringLiteral = StringLiteral String ParseData
  deriving (Eq, Show)

data HexcolorLiteral = HexcolorLiteral Integer Integer Integer ParseData
  deriving (Eq, Show)

data NumberLiteral = NumberLiteral Double ParseData
  deriving (Eq, Show)
