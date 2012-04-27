module CSS.ParseNode (parseData) where

import CSS.Model

class (Eq a, Show a) => ParseNode a where
  parseData :: a -> ParseData

instance ParseNode Stylesheet where
  parseData (Stylesheet _ _ pd) = pd

instance ParseNode CharSet where
  parseData (CharSet _ pd) = pd

instance ParseNode Item where
  parseData (MediaItem _ pd) = pd
  parseData (RuleSetItem _ pd) = pd

instance ParseNode Media where
  parseData (Media _ _ pd) = pd

instance ParseNode RuleSet where
  parseData (RuleSet _ _ pd) = pd

instance ParseNode Selector where
  parseData (Selector _ pd) = pd

instance ParseNode SelectorPart where
  parseData (NearestChildSelectorPart _ pd) = pd
  parseData (SiblingSelectorPart _ pd) = pd
  parseData (SimpleSelectorPart _ pd) = pd

instance ParseNode NearestChildSelector where
  parseData (NearestChildSelector _ pd) = pd

instance ParseNode SiblingSelector where
  parseData (SiblingSelector _ pd) = pd

instance ParseNode SimpleSelector where
  parseData (SimpleSelector _ pd) = pd

instance ParseNode SelTerm where
  parseData (ElementSelTerm _ pd) = pd
  parseData (IdSelTerm _ pd) = pd
  parseData (ClassSelTerm _ pd) = pd
  parseData (PseudoElementSelTerm _ pd) = pd
  parseData (AttributeSelTerm _ pd) = pd

instance ParseNode Element where
  parseData (NameElement _ pd) = pd
  parseData (WildcardElement _ pd) = pd

instance ParseNode Name where
  parseData (Name _ pd) = pd

instance ParseNode Wildcard where
  parseData (Wildcard pd) = pd

instance ParseNode Id where
  parseData (Id _ pd) = pd

instance ParseNode Class where
  parseData (Class _ pd) = pd

instance ParseNode PseudoElement where
  parseData (PseudoElement _ pd) = pd

instance ParseNode Attribute where
  parseData (AttributeHas _ pd) = pd
  parseData (AttributeEquals _ pd) = pd
  parseData (AttributeIncludes _ pd) = pd
  parseData (AttributeLike _ pd) = pd

instance ParseNode HasMatcher where
  parseData (HasMatcher _ pd) = pd

instance ParseNode EqualsMatcher where
  parseData (EqualsMatcher _ _ pd) = pd

instance ParseNode IncludesMatcher where
  parseData (IncludesMatcher _ _ pd) = pd

instance ParseNode LikeMatcher where
  parseData (LikeMatcher _ _ pd) = pd

instance ParseNode Declaration where
  parseData (Declaration _ _ _ _ pd) = pd

instance ParseNode DeclarationHack where
  parseData (AsterixDeclarationHack _ pd) = pd
  parseData (HashDeclarationHack _ pd) = pd
  parseData (UnderscoreDeclarationHack _ pd) = pd

instance ParseNode AsterixHack where
  parseData (AsterixHack pd) = pd

instance ParseNode HashHack where
  parseData (HashHack pd) = pd

instance ParseNode UnderscoreHack where
  parseData (UnderscoreHack pd) = pd

instance ParseNode Property where
  parseData (Property _ pd) = pd

instance ParseNode Priority where
  parseData (ImportantPriority _ pd) = pd
  parseData (NormalPriority pd) = pd

instance ParseNode Important where
  parseData (Important pd) = pd

instance ParseNode Expression where
  parseData (Expression _ pd) = pd

instance ParseNode Term where
  parseData (HexcolorLiteralTerm _ pd) = pd
  parseData (MeasureTerm _ pd) = pd
  parseData (StringLiteralTerm _ pd) = pd
  parseData (URITerm _ pd) = pd
  parseData (OpacityHackTerm _ pd) = pd
  parseData (FunctionTerm _ pd) = pd
  parseData (IdentifierTerm _ pd) = pd

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

instance ParseNode Function where
  parseData (Function _ _ pd) = pd

instance ParseNode Percentage where
  parseData (Percentage _ pd) = pd

instance ParseNode Centimeter where
  parseData (Centimeter _ pd) = pd

instance ParseNode Inch where
  parseData (Inch _ pd) = pd

instance ParseNode Degree where
  parseData (Degree _ pd) = pd

instance ParseNode Radian where
  parseData (Radian _ pd) = pd

instance ParseNode Grad where
  parseData (Grad _ pd) = pd

instance ParseNode Second where
  parseData (Second _ pd) = pd

instance ParseNode Hertz where
  parseData (Hertz _ pd) = pd

instance ParseNode Kilohertz where
  parseData (Kilohertz _ pd) = pd

instance ParseNode Ems where
  parseData (Ems _ pd) = pd

instance ParseNode Exs where
  parseData (Exs _ pd) = pd

instance ParseNode Millimeter where
  parseData (Millimeter _ pd) = pd

instance ParseNode Millisecond where
  parseData (Millisecond _ pd) = pd

instance ParseNode Pixel where
  parseData (Pixel _ pd) = pd

instance ParseNode Point where
  parseData (Point _ pd) = pd

instance ParseNode Pica where
  parseData (Pica _ pd) = pd

instance ParseNode NumberOnly where
  parseData (NumberOnly _ pd) = pd

instance ParseNode URI where
  parseData (URI _ pd) = pd

instance ParseNode OpacityHack where
  parseData (OpacityHack _ pd) = pd

instance ParseNode Identifier where
  parseData (Identifier _ pd) = pd

instance ParseNode StringLiteral where
  parseData (StringLiteral _ pd) = pd

instance ParseNode HexcolorLiteral where
  parseData (HexcolorLiteral _ _ _ pd) = pd

instance ParseNode NumberLiteral where
  parseData (NumberLiteral _ pd) = pd
