{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module CSS.Search (
    find
  , name
  , findNamed) where

import Data.Char

import CSS.Model

findNamed :: (Searchable a b, Named b) => a -> String -> [b]
findNamed x n = filter ((==) (map toLower n) . name) $ find x

findList :: (Searchable a b) => [a] -> [b]
findList = concatMap find

class Searchable a b where
  find :: a -> [b]

class (Searchable b b) => Named b where
  name :: b -> String

instance Searchable Stylesheet [SelTerm] where
  find (Stylesheet _ xs _) = findList xs

instance Searchable Stylesheet Id where
  find (Stylesheet _ xs _) = findList xs

instance Searchable Stylesheet PseudoElement where
  find (Stylesheet _ xs _) = findList xs

instance Searchable Item [SelTerm] where
  find (MediaItem x _) = find x
  find (RuleSetItem x _) = find x

instance Searchable Item Id where
  find (MediaItem x _) = find x
  find (RuleSetItem x _) = find x

instance Searchable Item PseudoElement where
  find (MediaItem x _) = find x
  find (RuleSetItem x _) = find x

instance Searchable Media [SelTerm] where
  find (Media _ xs _) = findList xs

instance Searchable Media Id where
  find (Media _ xs _) = findList xs

instance Searchable Media PseudoElement where
  find (Media _ xs _) = findList xs

instance Searchable RuleSet [SelTerm] where
  find (RuleSet xs _ _) = findList xs

instance Searchable RuleSet Id where
  find (RuleSet xs _ _) = findList xs

instance Searchable RuleSet PseudoElement where
  find (RuleSet xs _ _) = findList xs

instance Searchable Selector [SelTerm] where
  find (Selector xs _) = findList xs

instance Searchable Selector Id where
  find (Selector xs _) = findList xs

instance Searchable Selector PseudoElement where
  find (Selector xs _) = findList xs

instance Searchable SelectorPart [SelTerm] where
  find (NearestChildSelectorPart x _) = find x
  find (SiblingSelectorPart x _) = find x
  find (SimpleSelectorPart x _) = find x

instance Searchable SelectorPart Id where
  find (NearestChildSelectorPart x _) = find x
  find (SiblingSelectorPart x _) = find x
  find (SimpleSelectorPart x _) = find x

instance Searchable SelectorPart PseudoElement where
  find (NearestChildSelectorPart x _) = find x
  find (SiblingSelectorPart x _) = find x
  find (SimpleSelectorPart x _) = find x

instance Searchable NearestChildSelector [SelTerm] where
  find (NearestChildSelector xs _) = [xs]

instance Searchable NearestChildSelector Id where
  find (NearestChildSelector xs _) = findList xs

instance Searchable NearestChildSelector PseudoElement where
  find (NearestChildSelector xs _) = findList xs

instance Searchable SiblingSelector [SelTerm] where
  find (SiblingSelector xs _) = [xs]

instance Searchable SiblingSelector Id where
  find (SiblingSelector xs _) = findList xs

instance Searchable SiblingSelector PseudoElement where
  find (SiblingSelector xs _) = findList xs

instance Searchable SimpleSelector [SelTerm] where
  find (SimpleSelector xs _) = [xs]

instance Searchable SimpleSelector Id where
  find (SimpleSelector xs _) = findList xs

instance Searchable SimpleSelector PseudoElement where
  find (SimpleSelector xs _) = findList xs

instance Searchable SelTerm Id where
  find (IdSelTerm x _) = find x
  find _ = []

instance Searchable SelTerm PseudoElement where
  find (PseudoElementSelTerm x _) = find x
  find _ = []

instance Searchable Id Id where
  find x = [x]

instance Searchable PseudoElement PseudoElement where
  find x = [x]

instance Named Id where
  name (Id (Identifier xs _) _) = xs

instance Named PseudoElement where
  name (PseudoElement (Identifier xs _) _) = map toLower xs
