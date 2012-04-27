{-# OPTIONS -XMultiParamTypeClasses #-}

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

instance Searchable Stylesheet PseudoElement where
  find (Stylesheet _ xs _) = findList xs

instance Searchable Item PseudoElement where
  find (MediaItem x _) = find x
  find (RuleSetItem x _) = find x

instance Searchable Media PseudoElement where
  find (Media _ xs _) = findList xs

instance Searchable RuleSet PseudoElement where
  find (RuleSet xs _ _) = findList xs

instance Searchable Selector PseudoElement where
  find (Selector xs _) = findList xs

instance Searchable SelectorPart PseudoElement where
  find (NearestChildSelectorPart x _) = find x
  find (SiblingSelectorPart x _) = find x
  find (SimpleSelectorPart x _) = find x

instance Searchable NearestChildSelector PseudoElement where
  find (NearestChildSelector xs _) = findList xs

instance Searchable SiblingSelector PseudoElement where
  find (SiblingSelector xs _) = findList xs

instance Searchable SimpleSelector PseudoElement where
  find (SimpleSelector xs _) = findList xs

instance Searchable SelTerm PseudoElement where
  find (PseudoElementSelTerm x _) = find x
  find _ = []

instance Searchable PseudoElement PseudoElement where
  find x = [x]

instance Named PseudoElement where
  name (PseudoElement (Identifier xs _) _) = map toLower xs
