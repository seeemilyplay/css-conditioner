{-# OPTIONS -XMultiParamTypeClasses #-}

module CSSSearch where

import Data.Char

import CSSParser


class Searchable a b where
  find :: (a -> Bool) -> b -> [a]

instance Searchable PseudoElement PseudoElement where
  find p el | p el = [el]
  find _ _ = []

instance Searchable PseudoElement SelTerm where
  find p (PseudoElementSelTerm el _) = find p el
  find _ _ = []

instance Searchable PseudoElement NearestChildSelector where
  find p (NearestChildSelector ss _) = concatMap (find p) ss

instance Searchable PseudoElement SiblingSelector where
  find p (SiblingSelector ss _) = concatMap (find p) ss

instance Searchable PseudoElement SimpleSelector where
  find p (SimpleSelector ss _) = concatMap (find p) ss

instance Searchable PseudoElement SelectorPart where
  find p (NearestChildSelectorPart el _) = find p el
  find p (SiblingSelectorPart el _) = find p el
  find p (SimpleSelectorPart el _) = find p el

instance Searchable PseudoElement Selector where
  find p (Selector ss _) = concatMap (find p) ss

instance Searchable PseudoElement RuleSet where
  find p (RuleSet ss _ _) = concatMap (find p) ss

instance Searchable PseudoElement Media where
  find p (Media _ rs _) = concatMap (find p) rs

instance Searchable PseudoElement Item where
  find p (MediaItem el _) = find p el
  find p (RuleSetItem el _) = find p el

instance Searchable PseudoElement Stylesheet where
  find p (Stylesheet _ is _) = concatMap (find p) is

tryout fp = do
  res <- parseCSSFile fp
  case res of
    Left msg -> return $ Left msg
    Right ss -> return $ Right $ map parseData $ find (nameIs "hover") ss
  where
    nameIs n (PseudoElement (Identifier name _) _) | (map toLower n) == (map toLower name) = True
    nameIs _ _ = False