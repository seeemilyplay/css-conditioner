module Hints.Pseudo (hints) where

import CSS.Model
import CSS.ParseNode
import CSS.Search
import Hints.Model

hints :: [Hint]
hints =
  [ Hint {
        categories = [IE6, IE7]
      , explanation = ":before pseudo element not implemented in IE7 or lower"
      , findInstances = findPseudoInstances "before"
    }
  , Hint {
        categories = [IE6, IE7]
      , explanation = ":after pseudo element not implemented in IE7 or lower"
      , findInstances = findPseudoInstances "after"
    }
  , Hint {
        categories = [IE6, IE7]
      , explanation = ":focus pseudo element not implemented in IE7 or lower"
      , findInstances = findPseudoInstances "focus"
    }
  ]

findPseudoInstances :: String -> Stylesheet -> [ParseData]
findPseudoInstances n ss = map parseData matchingPseudoElements
  where
    matchingPseudoElements :: [PseudoElement]
    matchingPseudoElements = ss `findNamed` n
