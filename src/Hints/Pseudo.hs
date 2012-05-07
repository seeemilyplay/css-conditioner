module Hints.Pseudo (hints) where

import CSS.Model
import CSS.ParseNode
import CSS.Search
import Hints.Model

hints :: [Hint]
hints =
  [ Hint {
        label = "hints.pseudo.1"
      , categories = [CSS2_1, IE, IE7]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":before pseudo element doesn't work in IE7 or lower"
      , findInstances = findPseudoInstances "before"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.2"
      , categories = [CSS2_1, IE, IE7]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":after pseudo element doesn't work in IE7 or lower"
      , findInstances = findPseudoInstances "after"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.3"
      , categories = [CSS2_1, IE, IE7]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":active pseudo element doesn't work in IE7 or lower"
      , findInstances = findPseudoInstances "active"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.4"
      , categories = [CSS2_1, IE, IE8, IE9, IE10]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":active pseudo element is buggy in IE8-IE10, clicking mouse button on nested elements doesn't trigger it"
      , findInstances = findPseudoInstances "active"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.5"
      , categories = [CSS2_1, IE, IE7, Dynamic]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":first-child pseudo element is buggy in IE8, it doesn't work with dynamically added elements"
      , findInstances = findPseudoInstances "first-child"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.6"
      , categories = [CSS2_1, IE, IE8, Dynamic]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":first-child pseudo element is buggy in IE8, it doesn't work well with focussed elements"
      , findInstances = findPseudoInstances "first-child"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.7"
      , categories = [CSS2_1, Firefox, Safari, Chrome, Opera, Dynamic]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":first-line pseudo element is buggy in Firefox, Safari, Chrome and Opera with dynamically changing text"
      , findInstances = findPseudoInstances "first-line"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.8"
      , categories = [CSS2_1, Firefox, Safari, Chrome, Opera, Dynamic]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":first-letter pseudo element is buggy in Firefox, Safari, Chrome and Opera with dynamically changing text"
      , findInstances = findPseudoInstances "first-letter"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.9"
      , categories = [CSS2_1, IE, IE7]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":focus pseudo element doesn't work in IE7 or lower"
      , findInstances = findPseudoInstances "focus"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.10"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":empty pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "empty"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.11"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":enabled pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "enabled"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.12"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":disabled pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "disabled"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.13"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":checked pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "checked"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.14"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":first-of-type pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "first-of-type"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.15"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":last-child pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "last-child"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.16"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":last-of-type pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "last-of-type"
      , maybeFixInstance = Nothing
    }
  , Hint {
        label = "hints.pseudo.17"
      , categories = [CSS3, IE, IE7, IE8]
      , references = ["http://www.quirksmode.org/css/contents.html"]
      , explanation = ":not pseudo element doesn't work in IE8 or lower"
      , findInstances = findPseudoInstances "not"
      , maybeFixInstance = Nothing
    }
  ]

findPseudoInstances :: String -> Stylesheet -> [ParseData]
findPseudoInstances n ss = map parseData matchingPseudoElements
  where
    matchingPseudoElements :: [PseudoElement]
    matchingPseudoElements = ss `findNamed` n
