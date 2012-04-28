module Hints.Model (
    HintCategory(..)
  , Hint(..)) where

import CSS.Model

data HintCategory =   IE
                    | IE7
                    | IE8
                    | IE9
                    | IE10
                    | Firefox
                    | Safari
                    | Chrome
                    | Opera
                    | Dynamic
                    | CSS2_1
                    | CSS3
  deriving (Eq, Show)

data Hint = Hint {
    label :: String
  , categories :: [HintCategory]
  , explanation :: String
  , references :: [String]
  , findInstances :: Stylesheet -> [ParseData]
}
