module Hints.Model (
    HintCategory(..)
  , Hint(..)) where

import CSS.Model

data HintCategory =   IE6
                    | IE7

data Hint = Hint {
    categories :: [HintCategory]
  , explanation :: String
  , findInstances :: Stylesheet -> [ParseData]
}
