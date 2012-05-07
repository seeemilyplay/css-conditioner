module Hints.List (getHints) where

import Hints.Model
import qualified Hints.Pseudo as Pseudo
import qualified Hints.Speed as Speed

getHints :: [HintCategory] -> [HintCategory] -> [Hint]
getHints includecats excludecats =
  excludedHints excludecats $ includedHints includecats

excludedHints :: [HintCategory] -> [Hint] -> [Hint]
excludedHints excludecats hints =
  filter (not . null . categories) $ map removeExcludedCategories hints
  where
    removeExcludedCategories :: Hint -> Hint
    removeExcludedCategories hint = hint{
      categories = removeCategories excludecats (categories hint)
    }
    removeCategories :: [HintCategory] -> [HintCategory] -> [HintCategory]
    removeCategories [] cs = cs
    removeCategories exs cs = foldl (flip excludeCategory) cs exs

excludeCategory :: HintCategory -> [HintCategory] -> [HintCategory]
excludeCategory IE cs = remove [IE, IE7, IE8, IE9, IE10] cs
excludeCategory IE7 cs = remove [IE, IE7] cs
excludeCategory IE8 cs = remove [IE, IE8] cs
excludeCategory IE9 cs = remove [IE, IE9] cs
excludeCategory IE10 cs = remove [IE, IE10] cs
excludeCategory Firefox cs = remove [Firefox] cs
excludeCategory Safari cs = remove [Safari] cs
excludeCategory Chrome cs = remove [Chrome] cs
excludeCategory Opera cs = remove [Opera] cs
excludeCategory Dynamic cs | Dynamic `elem` cs = []
excludeCategory Dynamic cs = cs
excludeCategory Speed cs | Speed `elem` cs = []
excludeCategory Speed cs = cs
excludeCategory CSS2_1 cs | CSS2_1 `elem` cs = []
excludeCategory CSS2_1 cs = cs
excludeCategory CSS3 cs | CSS3 `elem` cs = []
excludeCategory CSS3 cs = cs

remove :: Eq a => [a] -> [a] -> [a]
remove xs ys = foldl (\ys' x -> filter (x /=) ys') ys xs

includedHints :: [HintCategory] -> [Hint]
includedHints [] = allHints
includedHints includedcats =
  filter (any (`elem` includedcats) . categories) allHints

allHints :: [Hint]
allHints = Pseudo.hints ++ Speed.hints
