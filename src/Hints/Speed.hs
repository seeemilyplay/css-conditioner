module Hints.Speed (hints) where

import CSS.Model
import CSS.ParseNode
import CSS.Search
import Hints.Model

hints :: [Hint]
hints =
  [ Hint {
        label = "hints.speed.1"
      , categories = [Speed]
      , references = []
      , explanation = "unecessary selectors in addition to id selector"
      , findInstances = findIdsNotUsedFirst
      , maybeFixInstance = Just removeSelectorsBeforeId
    }
  ]

findIdsNotUsedFirst :: Stylesheet -> [ParseData]
findIdsNotUsedFirst = map parseData . concatMap nonFirstIds . find
  where
    nonFirstIds :: [SelTerm] -> [Id]
    nonFirstIds [] = []
    nonFirstIds (_:xs) = concatMap find xs

removeSelectorsBeforeId :: Stylesheet -> String -> ParseData -> String
removeSelectorsBeforeId _ss content _pd = content

