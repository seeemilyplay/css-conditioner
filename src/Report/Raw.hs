{-# LANGUAGE NamedFieldPuns #-}

module Report.Raw (
    ReportItem(..)
  , createRawReport) where

import CSS.Model
import Hints.Model

data ReportItem = ReportItem {
    label :: String
  , categories :: [HintCategory]
  , explanation :: String
  , references :: [String]
  , parseData :: ParseData
}

createRawReport :: Stylesheet -> [Hint] -> [ReportItem]
createRawReport ss hints = concatMap forHint hints
  where
    forHint :: Hint -> [ReportItem]
    forHint hint =
      map toItem $ findInstances hint ss
      where
        toItem parseData = ReportItem {
          Report.Raw.label = Hints.Model.label hint
        , Report.Raw.categories = Hints.Model.categories hint
        , Report.Raw.explanation = Hints.Model.explanation hint
        , Report.Raw.references = Hints.Model.references hint
        , Report.Raw.parseData = parseData
        }
