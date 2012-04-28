{-# LANGUAGE NamedFieldPuns #-}

module Report.Text (
  createTextReport) where

import Data.List
import Data.Ord
import System.FilePath
import Text.Parsec

import CSS.Model
import Report.Raw

createTextReport :: String -> [ReportItem] -> String
createTextReport original items =
  unlines . map formatItem $ sortBy positionOrdering items
  where
    formatItem ReportItem{categories, explanation, parseData} =
      "\n" ++ explanation ++ "\n"
        ++ formatLine parseData ++ "\n"
        ++ formatCategories categories
    formatCategories cats = "Categories: " ++ show cats
    formatLine (ParseData pos _) =
      let name = sourceName pos
          line = sourceLine pos
          text = lines original !! (line - 1) in
      takeFileName name ++ " line " ++ show line ++ ": " ++ text
    positionOrdering item1 item2 =
      let (ParseData pos1 _) = parseData item1
          (ParseData pos2 _) = parseData item2 in
      case comparing sourceLine pos1 pos2 of
        EQ -> comparing sourceColumn pos1 pos2
        x -> x
