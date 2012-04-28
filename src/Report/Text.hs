{-# LANGUAGE NamedFieldPuns #-}

module Report.Text (
  createTextReport) where

import System.FilePath
import Text.Parsec

import CSS.Model
import Report.Raw

createTextReport :: String -> [ReportItem] -> String
createTextReport original items = unlines $ map formatItem items
  where
    formatItem ReportItem{categories, explanation, parseData} =
      "\n" ++ explanation ++ "\n"
        ++ (formatLine parseData) ++ "\n"
        ++ formatCategories categories
    formatCategories cats = "Categories: " ++ show cats
    formatLine (ParseData pos _) =
      let name = sourceName pos
          line = sourceLine pos
          text = lines original !! (line - 1) in
      (takeFileName name) ++ " line " ++ show line ++ ": " ++ text

