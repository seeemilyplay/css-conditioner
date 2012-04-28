{-# LANGUAGE NamedFieldPuns #-}

module Main where

import CSS.Parser
import Hints.Model
import Report.Raw
import Report.Text
import qualified Hints.Pseudo as Pseudo

allHints :: [Hint]
allHints = Pseudo.hints

runHintsForFile :: FilePath -> IO ()
runHintsForFile fp = do
  ess <- parseCSSFile fp
  case ess of
    Left msg -> do
      print msg
      return ()
    Right ss -> do
      csscontents <- readFile fp
      let rawreport = createRawReport ss allHints
          textreport = createTextReport csscontents rawreport
      putStrLn textreport
      return ()
