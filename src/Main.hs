{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Either
import System.Environment
import System.Exit
import System.FilePath.Glob

import CSS.Parser
import Hints.Model
import Report.Raw
import Report.Text
import qualified Hints.Pseudo as Pseudo

allHints :: [Hint]
allHints = Pseudo.hints

main :: IO ()
main = do
  args <- getArgs
  case args of
    g : [] -> do
      files <- glob g
      results <- mapM processFile files
      let total = sum (rights results) + length (lefts results)
      if total < 1
        then do
          putStrLn "No problems found"
          exitSuccess
        else do
          putStrLn $ show total ++ " problems found"
          exitFailure
    _ -> do
      putStrLn "Expected use: css-conditioner \"file\""
      exitFailure
  where
    processFile :: FilePath -> IO (Either () Int)
    processFile fp = do
      ess <- parseCSSFile fp
      case ess of
        Left msg -> do
          print msg
          return $ Left ()
        Right ss -> do
          csscontents <- readFile fp
          let rawreport = createRawReport ss allHints
              textreport = createTextReport csscontents rawreport
          putStrLn textreport
          return $ Right (length rawreport)