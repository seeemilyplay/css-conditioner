{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative ((<$>))
import Data.Either
import System.Environment
import System.Exit
import System.FilePath.Glob

import ArgsParser
import CSS.Parser
import Hints.List
import Hints.Model
import Report.Raw
import Report.Text

main :: IO ()
main = do
  args <- getArgs
  case parseArguments args of
    (Left err) -> do
      print err
      exitFailure
    (Right Arguments{helpWanted}) | helpWanted -> do
      putStrLn "Usage: css-conditioner filepath"
      exitSuccess
    (Right Arguments{
             includedCategories,
             excludedCategories,
             inputFiles}) -> do
      files <- concat <$> mapM glob inputFiles
      let hints = getHints includedCategories excludedCategories
      putStrLn $ "Running for " ++ show (length files) ++ " files and " ++ show (length hints) ++ " hints..."
      results <- mapM (processFile hints) files
      let total = sum (rights results) + length (lefts results)
      if total < 1
        then do
          putStrLn "No problems found"
          exitSuccess
        else do
          putStrLn $ show total ++ " problems found"
          exitFailure
  where
    processFile :: [Hint] -> FilePath -> IO (Either () Int)
    processFile hints fp = do
      ess <- parseCSSFile fp
      case ess of
        Left msg -> do
          print msg
          return $ Left ()
        Right ss -> do
          csscontents <- readFile fp
          let rawreport = createRawReport ss hints
              textreport = createTextReport csscontents rawreport
          if (not $ null textreport)
            then putStrLn textreport
            else return ()
          return $ Right (length rawreport)
