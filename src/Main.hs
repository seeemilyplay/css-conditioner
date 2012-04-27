{-# LANGUAGE NamedFieldPuns #-}

module Main where

import CSS.Model
import CSS.Parser
import Hints.Model
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
    Right ss -> mapM_ (runHint ss) allHints

runHint :: Stylesheet -> Hint -> IO ()
runHint ss Hint{explanation, findInstances} =
  case findInstances ss of
    [] -> return ()
    instances -> do
      putStrLn explanation
      putStrLn $ show (length instances) ++ " problems found"
      print instances
      return ()
