{-# OPTIONS -XMultiParamTypeClasses #-}

module Hints.PseudoElements where

import Text.Parsec

import CSS.Model
import CSS.Parser
import CSS.Search

tryout :: FilePath -> IO (Either ParseError [PseudoElement])
tryout fp = do
  ecss <- parseCSSFile fp
  case ecss of
    Left msg -> return $ Left msg
    Right stylesheet ->
      return .Right $ stylesheet `findNamed` "hover"
