module ArgsParser (
    Arguments(..)
  , parseArguments) where

import Control.Applicative ((<$>))
import Data.Functor.Identity
import Data.Maybe (listToMaybe)
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Hints.Model


data Arguments = Arguments {
    includedCategories :: [HintCategory]
  , excludedCategories :: [HintCategory]
  , inputFiles :: [String]
  , helpWanted :: Bool
} deriving (Eq, Show)

parseArguments :: [String] -> Either ParseError Arguments
parseArguments args = do
  let input = unwords args
  toArguments <$> parse arguments input input
  where
    toArguments :: [Argument] -> Arguments
    toArguments xs = Arguments {
      includedCategories = concatMap getIncludes xs
    , excludedCategories = concatMap getExcludes xs
    , inputFiles = concatMap getInputFiles xs
    , helpWanted = any isHelp xs
    }
    getIncludes (Include cs) = cs
    getIncludes _ = []
    getExcludes (Exclude cs) = cs
    getExcludes _ = []
    getInputFiles (InputFile s) = [s]
    getInputFiles _ = []
    isHelp Help = True
    isHelp _ = False

data Argument = Include [HintCategory]
                | Exclude [HintCategory]
                | Help
                | InputFile String
  deriving (Eq, Show)

arguments :: GenParser Char st [Argument]
arguments =
  many1 (choice [ try help
                  <|> (try include
                       <|> exclude)
                , inputFile
                ])

inputFile :: GenParser Char st Argument
inputFile = lexeme lexer (InputFile <$> many1 (noneOf " ")) <?> "filepath"

help :: GenParser Char st Argument
help = do
  _ <- argName "help"
  return Help

include :: GenParser Char st Argument
include = do
  _ <- argName "include"
  Include <$> ArgsParser.categories

exclude :: GenParser Char st Argument
exclude = do
  _ <- argName "exclude"
  Exclude <$> ArgsParser.categories

argName :: String -> GenParser Char st String
argName s = (do
  _ <- string "--"
  lexeme lexer $ string s) <?> ("--" ++ s)

categories :: GenParser Char st [HintCategory]
categories = many1 category <?> "categories"

category :: GenParser Char st HintCategory
category = (do
  s <- identifier lexer
  returnCategoryOrError s) <?> "category"
  where
    returnCategoryOrError s =
      case fmap fst . listToMaybe $ reads s of
        Just r -> return r
        Nothing -> fail $ "Not a valid category: " ++ s


lexer :: GenTokenParser String st Identity
lexer = makeTokenParser haskellDef