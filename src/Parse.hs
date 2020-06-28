{-# LANGUAGE ApplicativeDo #-}

module Parse (findImports) where

import           Control.Applicative          ((<|>))
import           Data.Char                    (isSpace)
import           Data.Function                ((&))
import           Data.Maybe                   (fromMaybe, mapMaybe)
import           Text.ParserCombinators.ReadP

data Lexeme
    = SingleLineComment String
    | MultiLineComment String
    | ImportSideEffect String
    | StringLiteral String
    | Other
    deriving Show

singleQuote = '\''
doubleQuote = '"'

singleLineComment :: ReadP Lexeme
singleLineComment = SingleLineComment <$> do string "//"; munch (/= '\n')

multiLineComment :: ReadP Lexeme
multiLineComment = MultiLineComment <$> do string "/*"; manyTill get (string "*/")

importSideEffect :: ReadP Lexeme
importSideEffect = ImportSideEffect <$> do
    string "import"
    munch1 isSpace
    stringLiteral

stringLiteral :: ReadP String
stringLiteral = bracket singleQuote <|> bracket doubleQuote

bracket :: Char -> ReadP String
bracket ch = between (char ch) (char ch) (munch (/= ch))

other :: ReadP Lexeme
other = get *> return Other

lexeme :: ReadP [Lexeme]
lexeme = many $ (singleLineComment <++ multiLineComment <++ importSideEffect <++ (StringLiteral <$> stringLiteral) <++ other)

extractImport :: Lexeme -> Maybe String
extractImport (ImportSideEffect i) = Just i
extractImport _                    = Nothing

findImports :: String -> [String]
findImports contents =
      readP_to_S lexeme contents
    & maybeLast -- Because ReadP isn't greedy we have to go find the greediest parsing ourselves
    & fmap fst
    & fromMaybe []
    & mapMaybe extractImport
  where
    maybeLast ls = if length ls == 0 then Nothing else Just (last ls)
