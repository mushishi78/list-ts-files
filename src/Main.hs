{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Streamly.Data.Fold         as FL
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array      as A
import qualified Streamly.Prelude           as S
import qualified System.IO                  as FH

import           Control.Arrow              ((>>>))
import           Control.Monad.Identity
import           Data.Char                  (ord)
import           Data.Function              ((&))
import           Data.List                  (find, intercalate, isPrefixOf)
import           Data.Maybe                 (fromMaybe)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Parse                      (findImports)
import           Streamly
import           System.Environment         (getArgs)
import           System.FilePath
import           System.IO                  (IOMode (..), openFile, stdout)

data State = State
    { allfiles         :: Set FilePath
    , readFiles        :: Set FilePath
    , externalPackages :: Set String
    }

instance Show State where
    show (State all read packages) = intercalate "\n"
        [ "all: " ++ showSet all
        , "read: " ++ showSet read
        , "packages: " ++ showSet packages
        ]
      where
        showSet =
          Set.toList
            >>> fmap ((++) "\n    ")
            >>> intercalate ""

initialState :: FilePath -> State
initialState entry = State
    { allfiles = Set.singleton entry
    , readFiles = Set.empty
    , externalPackages = Set.empty
    }

isRead :: State -> FilePath -> Bool
isRead state filePath = Set.member filePath (readFiles state)

getUnreadFiles :: State -> [FilePath]
getUnreadFiles state =
      allfiles state
    & Set.toList
    & filter (not . (isRead state))

parseFile :: String -> IO [FilePath]
parseFile filePath =
      readFile filePath
    & fmap findImports

insertImports :: FilePath -> [FilePath] -> State -> State
insertImports filePath imports s = State
    { readFiles = Set.insert filePath (readFiles s)
    , allfiles = foldr Set.insert (allfiles s) relativeImports
    , externalPackages = foldr Set.insert (externalPackages s) absoluteImports
    }
  where
    directory = takeDirectory filePath
    absoluteImports = imports & filter (not . isPrefixOf ".")
    relativeImports =
        imports
      & filter (isPrefixOf ".")
      & fmap (\p -> p ++ ".ts")
      & fmap (combine directory)
      & fmap normalise

recurse' :: State -> IO State
recurse' state =
    case getUnreadFiles state of
        [] -> return state

        (filePath:_) -> do
            imports <- parseFile filePath
            let newState = insertImports filePath imports state
            recurse' newState


main :: IO ()
main = do
    args <- getArgs
    let entry = normalise $ head args
    newState <- recurse' (initialState entry)
    putStrLn (show newState)
