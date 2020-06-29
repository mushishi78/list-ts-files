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
import           Data.Maybe                 (fromMaybe, isJust)
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
    , hasCompleted     :: Bool
    }

instance Show State where
    show (State all read packages _) = "\n" ++ intercalate "\n"
        [ "all: " ++ showSet all
        , "read: " ++ showSet read
        , "packages: " ++ showSet packages
        ]
      where
        showSet =
          Set.toList
            >>> fmap ((++) "\n    ")
            >>> intercalate ""

getInitialState :: FilePath -> State
getInitialState entry = State
    { allfiles = Set.singleton entry
    , readFiles = Set.empty
    , externalPackages = Set.empty
    , hasCompleted = False
    }

getUnreadFiles :: State -> [FilePath]
getUnreadFiles (State all read _ _) = Set.difference all read & Set.toList

insertImports :: FilePath -> [FilePath] -> State -> State
insertImports filePath imports state = state
    { readFiles = Set.insert filePath (readFiles state)
    , allfiles = foldr Set.insert (allfiles state) relativeImports
    , externalPackages = foldr Set.insert (externalPackages state) absoluteImports
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

iteration :: State -> IO State
iteration state =
    case getUnreadFiles state of
        [] -> return (state { hasCompleted = True })

        (filePath:_) -> do
            contents <- readFile filePath
            let imports = findImports contents
            return $ insertImports filePath imports state

main :: IO ()
main = do
    args <- getArgs
    let entry = normalise $ head args

    result <- getInitialState entry
        & pure
        & S.iterateM iteration
        & S.takeWhile (not . hasCompleted)
        & serially
        & S.last

    case result of
        Nothing    -> pure ()
        Just state -> putStrLn $ show state

