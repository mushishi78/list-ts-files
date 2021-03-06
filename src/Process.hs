module Process (process) where

import           Control.Arrow    ((>>>))
import           Data.Function    ((&))
import           Data.List        (intercalate, isPrefixOf)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Parse            (findImports)
import           Streamly
import qualified Streamly.Prelude as S
import           System.FilePath

data State = State
    { allFiles         :: Set FilePath
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

emptyState :: State
emptyState = State
    { allFiles = Set.empty
    , readFiles = Set.empty
    , externalPackages = Set.empty
    , hasCompleted = False
    }

getInitialState :: FilePath -> State
getInitialState entry = emptyState { allFiles = Set.singleton (normalise entry) }

getUnreadFiles :: State -> [FilePath]
getUnreadFiles (State all read _ _) = Set.difference all read & Set.toList

insertImports :: FilePath -> [FilePath] -> State -> State
insertImports filePath imports state = state
    { readFiles = Set.insert filePath (readFiles state)
    , allFiles = foldr Set.insert (allFiles state) relativeImports
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

readImports :: State -> FilePath -> IO State
readImports state filePath = do
    contents <- readFile filePath
    let imports = findImports contents
    return $ insertImports filePath imports state

mergeState :: State -> State -> State
mergeState a b = State
    { allFiles = Set.union (allFiles a) (allFiles b)
    , readFiles = Set.union (readFiles a) (readFiles b)
    , externalPackages = Set.union (externalPackages a) (externalPackages b)
    , hasCompleted = (hasCompleted a) || (hasCompleted b)
    }

iteration :: State -> IO State
iteration state =
    case getUnreadFiles state of
        [] -> return (state { hasCompleted = True })

        unreadFiles ->
              S.fromList unreadFiles
            & S.mapM (readImports state)
            & asyncly
            & S.foldr mergeState state

uniquePaths :: (Set String, State) -> State -> (Set String, State)
uniquePaths (_, previousState) state =
    ( Set.union newLocalFiles newPackages, state )

  where
    newLocalFiles = Set.difference (allFiles state) (allFiles previousState)
    newPackages = Set.difference (externalPackages state) (externalPackages previousState)

process :: FilePath -> Serial String
process entry =
  getInitialState entry
    & pure
    & S.iterateM iteration
    & S.takeWhile (not . hasCompleted)
    & asyncly
    & S.scanl' uniquePaths (Set.empty, emptyState)
    & fmap (Set.toList . fst)
    & S.concatMap (S.fromList)
