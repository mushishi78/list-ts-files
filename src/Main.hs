module Main where

import qualified Streamly.Data.Fold         as FL
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array      as A
import qualified Streamly.Prelude           as S
import qualified System.IO                  as FH

import           Control.Monad.Identity
import           Data.Char                  (ord)
import           Data.Function              ((&))
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Parse                      (findImports)
import           Streamly
import           System.Environment         (getArgs)
import           System.IO                  (IOMode (..), openFile, stdout)

data State = State
    { allfiles  :: [String]
    , readFiles :: [String]
    } deriving (Show)

initialState :: String -> State
initialState entry = State
    { allfiles = [entry]
    , readFiles = []
    }

isRead :: State -> String -> Bool
isRead state file = elem file (readFiles state)

getUnreadFiles :: State -> [String]
getUnreadFiles state =
    filter (not . (isRead state)) (allfiles state)

parseFile :: String -> IO [String]
parseFile fileName = do
    contents <- readFile fileName
    return $ findImports contents




main :: IO ()
main = do
    args <- getArgs
    let entry = head args
    let state = initialState entry
    let unreadFiles = getUnreadFiles state
    imports <- parseFile (head unreadFiles)
    mapM_ putStrLn imports
