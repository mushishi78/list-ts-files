module Main where

import           Data.Function      ((&))
import           Process            (process)
import qualified Streamly.Prelude   as S
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let entry = head args

    process entry
        & S.mapM_ putStrLn

