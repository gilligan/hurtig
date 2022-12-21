module Hurtig where

import qualified Data.List as L
import Hurtig.QuickLog (printQuickLog)
import Hurtig.QuickParser (parseQuickOutput)

runHurtig :: IO ()
runHurtig = do
  stdin <- getContents
  case parseQuickOutput stdin of
    Right res -> do
      let p = L.intercalate "\n" $ printQuickLog <$> res
      putStrLn p
    Left x -> putStrLn $ "Failed to parse input:\n\n" ++ show x
