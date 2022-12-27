module Hurtig where

import Hurtig.QuickParser (parseQuickOutput)
import Hurtig.TestTree
import System.Exit

runHurtig :: IO ()
runHurtig = do
  stdin <- getContents
  case parseQuickOutput stdin of
    Right res -> do
      putStrLn $ printTree 2 $ fromQuickLog res
      exitSuccess
    Left err -> do
      putStrLn $ "Failed to parse input:\n\n" ++ show err
      exitWith (ExitFailure 1)
