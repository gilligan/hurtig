module Main where

import Hurtig.Foo

main :: IO ()
main = do
  putStrLn $ "Hello, Haskell!" ++ show foo
