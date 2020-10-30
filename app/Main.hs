module Main where

import Types.Game


main :: IO ()
main = do
  let initVals = values $ grid initialState
  grid1 <- generateRandomTile initVals
  grid <- generateRandomTile grid1
  mainChecks grid