module Main where

import Lib
import Types


main :: IO ()
main = someFunc

list1 :: [[Int]]
list1 = [[1,0,0],[0,1,0],[0,0,1]]

drawGrid :: PlayGrid -> IO ()
drawGrid grid = print grid
