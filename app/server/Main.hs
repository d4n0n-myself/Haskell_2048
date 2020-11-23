module Main where

import Options
import Control.Lens.Lens ((&))
import Network.Wai.Handler.Warp
import Data.Streaming.Network.Internal (HostPreference (Host))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.Map (empty)
import GHC.Show
import Server (serverApplication)
import System.Random

main :: IO ()
main = runCommand $ \options _ -> do
    putStrLn $ (toString options)
    let warpSettings = optionsToWarpSettings
    gameStorage <- atomically empty
    runSettings warpSettings $ serverApplication gameStorage

optionsToWarpSettings :: Settings
optionsToWarpSettings
    = defaultSettings
        & setHost (Host "127.0.0.1")
        & setPort (5010)
        & setTimeout (60)

data MyOptions = MyOptions
  { host :: String,
    port ::  Int
  }

toString :: MyOptions -> String
toString options = "Host: " ++ (options & host) ++ ", Port: " ++ show (options & port)

instance Options MyOptions where
    defineOptions = pure MyOptions
        <*> simpleOption "host" "127.0.0.1" ""
        <*> simpleOption "port" 5010 ""

instance Show MyOptions where
    show options = "Host: " ++ (options & host) ++ ", Port: " ++ show (options & port)