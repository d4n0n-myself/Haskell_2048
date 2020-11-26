module Main where

import Options
import Control.Lens.Lens ((&))
import Network.Wai.Handler.Warp
import Data.Streaming.Network.Internal (HostPreference (Host))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.Map (empty)
import Server (serverApplication)

main :: IO ()
main = runCommand $ \options _ -> do
    putStrLn $ (toString options)
    let warpSettings = optionsToWarpSettings options
    gameStorage <- atomically empty
    runSettings warpSettings $ serverApplication gameStorage

optionsToWarpSettings :: MyOptions -> Settings
optionsToWarpSettings myOptions
    = defaultSettings
        & setHost (Host $ host $ myOptions)
        & setPort (port $ myOptions)
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