{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server (serverApplication) where

import Prelude hiding (lookup, Left, Right)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Data.Swagger
import Data.UUID (UUID)
import Control.Lens
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.STM.Map (Map, insert, lookup)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM (atomically)

import Types
import Api

type Cache = Map UUID GameState

type AppM = ReaderT Cache Handler


gameServer :: ServerT ApiWithSwagger AppM
gameServer =
    (createGame
    :<|> getGame
    :<|> makeMove)
    :<|> swaggerDocServer

createGame :: AppM GameState
createGame = do
    gameStorage <- ask
    newGameState <- liftIO createGameState
    let newGameGuid = uuid $ newGameState
    liftIO $ atomically $ insert newGameGuid newGameState gameStorage
    return newGameState

getGame :: Maybe UUID -> AppM GameState
getGame Nothing = failingHandler "Game-Uuid header is required!"
getGame (Just gameUuid) = do
    gameStorage <- ask
    gameStateMaybe <- liftIO $ atomically $ lookup gameUuid gameStorage
    case gameStateMaybe of
        Just gameState -> return gameState
        Nothing -> failingHandler $ "Game with UUID '" ++ show gameUuid ++ "' does not exist!"

makeMove :: Maybe UUID -> Maybe Int -> AppM GameState
makeMove Nothing Nothing = failingHandler "Parameters not provided"
makeMove Nothing _ = failingHandler "UUID not provided"
makeMove _ Nothing = failingHandler "Move not provided"
makeMove (Just guid) (Just moveInt) = do
  if (moveInt > 4 || moveInt < 1)
        then failingHandler "Moves can be only [1..4]"
        else do
            gameStorage <- ask
            gameStateMaybe <- liftIO $ atomically $ lookup guid gameStorage
            case gameStateMaybe of
                Nothing -> failingHandler $ "Game with UUID '" ++ show guid ++ "' does not exist!"
                Just gameState -> do
                  if gameWon $ gameState then return gameState
                  else do
                    if gameLost $ gameState then return gameState
                    else do
                      let moveValue = moveMappingFromInt moveInt
                      let afterMoveGrid = performMove moveValue (values $ grid $ gameState)
                      let isGameLost = checkIfGameLost afterMoveGrid
                      let isGameWon = checkIfGameWon afterMoveGrid
                      if (isGameLost || isGameWon) then return GameState {
                                                                 grid = PlayGrid { values = afterMoveGrid },
                                                                 uuid = guid,
                                                                 gameWon = isGameWon,
                                                                 gameLost = isGameLost
                                                               }
                      else do
                        if ((values $ grid $ gameState) == afterMoveGrid)
                          then failingHandler "Nothing changed, try another direction"
                          else do
                            newTiles <- liftIO $ generateRandomTile afterMoveGrid
                            let newGameState = GameState {
                              grid = PlayGrid { values = newTiles },
                              uuid = guid,
                              gameWon = isGameWon,
                              gameLost = isGameLost
                            }
                            liftIO $ atomically $ insert guid newGameState gameStorage
                            return newGameState

failingHandler message = throwError $ err400 { errReasonPhrase = message }

serverApplication :: Cache -> Application
serverApplication gameStorage =
    serve apiProxy $
    hoistServer apiProxy ((flip runReaderT) gameStorage) gameServer

swagger :: Swagger
swagger = toSwagger (Proxy :: Proxy Api)
    & info.title       .~ "2048 API"
    & info.version     .~ "1.0"
    & info.description ?~ "API for communicating with game server."

swaggerFromHandlerToAppM :: Server SwaggerApi -> ServerT SwaggerApi AppM
swaggerFromHandlerToAppM = hoistServer (Proxy :: Proxy SwaggerApi) lift

swaggerDocServer :: ServerT SwaggerApi AppM
swaggerDocServer = swaggerFromHandlerToAppM $ swaggerSchemaUIServer swagger

