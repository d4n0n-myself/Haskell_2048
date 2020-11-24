{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Api where

import Servant.API
import Servant.Swagger.UI (SwaggerSchemaUI)
import Data.UUID (UUID)
import Data.Proxy (Proxy(..))

import Types
import Types.Game

type Api
    =    "CreateGame" :> Post '[JSON] GameState
    :<|> "GetGame"    :> Header "Game-Uuid" UUID :> Get '[JSON] GameState
    :<|> "MakeMove"   :> Header "Game-Uuid" UUID :> Header "Move" Int :> Get '[JSON] GameState

type SwaggerApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

type ApiWithSwagger = Api :<|> SwaggerApi

apiProxy :: Proxy ApiWithSwagger
apiProxy = Proxy

