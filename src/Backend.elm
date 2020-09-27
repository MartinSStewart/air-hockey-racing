module Backend exposing (app)

import BackendLogic exposing (Effect(..))
import Dict
import Duration
import Lamdera exposing (ClientId, SessionId)
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = ( BackendLogic.init, Cmd.none )
        , update = \msg model -> BackendLogic.update msg model |> Tuple.mapSecond (List.map effectToCmd >> Cmd.batch)
        , updateFromFrontend =
            \sessionId clientId msg model ->
                BackendLogic.updateFromFrontend sessionId clientId msg model
                    |> Tuple.mapSecond (List.map effectToCmd >> Cmd.batch)
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]


effectToCmd (Effect clientId_ effectMsg) =
    Lamdera.sendToFrontend clientId_ effectMsg
