module Backend exposing (app)

import AssocList as Dict
import AssocSet as Set exposing (Set)
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Id exposing (Id)
import Lamdera
import List.Extra as List
import Lobby
import Types exposing (..)
import User exposing (UserId)


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        { init = ( init, Command.none )
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Subscription BackendOnly BackendMsg
subscriptions model =
    Subscription.batch
        [ Effect.Lamdera.onConnect ClientConnected
        , Effect.Lamdera.onDisconnect ClientDisconnected
        ]


init : BackendModel
init =
    { userSessions = Dict.empty
    , users = Dict.empty
    , lobbies = Dict.empty
    , matches = Dict.empty
    }


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            let
                { clientIds, userId } =
                    Dict.get sessionId model.userSessions
                        |> Maybe.withDefault
                            { userId = Dict.size model.users |> Id.fromInt, clientIds = Dict.empty }
            in
            ( { model
                | userSessions =
                    Dict.insert
                        sessionId
                        { clientIds = Dict.insert clientId () clientIds, userId = userId }
                        model.userSessions
                , users = Dict.insert userId { name = "TempName" } model.users
              }
            , ClientInit
                { lobbies = Dict.map (\_ lobby -> Lobby.preview lobby) model.lobbies
                , userId = userId
                , currentLobby = Nothing
                }
                |> Effect.Lamdera.sendToFrontend clientId
            )

        ClientDisconnected sessionId clientId ->
            ( { model
                | userSessions =
                    Dict.update
                        sessionId
                        (Maybe.map
                            (\userSession ->
                                { userSession | clientIds = Dict.remove clientId userSession.clientIds }
                            )
                        )
                        model.userSessions
              }
            , Command.none
            )


getUserFromSessionId : SessionId -> BackendModel -> Maybe ( Id UserId, BackendUserData )
getUserFromSessionId sessionId model =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case Dict.get userId model.users of
                Just user ->
                    Just ( userId, user )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


updateFromFrontend :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case msg of
                CreateLobbyRequest ->
                    let
                        lobbyId : Id LobbyId
                        lobbyId =
                            Dict.size model.lobbies |> Id.fromInt

                        lobby =
                            Lobby.init "New lobby" userId
                    in
                    ( { model | lobbies = Dict.insert lobbyId lobby model.lobbies }
                    , CreateLobbyResponse lobbyId lobby |> Effect.Lamdera.sendToFrontend clientId
                    )

                JoinLobbyRequest lobbyId ->
                    case Dict.get lobbyId model.lobbies of
                        Just lobby ->
                            ( { model
                                | lobbies =
                                    Dict.update
                                        lobbyId
                                        (\_ -> Just { lobby | users = Set.insert userId lobby.users })
                                        model.lobbies
                              }
                            , Command.batch
                                [ Ok lobby |> JoinLobbyResponse |> Effect.Lamdera.sendToFrontend clientId
                                , Set.toList lobby.users
                                    |> List.concatMap
                                        (\lobbyUserId ->
                                            getSessionIdsFromUserId lobbyUserId model
                                                |> List.map
                                                    (\lobbyUserSessionId ->
                                                        JoinLobbyBroadcast userId
                                                            |> Effect.Lamdera.sendToFrontends lobbyUserSessionId
                                                    )
                                        )
                                    |> Command.batch
                                ]
                            )

                        Nothing ->
                            ( model
                            , Err LobbyNotFound |> JoinLobbyResponse |> Effect.Lamdera.sendToFrontend clientId
                            )

                StartMatchRequest time ->
                    Debug.todo ""

        Nothing ->
            ( model, Command.none )


getSessionIdsFromUserId : Id UserId -> BackendModel -> List SessionId
getSessionIdsFromUserId userId model =
    Dict.toList model.userSessions
        |> List.filterMap
            (\( sessionId, data ) ->
                if userId == data.userId then
                    Just sessionId

                else
                    Nothing
            )
