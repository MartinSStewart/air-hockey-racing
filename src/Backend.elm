module Backend exposing (app)

import AssocList as Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task
import Effect.Time
import Id exposing (Id)
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import MatchSetup exposing (MatchSetup, MatchSetupMsg(..), PlayerData)
import Time
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
    , counter = 0
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
            , ClientInit userId (getLobbyData model)
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

        GotTimeForUpdateFromFrontend sessionId clientId toBackend time ->
            updateFromFrontendWithTime sessionId clientId toBackend model time


getLobbyData model =
    { lobbies = Dict.map (\_ lobby -> MatchSetup.preview lobby) model.lobbies }


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
    ( model, Effect.Time.now |> Task.perform (GotTimeForUpdateFromFrontend sessionId clientId msg) )


updateFromFrontendWithTime :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> Time.Posix
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontendWithTime sessionId clientId msg model time =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case msg of
                CreateLobbyRequest ->
                    let
                        ( lobbyId, model2 ) =
                            getId model

                        lobby =
                            MatchSetup.init "New lobby" userId

                        lobbyPreview =
                            MatchSetup.preview lobby
                    in
                    ( { model2 | lobbies = Dict.insert lobbyId lobby model2.lobbies }
                    , Command.batch
                        [ CreateLobbyResponse lobbyId lobby |> Effect.Lamdera.sendToFrontend clientId
                        , Dict.keys model2.userSessions
                            |> List.map
                                (\userSessionId ->
                                    CreateLobbyBroadcast lobbyId lobbyPreview
                                        |> Effect.Lamdera.sendToFrontends userSessionId
                                )
                            |> Command.batch
                        ]
                    )

                MatchSetupRequest lobbyId matchSetupMsg ->
                    case Dict.get lobbyId model.lobbies of
                        Just matchSetup ->
                            let
                                matchSetup2 : Maybe MatchSetup
                                matchSetup2 =
                                    MatchSetup.matchSetupUpdate { userId = userId, msg = matchSetupMsg } matchSetup

                                model2 =
                                    { model | lobbies = Dict.update lobbyId (\_ -> matchSetup2) model.lobbies }
                            in
                            ( model2
                            , Command.batch
                                [ case matchSetupMsg of
                                    JoinMatchSetup ->
                                        MatchSetup.joinUser userId matchSetup
                                            |> Ok
                                            |> JoinLobbyResponse lobbyId
                                            |> Effect.Lamdera.sendToFrontend clientId

                                    _ ->
                                        Command.none
                                , case matchSetup2 of
                                    Just _ ->
                                        Command.none

                                    Nothing ->
                                        Effect.Lamdera.broadcast (RemoveLobbyBroadcast lobbyId)
                                , MatchSetup.allUsers matchSetup
                                    |> List.Nonempty.toList
                                    |> List.concatMap
                                        (\( lobbyUserId, _ ) ->
                                            getSessionIdsFromUserId lobbyUserId model
                                                |> List.map
                                                    (\lobbyUserSessionId ->
                                                        MatchSetupBroadcast lobbyId
                                                            userId
                                                            matchSetupMsg
                                                            (case ( lobbyUserId == userId, matchSetupMsg ) of
                                                                ( True, LeaveMatchSetup ) ->
                                                                    getLobbyData model2 |> Just

                                                                _ ->
                                                                    Nothing
                                                            )
                                                            |> Effect.Lamdera.sendToFrontends lobbyUserSessionId
                                                    )
                                        )
                                    |> Command.batch
                                ]
                            )

                        Nothing ->
                            ( model
                            , Err LobbyNotFound |> JoinLobbyResponse lobbyId |> Effect.Lamdera.sendToFrontend clientId
                            )

                PingRequest ->
                    ( model, PingResponse time |> Effect.Lamdera.sendToFrontend clientId )

        Nothing ->
            ( model, Command.none )


getId : BackendModel -> ( Id a, BackendModel )
getId model =
    ( Id.fromInt model.counter, { model | counter = model.counter + 1 } )


getUserOwnedLobby : Id UserId -> BackendModel -> Maybe ( Id LobbyId, MatchSetup )
getUserOwnedLobby userId model =
    Dict.toList model.lobbies
        |> List.find (\( _, lobby ) -> MatchSetup.isOwner userId lobby)


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
