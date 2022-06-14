module Backend exposing (app)

import AssocList as Dict exposing (Dict)
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task
import Effect.Time
import Id exposing (Id)
import Lamdera
import List.Nonempty
import MatchSetup exposing (MatchSetup, MatchSetupMsg(..), ServerTime(..))
import NetworkModel exposing (EventId)
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
subscriptions _ =
    Subscription.batch
        [ Effect.Lamdera.onConnect ClientConnected
        , Effect.Lamdera.onDisconnect ClientDisconnected
        ]


init : BackendModel
init =
    { userSessions = Dict.empty
    , users = Dict.empty
    , lobbies = Dict.empty
    , dummyChange = 0
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
            ( model, Effect.Time.now |> Task.perform (ServerTime >> ClientDisconnectedWithTime sessionId clientId) )

        ClientDisconnectedWithTime sessionId clientId time ->
            case getUserFromSessionId sessionId model of
                Just ( userId, _ ) ->
                    let
                        matchIds : List (Id LobbyId)
                        matchIds =
                            Dict.toList model.lobbies
                                |> List.filterMap
                                    (\( lobbyId, match ) ->
                                        if MatchSetup.allUsers_ match |> Dict.member userId then
                                            Just lobbyId

                                        else
                                            Nothing
                                    )
                    in
                    List.foldl
                        (\matchId ( model2, cmd ) ->
                            matchSetupRequest time matchId userId (Id.fromInt -1) clientId LeaveMatchSetup model2
                                |> Tuple.mapSecond (\cmd2 -> Command.batch [ cmd, cmd2 ])
                        )
                        ( { model
                            | userSessions =
                                Dict.update
                                    sessionId
                                    (Maybe.map
                                        (\userSession ->
                                            { userSession
                                                | clientIds = Dict.remove clientId userSession.clientIds
                                            }
                                        )
                                    )
                                    model.userSessions
                          }
                        , Command.none
                        )
                        matchIds

                Nothing ->
                    ( model, Command.none )

        UpdateFromFrontendWithTime sessionId clientId toBackend time ->
            updateFromFrontendWithTime sessionId clientId toBackend model time


getLobbyData : BackendModel -> { lobbies : Dict (Id LobbyId) MatchSetup.LobbyPreview }
getLobbyData model =
    { lobbies =
        Dict.filter
            (\_ lobby -> MatchSetup.getMatch lobby == Nothing)
            model.lobbies
            |> Dict.map (\_ lobby -> MatchSetup.preview lobby)
    }


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
    ( model, Effect.Time.now |> Task.perform (ServerTime >> UpdateFromFrontendWithTime sessionId clientId msg) )


updateFromFrontendWithTime :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ServerTime
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontendWithTime sessionId clientId msg model time =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case msg of
                CreateMatchRequest ->
                    let
                        ( lobbyId, model2 ) =
                            getId model

                        lobby =
                            MatchSetup.init userId

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

                MatchSetupRequest lobbyId eventId matchSetupMsg ->
                    matchSetupRequest time lobbyId userId eventId clientId matchSetupMsg model

                PingRequest ->
                    ( model, PingResponse time |> Effect.Lamdera.sendToFrontend clientId )

        Nothing ->
            ( model, Command.none )


matchSetupRequest :
    ServerTime
    -> Id LobbyId
    -> Id UserId
    -> Id EventId
    -> ClientId
    -> MatchSetupMsg
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
matchSetupRequest currentTime lobbyId userId eventId clientId matchSetupMsg model =
    case Dict.get lobbyId model.lobbies of
        Just matchSetup ->
            let
                matchSetup2 : MatchSetup
                matchSetup2 =
                    MatchSetup.matchSetupUpdate { userId = userId, msg = matchSetupMsg } matchSetup

                model2 : BackendModel
                model2 =
                    { model | lobbies = Dict.update lobbyId (\_ -> Just matchSetup2) model.lobbies }

                matchSetupMsg2 : MatchSetupMsg
                matchSetupMsg2 =
                    case matchSetupMsg of
                        MatchInputRequest time input ->
                            MatchInputRequest (MatchSetup.clampTime currentTime time) input

                        _ ->
                            matchSetupMsg

                matchSetupBroadcast : BackendModel -> Command BackendOnly ToFrontend BackendMsg
                matchSetupBroadcast model_ =
                    MatchSetup.allUsers matchSetup
                        |> List.Nonempty.toList
                        |> List.concatMap
                            (\( lobbyUserId, _ ) ->
                                getSessionIdsFromUserId lobbyUserId model_
                                    |> List.map
                                        (\lobbyUserSessionId ->
                                            if lobbyUserId == userId then
                                                MatchSetupResponse
                                                    lobbyId
                                                    userId
                                                    matchSetupMsg2
                                                    (case ( lobbyUserId == userId, matchSetupMsg2 ) of
                                                        ( True, LeaveMatchSetup ) ->
                                                            getLobbyData model_ |> Just

                                                        _ ->
                                                            Nothing
                                                    )
                                                    eventId
                                                    |> Effect.Lamdera.sendToFrontends lobbyUserSessionId

                                            else
                                                MatchSetupBroadcast
                                                    lobbyId
                                                    userId
                                                    matchSetupMsg2
                                                    |> Effect.Lamdera.sendToFrontends lobbyUserSessionId
                                        )
                            )
                        |> Command.batch
            in
            case matchSetupMsg2 of
                MatchInputRequest _ _ ->
                    ( model2, matchSetupBroadcast model2 )

                JoinMatchSetup ->
                    if List.Nonempty.length (MatchSetup.allUsers matchSetup) < MatchSetup.maxPlayers matchSetup then
                        ( model2
                        , Command.batch
                            [ MatchSetup.joinUser userId matchSetup
                                |> Ok
                                |> JoinLobbyResponse lobbyId
                                |> Effect.Lamdera.sendToFrontend clientId
                            , matchSetupBroadcast model2
                            , newPreview lobbyId matchSetup matchSetup2
                            ]
                        )

                    else
                        ( model
                        , Err LobbyFull |> JoinLobbyResponse lobbyId |> Effect.Lamdera.sendToFrontend clientId
                        )

                LeaveMatchSetup ->
                    case MatchSetup.leaveUser userId matchSetup of
                        Just _ ->
                            ( model2, Command.batch [ matchSetupBroadcast model2, newPreview lobbyId matchSetup matchSetup2 ] )

                        Nothing ->
                            let
                                model3 =
                                    { model | lobbies = Dict.remove lobbyId model.lobbies }
                            in
                            ( model3
                            , Command.batch
                                [ matchSetupBroadcast model3
                                , Effect.Lamdera.broadcast (RemoveLobbyBroadcast lobbyId)
                                ]
                            )

                _ ->
                    ( model2, Command.batch [ newPreview lobbyId matchSetup matchSetup2, matchSetupBroadcast model2 ] )

        Nothing ->
            ( model
            , Err LobbyNotFound |> JoinLobbyResponse lobbyId |> Effect.Lamdera.sendToFrontend clientId
            )


newPreview : Id LobbyId -> MatchSetup -> MatchSetup -> Command BackendOnly ToFrontend BackendMsg
newPreview lobbyId oldMatchSetup newMatchSetup =
    if MatchSetup.preview oldMatchSetup == MatchSetup.preview newMatchSetup then
        Command.none

    else
        MatchSetup.preview newMatchSetup
            |> UpdateLobbyBroadcast lobbyId
            |> Effect.Lamdera.broadcast


getId : BackendModel -> ( Id a, BackendModel )
getId model =
    ( Id.fromInt model.counter, { model | counter = model.counter + 1 } )


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
