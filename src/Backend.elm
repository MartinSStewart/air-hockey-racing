module Backend exposing (app, app_)

import Duration
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task
import Effect.Time
import Id exposing (Id)
import Lamdera
import Length exposing (Meters)
import List.Extra
import List.Nonempty
import Match exposing (Match, Msg(..), ServerTime(..), WorldCoordinate)
import MatchPage exposing (MatchId)
import NetworkModel exposing (EventId)
import NonemptySet
import Point2d exposing (Point2d)
import Quantity
import SeqDict exposing (SeqDict)
import SeqSet
import Timeline exposing (FrameId, Timeline)
import Types exposing (..)
import User exposing (UserId)


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : String -> String -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Effect.Lamdera.backend Lamdera.broadcast Lamdera.sendToFrontend app_


app_ :
    { init : ( BackendModel, Command restriction toMsg msg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
    , subscriptions : BackendModel -> Subscription BackendOnly BackendMsg
    }
app_ =
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
    { userSessions = SeqDict.empty
    , users = SeqDict.empty
    , lobbies = SeqDict.empty
    , joiningActiveMatch = SeqDict.empty
    , dummyChange = 0
    , counter = 0
    , playerPositions = SeqDict.empty
    }


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            let
                { clientIds, userId } =
                    SeqDict.get sessionId model.userSessions
                        |> Maybe.withDefault
                            { userId = SeqDict.size model.users |> Id.fromInt, clientIds = SeqDict.empty }
            in
            ( { model
                | userSessions =
                    SeqDict.insert
                        sessionId
                        { clientIds = SeqDict.insert clientId () clientIds, userId = userId }
                        model.userSessions
                , users = SeqDict.insert userId { name = "TempName" } model.users
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
                        matchIds : List (Id MatchId)
                        matchIds =
                            SeqDict.toList model.lobbies
                                |> List.filterMap
                                    (\( lobbyId, match ) ->
                                        if Match.allUsers_ match |> SeqDict.member userId then
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
                                SeqDict.update
                                    sessionId
                                    (Maybe.map
                                        (\userSession ->
                                            { userSession
                                                | clientIds = SeqDict.remove clientId userSession.clientIds
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


getLobbyData : BackendModel -> { lobbies : SeqDict (Id MatchId) Match.LobbyPreview }
getLobbyData model =
    { lobbies =
        SeqDict.filter
            (\_ lobby -> Match.matchActive lobby == Nothing)
            model.lobbies
            |> SeqDict.map (\_ lobby -> Match.preview lobby)
    }


getUserFromSessionId : SessionId -> BackendModel -> Maybe ( Id UserId, BackendUserData )
getUserFromSessionId sessionId model =
    case SeqDict.get sessionId model.userSessions of
        Just { userId } ->
            case SeqDict.get userId model.users of
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


updateMatchPageToBackend :
    Id UserId
    -> SessionId
    -> ClientId
    -> MatchPage.ToBackend
    -> BackendModel
    -> ServerTime
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateMatchPageToBackend userId sessionId clientId msg model time =
    case Debug.log "updateMatchPageToBackend" msg of
        MatchPage.MatchRequest lobbyId eventId matchSetupMsg ->
            matchSetupRequest time lobbyId userId eventId clientId matchSetupMsg model

        MatchPage.DesyncCheckRequest lobbyId frameId positions ->
            case SeqDict.get lobbyId model.lobbies of
                Just match ->
                    let
                        playerPositions : SeqDict (Id FrameId) (SeqDict (Id UserId) (Point2d Meters WorldCoordinate))
                        playerPositions =
                            SeqDict.get lobbyId model.playerPositions |> Maybe.withDefault SeqDict.empty
                    in
                    case SeqDict.get frameId playerPositions of
                        Just playerPositions2 ->
                            if playerPositions2 == positions then
                                ( model, Command.none )

                            else
                                ( model
                                , broadcastToMatch match (MatchPage.DesyncBroadcast lobbyId frameId) model
                                )

                        Nothing ->
                            ( { model
                                | playerPositions =
                                    SeqDict.insert
                                        lobbyId
                                        (SeqDict.insert frameId positions playerPositions
                                            |> SeqDict.remove (Id.toInt frameId - 30 |> Id.fromInt)
                                        )
                                        model.playerPositions
                              }
                            , Command.none
                            )

                Nothing ->
                    ( model, Command.none )

        MatchPage.CurrentCache matchId frameId matchState ->
            case ( SeqDict.get ( matchId, frameId ) model.joiningActiveMatch, SeqDict.get matchId model.lobbies ) of
                ( Just usersTryingToJoin, Just match ) ->
                    ( { model | joiningActiveMatch = SeqDict.remove ( matchId, frameId ) model.joiningActiveMatch }
                    , NonemptySet.toList usersTryingToJoin
                        |> List.map
                            (\clientId2 ->
                                JoinLobbyResponse matchId (JoinedActiveMatch match frameId matchState)
                                    |> Effect.Lamdera.sendToFrontend clientId2
                            )
                        |> Command.batch
                    )

                _ ->
                    ( model, Command.none )


updateFromFrontendWithTime :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ServerTime
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontendWithTime sessionId clientId msg model time =
    case SeqDict.get sessionId model.userSessions of
        Just { userId } ->
            case msg of
                CreateMatchRequest ->
                    let
                        ( lobbyId, model2 ) =
                            getId model

                        lobby =
                            Match.init userId

                        lobbyPreview =
                            Match.preview lobby
                    in
                    ( { model2 | lobbies = SeqDict.insert lobbyId lobby model2.lobbies }
                    , Command.batch
                        [ CreateLobbyResponse lobbyId lobby |> Effect.Lamdera.sendToFrontend clientId
                        , SeqDict.keys model2.userSessions
                            |> List.map
                                (\userSessionId ->
                                    CreateLobbyBroadcast lobbyId lobbyPreview
                                        |> Effect.Lamdera.sendToFrontends userSessionId
                                )
                            |> Command.batch
                        ]
                    )

                MatchPageToBackend matchPageToBackend ->
                    updateMatchPageToBackend userId sessionId clientId matchPageToBackend model time

                PingRequest ->
                    ( model, PingResponse time |> Effect.Lamdera.sendToFrontend clientId )

                EditorPageToBackend _ ->
                    ( model, Command.none )

        Nothing ->
            ( model, Command.none )


broadcastToMatch : Match -> MatchPage.ToFrontend -> BackendModel -> Command BackendOnly ToFrontend BackendMsg
broadcastToMatch match toFrontend model =
    Match.allUsers match
        |> List.Nonempty.toList
        |> List.concatMap
            (\( lobbyUserId, _ ) ->
                getSessionIdsFromUserId lobbyUserId model
                    |> List.map
                        (\lobbyUserSessionId ->
                            Effect.Lamdera.sendToFrontends lobbyUserSessionId (MatchPageToFrontend toFrontend)
                        )
            )
        |> Command.batch


matchSetupRequest :
    ServerTime
    -> Id MatchId
    -> Id UserId
    -> Id EventId
    -> ClientId
    -> Msg
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
matchSetupRequest currentTime lobbyId userId eventId clientId matchSetupMsg model =
    case SeqDict.get lobbyId model.lobbies of
        Just match ->
            let
                matchSetup2 : Match
                matchSetup2 =
                    Match.matchSetupUpdate { userId = userId, msg = matchSetupMsg } match

                model2 : BackendModel
                model2 =
                    { model | lobbies = SeqDict.update lobbyId (\_ -> Just matchSetup2) model.lobbies }

                matchSetupMsg2 : Msg
                matchSetupMsg2 =
                    case matchSetupMsg of
                        MatchInputRequest time input ->
                            MatchInputRequest (Match.clampTime currentTime time) input

                        _ ->
                            matchSetupMsg

                matchSetupBroadcast : BackendModel -> Command BackendOnly ToFrontend BackendMsg
                matchSetupBroadcast model_ =
                    Match.allUsers match
                        |> List.Nonempty.toList
                        |> List.concatMap
                            (\( lobbyUserId, _ ) ->
                                getSessionIdsFromUserId lobbyUserId model_
                                    |> List.map
                                        (\lobbyUserSessionId ->
                                            (if lobbyUserId == userId then
                                                case matchSetupMsg2 of
                                                    LeaveMatchSetup ->
                                                        getLobbyData model_ |> RejoinMainLobby

                                                    _ ->
                                                        MatchPage.MatchSetupResponse
                                                            lobbyId
                                                            userId
                                                            matchSetupMsg2
                                                            eventId
                                                            |> MatchPageToFrontend

                                             else
                                                MatchPage.MatchSetupBroadcast lobbyId userId matchSetupMsg2
                                                    |> MatchPageToFrontend
                                            )
                                                |> Effect.Lamdera.sendToFrontends lobbyUserSessionId
                                        )
                            )
                        |> Command.batch
            in
            case matchSetupMsg2 of
                MatchInputRequest _ _ ->
                    ( model2, matchSetupBroadcast model2 )

                JoinMatchSetup ->
                    case Match.joinUser userId match of
                        Ok matchWithJoinedUser ->
                            case Match.matchActive match of
                                Just matchActive ->
                                    let
                                        latestFrameThatWontChange : Id FrameId
                                        latestFrameThatWontChange =
                                            Match.serverTimeToFrameId
                                                (Match.serverTimeAdd (Quantity.negate Match.maxInputDelay) currentTime)
                                                matchActive
                                    in
                                    ( { model2
                                        | joiningActiveMatch =
                                            SeqDict.update
                                                ( lobbyId, latestFrameThatWontChange )
                                                (\maybe ->
                                                    case maybe of
                                                        Just nonempty ->
                                                            NonemptySet.insert clientId nonempty |> Just

                                                        Nothing ->
                                                            NonemptySet.singleton clientId |> Just
                                                )
                                                model2.joiningActiveMatch
                                      }
                                    , broadcastToMatch match (MatchPage.NeedCurrentCacheBroadcast lobbyId latestFrameThatWontChange) model2
                                    )

                                Nothing ->
                                    ( { model2 | lobbies = SeqDict.insert lobbyId matchWithJoinedUser model2.lobbies }
                                    , Command.batch
                                        [ JoinedLobby matchWithJoinedUser
                                            |> JoinLobbyResponse lobbyId
                                            |> Effect.Lamdera.sendToFrontend clientId
                                        , matchSetupBroadcast model2
                                        , newPreview lobbyId matchWithJoinedUser matchSetup2
                                        ]
                                    )

                        Err () ->
                            ( model
                            , JoinLobbyResponse lobbyId (JoinLobbyError MatchFull) |> Effect.Lamdera.sendToFrontend clientId
                            )

                LeaveMatchSetup ->
                    case Match.leaveUser userId match of
                        Just _ ->
                            ( model2, Command.batch [ matchSetupBroadcast model2, newPreview lobbyId match matchSetup2 ] )

                        Nothing ->
                            let
                                model3 =
                                    { model | lobbies = SeqDict.remove lobbyId model.lobbies }
                            in
                            ( model3
                            , Command.batch
                                [ matchSetupBroadcast model3
                                , Effect.Lamdera.broadcast (RemoveLobbyBroadcast lobbyId)
                                ]
                            )

                _ ->
                    ( model2, Command.batch [ newPreview lobbyId match matchSetup2, matchSetupBroadcast model2 ] )

        Nothing ->
            ( model
            , JoinLobbyResponse lobbyId (JoinLobbyError MatchNotFound) |> Effect.Lamdera.sendToFrontend clientId
            )


newPreview : Id MatchId -> Match -> Match -> Command BackendOnly ToFrontend BackendMsg
newPreview lobbyId oldMatchSetup newMatchSetup =
    if Match.preview oldMatchSetup == Match.preview newMatchSetup then
        Command.none

    else
        Match.preview newMatchSetup
            |> UpdateLobbyBroadcast lobbyId
            |> Effect.Lamdera.broadcast


getId : BackendModel -> ( Id a, BackendModel )
getId model =
    ( Id.fromInt model.counter, { model | counter = model.counter + 1 } )


getSessionIdsFromUserId : Id UserId -> BackendModel -> List SessionId
getSessionIdsFromUserId userId model =
    SeqDict.toList model.userSessions
        |> List.filterMap
            (\( sessionId, data ) ->
                if userId == data.userId then
                    Just sessionId

                else
                    Nothing
            )
