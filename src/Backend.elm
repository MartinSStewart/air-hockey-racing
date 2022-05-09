module Backend exposing (app)

import Dict
import Id exposing (Id)
import IdDict
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Types exposing (..)
import User exposing (UserId)


app =
    Lamdera.backend
        { init = ( init, Cmd.none )
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]


init : BackendModel
init =
    { userSessions = Dict.empty
    , users = IdDict.empty
    , lobbies = IdDict.empty
    , matches = IdDict.empty
    }


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            let
                { clientIds, userId } =
                    Dict.get sessionId model.userSessions
                        |> Maybe.withDefault
                            { userId = IdDict.size model.users |> Id.fromInt, clientIds = Dict.empty }
            in
            ( { model
                | userSessions =
                    Dict.insert
                        sessionId
                        { clientIds = Dict.insert clientId () clientIds, userId = userId }
                        model.userSessions
                , users = IdDict.insert userId { name = "TempName" } model.users
              }
            , ClientInit { lobbies = model.lobbies, userId = userId } |> Lamdera.sendToFrontend clientId
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
            , Cmd.none
            )


getUserFromSessionId : SessionId -> BackendModel -> Maybe ( Id UserId, BackendUserData )
getUserFromSessionId sessionId model =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case IdDict.get userId model.users of
                Just user ->
                    Just ( userId, user )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId _ msg model =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case msg of
                SessionChange_ CreateLobby ->
                    ( { model
                        | lobbies =
                            IdDict.insert
                                (IdDict.size model.lobbies |> Id.fromInt)
                                { users = IdDict.singleton userId () }
                                model.lobbies
                      }
                    , broadcastChange CreateLobby (BroadcastCreateLobby userId) userId model
                    )

                SessionChange_ (JoinLobby lobbyId) ->
                    ( { model
                        | lobbies =
                            IdDict.update
                                lobbyId
                                (Maybe.map (\lobby -> { lobby | users = IdDict.insert userId () lobby.users }))
                                model.lobbies
                      }
                    , broadcastChange (JoinLobby lobbyId) (BroadcastJoinLobby userId lobbyId) userId model
                    )

                SessionChange_ (StartMatch time) ->
                    IdDict.toList model.lobbies
                        |> List.find (Tuple.second >> .users >> IdDict.member userId)
                        |> Maybe.map
                            (\( lobbyId, lobby ) ->
                                ( { model
                                    | lobbies =
                                        IdDict.remove
                                            lobbyId
                                            model.lobbies
                                    , matches =
                                        IdDict.insert
                                            (IdDict.size model.matches |> Id.fromInt)
                                            { users = lobby.users }
                                            model.matches
                                  }
                                , broadcastChange (StartMatch time) (BroadcastStartMatch time lobbyId) userId model
                                )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

                SessionChange_ (MatchInput frameId input) ->
                    ( model
                    , IdDict.toList model.matches
                        |> List.find (Tuple.second >> .users >> IdDict.member userId)
                        |> Maybe.map
                            (\( _, { users } ) ->
                                broadcastChange
                                    (MatchInput frameId input)
                                    (BroadcastMatchInput frameId { userId = userId, input = input })
                                    userId
                                    model
                            )
                        |> Maybe.withDefault Cmd.none
                    )

        Nothing ->
            ( model, Cmd.none )


broadcastMatchChange change broadcastChange_ currentUserId model =
    broadcast
        (\sessionId _ ->
            case Dict.get sessionId model.userSessions of
                Just { userId } ->
                    if userId == currentUserId then
                        change |> Just

                    else if userMatch userId model == Nothing then
                        Nothing

                    else
                        broadcastChange_ |> Just

                Nothing ->
                    Nothing
        )
        model


userMatch : Id UserId -> BackendModel -> Maybe (Id MatchId)
userMatch userId model =
    IdDict.toList model.matches
        |> List.find (Tuple.second >> .users >> IdDict.member userId)
        |> Maybe.map Tuple.first


broadcastChange : SessionChange -> BroadcastChange -> Id UserId -> BackendModel -> Cmd BackendMsg
broadcastChange change broadcastChange_ userId model =
    broadcast
        (\sessionId _ ->
            if Dict.get sessionId model.userSessions |> Maybe.map .userId |> (==) (Just userId) then
                Types.SessionChange change |> Change |> Just

            else
                Types.BroadcastChange broadcastChange_ |> Change |> Just
        )
        model


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> Cmd BackendMsg
broadcast msgFunc model =
    model.userSessions
        |> Dict.toList
        |> List.concatMap (\( sessionId, { clientIds } ) -> Dict.keys clientIds |> List.map (Tuple.pair sessionId))
        |> List.filterMap (\( sessionId, clientId ) -> msgFunc sessionId clientId |> Maybe.map (Lamdera.sendToFrontend clientId))
        |> Cmd.batch
