module BackendLogic exposing (Effect(..), init, update, updateFromFrontend)

import Dict
import Id exposing (Id)
import IdDict
import List.Extra as List
import Match exposing (Match)
import Types exposing (..)
import User exposing (UserId)


type Effect
    = Effect ClientId ToFrontend


init : BackendModel
init =
    { userSessions = Dict.empty
    , users = IdDict.empty
    , lobbies = IdDict.empty
    , matches = IdDict.empty
    }


update : BackendMsg -> BackendModel -> ( BackendModel, List Effect )
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
            , ClientInit { lobbies = model.lobbies, userId = userId } |> Effect clientId |> List.singleton
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
            , []
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


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, List Effect )
updateFromFrontend sessionId clientId msg model =
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

                SessionChange_ StartMatch ->
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
                                            (Match.init lobby.users)
                                            model.matches
                                  }
                                , broadcastChange StartMatch (BroadcastStartMatch lobbyId) userId model
                                )
                            )
                        |> Maybe.withDefault ( model, [] )

                SessionChange_ (Move time direction) ->
                    ( model, broadcastChange (Move time direction) (BroadcastMove userId time direction) userId model )

        Nothing ->
            ( model, [] )


broadcastMatchChange change broadcastChange_ currentUserId model =
    broadcast
        (\sessionId _ ->
            case Dict.get sessionId model.userSessions of
                Just { userId } ->
                    if userId == currentUserId then
                        SessionChange change |> Change |> Just

                    else if userMatch userId model == Nothing then
                        Nothing

                    else
                        BroadcastChange broadcastChange_ |> Change |> Just

                Nothing ->
                    Nothing
        )
        model


userMatch : Id UserId -> BackendModel -> Maybe ( Id MatchId, Match )
userMatch userId model =
    IdDict.toList model.matches |> List.find (Tuple.second >> Match.users >> IdDict.member userId)


broadcastChange : SessionChange -> BroadcastChange -> Id UserId -> BackendModel -> List Effect
broadcastChange change broadcastChange_ userId model =
    broadcast
        (\sessionId _ ->
            if Dict.get sessionId model.userSessions |> Maybe.map .userId |> (==) (Just userId) then
                SessionChange change |> Change |> Just

            else
                BroadcastChange broadcastChange_ |> Change |> Just
        )
        model


updateUser : Id UserId -> (BackendUserData -> BackendUserData) -> BackendModel -> BackendModel
updateUser userId updateUserFunc model =
    { model | users = IdDict.update userId (Maybe.map updateUserFunc) model.users }


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> List Effect
broadcast msgFunc model =
    model.userSessions
        |> Dict.toList
        |> List.concatMap (\( sessionId, { clientIds } ) -> Dict.keys clientIds |> List.map (Tuple.pair sessionId))
        |> List.filterMap (\( sessionId, clientId ) -> msgFunc sessionId clientId |> Maybe.map (Effect clientId))
