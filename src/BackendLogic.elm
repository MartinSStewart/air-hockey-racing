module BackendLogic exposing (Effect(..), init, update, updateFromFrontend)

import Dict
import Id exposing (Id)
import IdDict
import Types exposing (..)


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
                    , broadcastCreateLobby userId model
                    )

        Nothing ->
            ( model, [] )


broadcastCreateLobby : Id UserId -> BackendModel -> List Effect
broadcastCreateLobby userId model =
    broadcast
        (\sessionId _ ->
            if Dict.get sessionId model.userSessions |> Maybe.map .userId |> (==) (Just userId) then
                SessionChange CreateLobby |> Change |> Just

            else
                BroadcastChange (BroadcastCreateLobby userId) |> Change |> Just
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
