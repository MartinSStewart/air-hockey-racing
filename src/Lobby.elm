module Lobby exposing (Lobby, LobbyData, LobbyPreview, allUsers, init, joinUser, preview)

import AssocSet as Set exposing (Set)
import Id exposing (Id)
import User exposing (UserId)


type Lobby
    = Lobby LobbyData


type alias LobbyPreview =
    { name : String, userCount : Int }


type alias LobbyData =
    { name : String
    , owner : Id UserId
    , users : Set (Id UserId)
    }


init : String -> Id UserId -> Lobby
init name owner =
    { name = name
    , owner = owner
    , users = Set.empty
    }
        |> Lobby


joinUser : Id UserId -> Lobby -> Lobby
joinUser userId (Lobby lobby) =
    (if userId == lobby.owner then
        lobby

     else
        { lobby | users = Set.insert userId lobby.users }
    )
        |> Lobby


preview : Lobby -> LobbyPreview
preview (Lobby lobby) =
    { name = lobby.name, userCount = Set.size lobby.users + 1 }


allUsers : Lobby -> List (Id UserId)
allUsers (Lobby lobby) =
    lobby.owner :: Set.toList lobby.users
