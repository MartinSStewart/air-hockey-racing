module MatchSetup exposing
    ( LobbyPreview
    , MatchSetup
    , MatchSetupData
    , MatchSetupMsg(..)
    , allUsers
    , init
    , isOwner
    , joinUser
    , leaveUser
    , matchSetupUpdate
    , preview
    )

import AssocSet as Set exposing (Set)
import Id exposing (Id)
import List.Nonempty exposing (Nonempty(..))
import User exposing (UserId)


type MatchSetup
    = MatchSetup MatchSetupData


type alias LobbyPreview =
    { name : String, userCount : Int }


type alias MatchSetupData =
    { name : String
    , owner : Id UserId
    , users : Set (Id UserId)
    }


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup


init : String -> Id UserId -> MatchSetup
init name owner =
    { name = name
    , owner = owner
    , users = Set.empty
    }
        |> MatchSetup


joinUser : Id UserId -> MatchSetup -> MatchSetup
joinUser userId (MatchSetup lobby) =
    (if userId == lobby.owner then
        lobby

     else
        { lobby | users = Set.insert userId lobby.users }
    )
        |> MatchSetup


leaveUser : Id UserId -> MatchSetup -> Maybe MatchSetup
leaveUser userId (MatchSetup lobby) =
    if userId == lobby.owner then
        let
            users =
                Set.toList lobby.users
        in
        case users of
            newOwner :: _ ->
                { lobby | owner = newOwner, users = List.drop 1 users |> Set.fromList } |> MatchSetup |> Just

            [] ->
                Nothing

    else
        { lobby | users = Set.remove userId lobby.users } |> MatchSetup |> Just


isOwner : Id UserId -> MatchSetup -> Bool
isOwner userId (MatchSetup lobby) =
    lobby.owner == userId


preview : MatchSetup -> LobbyPreview
preview (MatchSetup lobby) =
    { name = lobby.name, userCount = Set.size lobby.users + 1 }


allUsers : MatchSetup -> Nonempty (Id UserId)
allUsers (MatchSetup lobby) =
    Nonempty lobby.owner (Set.toList lobby.users)


matchSetupUpdate : { userId : Id UserId, msg : MatchSetupMsg } -> MatchSetup -> MatchSetup
matchSetupUpdate { userId, msg } lobby =
    case msg of
        JoinMatchSetup ->
            joinUser userId lobby

        LeaveMatchSetup ->
            leaveUser userId lobby |> Maybe.withDefault lobby
