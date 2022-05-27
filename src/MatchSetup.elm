module MatchSetup exposing
    ( LobbyPreview
    , MatchSetup
    , MatchSetupData
    , MatchSetupMsg(..)
    , PlayerData
    , allUsers
    , allUsers_
    , init
    , isOwner
    , joinUser
    , matchSetupUpdate
    , preview
    )

import AssocList as Dict exposing (Dict)
import ColorIndex exposing (ColorIndex(..))
import Decal exposing (Decal)
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
    , ownerPlayerData : PlayerData
    , users : Dict (Id UserId) PlayerData
    }


type alias PlayerData =
    { primaryColor : ColorIndex, secondaryColor : ColorIndex, decal : Decal }


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor ColorIndex
    | SetSecondaryColor ColorIndex
    | SetDecal Decal


init : String -> Id UserId -> MatchSetup
init name owner =
    { name = name
    , owner = owner
    , ownerPlayerData = defaultPlayerData
    , users = Dict.empty
    }
        |> MatchSetup


defaultPlayerData =
    { primaryColor = Blue, secondaryColor = Green, decal = Decal.Star }


joinUser : Id UserId -> MatchSetup -> MatchSetup
joinUser userId (MatchSetup lobby) =
    (if userId == lobby.owner then
        lobby

     else
        { lobby | users = Dict.insert userId defaultPlayerData lobby.users }
    )
        |> MatchSetup


leaveUser : Id UserId -> MatchSetup -> Maybe MatchSetup
leaveUser userId (MatchSetup lobby) =
    if userId == lobby.owner then
        let
            users =
                Dict.toList lobby.users
        in
        case users of
            ( newOwner, newOwnerPlayerData ) :: _ ->
                { lobby
                    | owner = newOwner
                    , ownerPlayerData = newOwnerPlayerData
                    , users = List.drop 1 users |> Dict.fromList
                }
                    |> MatchSetup
                    |> Just

            [] ->
                Nothing

    else
        { lobby | users = Dict.remove userId lobby.users } |> MatchSetup |> Just


isOwner : Id UserId -> MatchSetup -> Bool
isOwner userId (MatchSetup lobby) =
    lobby.owner == userId


preview : MatchSetup -> LobbyPreview
preview (MatchSetup lobby) =
    { name = lobby.name, userCount = Dict.size lobby.users + 1 }


allUsers : MatchSetup -> Nonempty ( Id UserId, PlayerData )
allUsers (MatchSetup lobby) =
    Nonempty ( lobby.owner, lobby.ownerPlayerData ) (Dict.toList lobby.users)


allUsers_ : MatchSetup -> Dict (Id UserId) PlayerData
allUsers_ (MatchSetup lobby) =
    Dict.insert lobby.owner lobby.ownerPlayerData lobby.users


matchSetupUpdate : { userId : Id UserId, msg : MatchSetupMsg } -> MatchSetup -> MatchSetup
matchSetupUpdate { userId, msg } lobby =
    case msg of
        JoinMatchSetup ->
            joinUser userId lobby

        LeaveMatchSetup ->
            leaveUser userId lobby |> Maybe.withDefault lobby

        SetPrimaryColor colorIndex ->
            updatePlayerData userId (\a -> { a | primaryColor = colorIndex }) lobby

        SetSecondaryColor colorIndex ->
            updatePlayerData userId (\a -> { a | secondaryColor = colorIndex }) lobby

        SetDecal decal ->
            updatePlayerData userId (\a -> { a | decal = decal }) lobby


updatePlayerData : Id UserId -> (PlayerData -> PlayerData) -> MatchSetup -> MatchSetup
updatePlayerData userId updateFunc (MatchSetup matchSetup) =
    (if userId == matchSetup.owner then
        { matchSetup | ownerPlayerData = updateFunc matchSetup.ownerPlayerData }

     else
        { matchSetup | users = Dict.update userId (Maybe.map updateFunc) matchSetup.users }
    )
        |> MatchSetup
