module MatchSetup exposing
    ( LobbyPreview
    , Match
    , MatchSetup
    , MatchSetupData
    , MatchSetupMsg(..)
    , MatchState
    , Player
    , PlayerData
    , PlayerMode(..)
    , TimelineEvent
    , WorldCoordinate
    , allUsers
    , allUsers_
    , getMatch
    , init
    , isOwner
    , joinUser
    , matchSetupUpdate
    , name
    , preview
    )

import Angle exposing (Angle)
import AssocList as Dict exposing (Dict)
import AssocSet as Set
import ColorIndex exposing (ColorIndex(..))
import Decal exposing (Decal)
import Direction2d exposing (Direction2d)
import Id exposing (Id)
import Length exposing (Meters)
import List.Nonempty exposing (Nonempty(..))
import MatchName exposing (MatchName)
import Point2d exposing (Point2d)
import Time
import Timeline exposing (FrameId, Timeline)
import User exposing (UserId)
import Vector2d exposing (Vector2d)


type MatchSetup
    = MatchSetup MatchSetupData


type alias LobbyPreview =
    { name : MatchName, userCount : Int }


type alias MatchSetupData =
    { name : MatchName
    , owner : Id UserId
    , ownerPlayerData : PlayerData
    , users : Dict (Id UserId) PlayerData
    , match : Maybe Match
    }


type alias TimelineEvent =
    { userId : Id UserId, input : Maybe (Direction2d WorldCoordinate) }


type alias MatchState =
    { players : Dict (Id UserId) Player }


type alias Player =
    { position : Point2d Meters WorldCoordinate
    , velocity : Vector2d Meters WorldCoordinate
    , rotationalVelocity : Angle
    , rotation : Angle
    , input : Maybe (Direction2d WorldCoordinate)
    , finishTime : Maybe (Id FrameId)
    , lastCollision : Maybe (Id FrameId)
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias PlayerData =
    { primaryColor : ColorIndex, secondaryColor : ColorIndex, decal : Decal, mode : PlayerMode }


type alias Match =
    { startTime : Time.Posix, timeline : Timeline TimelineEvent }


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor ColorIndex
    | SetSecondaryColor ColorIndex
    | SetDecal Decal
    | SetPlayerMode PlayerMode
    | StartMatch Time.Posix
    | MatchInputRequest (Id FrameId) (Maybe (Direction2d WorldCoordinate))
    | SetMatchName MatchName


type PlayerMode
    = PlayerMode
    | SpectatorMode


init : Id UserId -> MatchSetup
init owner =
    { name = MatchName.empty
    , owner = owner
    , ownerPlayerData = defaultPlayerData
    , users = Dict.empty
    , match = Nothing
    }
        |> MatchSetup


defaultPlayerData : PlayerData
defaultPlayerData =
    { primaryColor = Blue, secondaryColor = Green, decal = Decal.Star, mode = PlayerMode }


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


getMatch : MatchSetup -> Maybe { startTime : Time.Posix, timeline : Timeline TimelineEvent }
getMatch (MatchSetup matchSetup) =
    matchSetup.match


name : MatchSetup -> MatchName
name (MatchSetup matchSetup) =
    matchSetup.name


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


matchSetupUpdate : { userId : Id UserId, msg : MatchSetupMsg } -> MatchSetup -> Maybe MatchSetup
matchSetupUpdate { userId, msg } lobby =
    case msg of
        JoinMatchSetup ->
            joinUser userId lobby |> Just

        LeaveMatchSetup ->
            leaveUser userId lobby

        SetPrimaryColor colorIndex ->
            updatePlayerData userId (\a -> { a | primaryColor = colorIndex }) lobby |> Just

        SetSecondaryColor colorIndex ->
            updatePlayerData userId (\a -> { a | secondaryColor = colorIndex }) lobby |> Just

        SetDecal decal ->
            updatePlayerData userId (\a -> { a | decal = decal }) lobby |> Just

        SetPlayerMode mode ->
            updatePlayerData userId (\a -> { a | mode = mode }) lobby |> Just

        StartMatch time ->
            startMatch time userId lobby |> Just

        MatchInputRequest frameId input ->
            addInput userId frameId input lobby |> Just

        SetMatchName matchName ->
            if isOwner userId lobby then
                setMatchName matchName lobby |> Just

            else
                Just lobby


startMatch : Time.Posix -> Id UserId -> MatchSetup -> MatchSetup
startMatch time userId (MatchSetup matchSetup) =
    if matchSetup.owner == userId then
        { matchSetup | match = Just { startTime = time, timeline = Set.empty } }
            |> MatchSetup

    else
        MatchSetup matchSetup


setMatchName : MatchName -> MatchSetup -> MatchSetup
setMatchName matchName (MatchSetup matchSetup) =
    { matchSetup | name = matchName } |> MatchSetup


addInput : Id UserId -> Id FrameId -> Maybe (Direction2d WorldCoordinate) -> MatchSetup -> MatchSetup
addInput userId frameId input (MatchSetup matchSetup) =
    { matchSetup
        | match =
            case ( allUsers_ (MatchSetup matchSetup) |> Dict.get userId, matchSetup.match ) of
                ( Just playerData, Just match ) ->
                    case playerData.mode of
                        PlayerMode ->
                            Just { match | timeline = Set.insert ( frameId, { userId = userId, input = input } ) match.timeline }

                        SpectatorMode ->
                            matchSetup.match

                _ ->
                    matchSetup.match
    }
        |> MatchSetup


updatePlayerData : Id UserId -> (PlayerData -> PlayerData) -> MatchSetup -> MatchSetup
updatePlayerData userId updateFunc (MatchSetup matchSetup) =
    (if userId == matchSetup.owner then
        { matchSetup | ownerPlayerData = updateFunc matchSetup.ownerPlayerData }

     else
        { matchSetup | users = Dict.update userId (Maybe.map updateFunc) matchSetup.users }
    )
        |> MatchSetup
