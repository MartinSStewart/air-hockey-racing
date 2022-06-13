module MatchSetup exposing
    ( LobbyPreview
    , Match
    , MatchSetup
    , MatchSetupMsg(..)
    , MatchState
    , Place(..)
    , Player
    , PlayerData
    , PlayerMode(..)
    , ServerTime(..)
    , TimelineEvent
    , WorldCoordinate
    , allUsers
    , allUsers_
    , clampTime
    , frameDuration
    , getMatch
    , init
    , isOwner
    , joinUser
    , leaveUser
    , matchSetupUpdate
    , maxInputDelay
    , maxPlayers
    , messagesOldestToNewest
    , name
    , preview
    , previousMatchFinishTimes
    , serverTimeToFrameId
    , unwrapServerTime
    )

import Angle exposing (Angle)
import AssocList as Dict exposing (Dict)
import AssocSet as Set
import ColorIndex exposing (ColorIndex(..))
import Decal exposing (Decal)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Id exposing (Id)
import Length exposing (Meters)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import MatchName exposing (MatchName)
import Point2d exposing (Point2d)
import Quantity
import Random
import TextMessage exposing (TextMessage)
import Time
import Timeline exposing (FrameId, Timeline)
import User exposing (UserId)
import Vector2d exposing (Vector2d)


type MatchSetup
    = MatchSetup MatchSetupData


type alias LobbyPreview =
    { name : MatchName, userCount : Int, maxUserCount : Int }


type alias MatchSetupData =
    { name : MatchName
    , owner : Id UserId
    , ownerPlayerData : PlayerData
    , users : Dict (Id UserId) PlayerData
    , match : Maybe Match
    , messages : List { userId : Id UserId, message : TextMessage }
    , previousMatch : Maybe (Dict (Id UserId) Place)
    , maxPlayers : Int
    }


type Place
    = Finished (Id FrameId)
    | DidNotFinish


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
    , finishTime : Place
    , lastCollision : Maybe (Id FrameId)
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias PlayerData =
    { primaryColor : ColorIndex, secondaryColor : ColorIndex, decal : Maybe Decal, mode : PlayerMode }


type alias Match =
    { startTime : ServerTime, timeline : Timeline TimelineEvent }


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor ColorIndex
    | SetSecondaryColor ColorIndex
    | SetDecal (Maybe Decal)
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Direction2d WorldCoordinate))
    | SetMatchName MatchName
    | SendTextMessage TextMessage
    | MatchFinished (Dict (Id UserId) Place)
    | SetMaxPlayers Int


type ServerTime
    = ServerTime Time.Posix


previousMatchFinishTimes : MatchSetup -> Maybe (Dict (Id UserId) Place)
previousMatchFinishTimes (MatchSetup matchSetup) =
    matchSetup.previousMatch


unwrapServerTime : ServerTime -> Time.Posix
unwrapServerTime (ServerTime serverTime) =
    serverTime


clampTime : ServerTime -> ServerTime -> ServerTime
clampTime (ServerTime currentTime) (ServerTime time) =
    clamp
        (Time.posixToMillis (Duration.subtractFrom currentTime maxInputDelay))
        (Time.posixToMillis currentTime)
        (Time.posixToMillis time)
        |> Time.millisToPosix
        |> ServerTime


type PlayerMode
    = PlayerMode
    | SpectatorMode


maxInputDelay : Duration
maxInputDelay =
    Duration.second


maxPlayers : MatchSetup -> Int
maxPlayers (MatchSetup matchSetup) =
    matchSetup.maxPlayers


init : Id UserId -> MatchSetup
init owner =
    { name = MatchName.empty
    , owner = owner
    , ownerPlayerData = initPlayerData owner
    , users = Dict.empty
    , match = Nothing
    , messages = []
    , previousMatch = Nothing
    , maxPlayers = 16
    }
        |> MatchSetup


initPlayerData : Id UserId -> PlayerData
initPlayerData userId =
    let
        randomData =
            Random.map2
                (\( primary, secondary ) decal ->
                    { primaryColor = primary
                    , secondaryColor = secondary
                    , decal = Just decal
                    , mode = PlayerMode
                    }
                )
                (List.Nonempty.sample ColorIndex.allColors
                    |> Random.andThen
                        (\primaryColor ->
                            (case ColorIndex.allColors |> List.Nonempty.toList |> List.remove primaryColor of
                                head :: rest ->
                                    Random.uniform head rest

                                [] ->
                                    Random.constant Red
                            )
                                |> Random.map (\secondaryColor -> ( primaryColor, secondaryColor ))
                        )
                )
                (List.Nonempty.sample Decal.allDecals)
    in
    Random.step randomData (Random.initialSeed (Id.toInt userId + 3)) |> Tuple.first


joinUser : Id UserId -> MatchSetup -> MatchSetup
joinUser userId (MatchSetup lobby) =
    (if userId == lobby.owner then
        lobby

     else
        { lobby | users = Dict.insert userId (initPlayerData userId) lobby.users }
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


getMatch : MatchSetup -> Maybe { startTime : ServerTime, timeline : Timeline TimelineEvent }
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
    { name = lobby.name, userCount = Dict.size lobby.users + 1, maxUserCount = lobby.maxPlayers }


allUsers : MatchSetup -> Nonempty ( Id UserId, PlayerData )
allUsers (MatchSetup lobby) =
    Nonempty ( lobby.owner, lobby.ownerPlayerData ) (Dict.toList lobby.users)


allUsers_ : MatchSetup -> Dict (Id UserId) PlayerData
allUsers_ (MatchSetup lobby) =
    Dict.insert lobby.owner lobby.ownerPlayerData lobby.users


messagesOldestToNewest : MatchSetup -> List { userId : Id UserId, message : TextMessage }
messagesOldestToNewest (MatchSetup matchSetup) =
    List.reverse matchSetup.messages


matchSetupUpdate : { userId : Id UserId, msg : MatchSetupMsg } -> MatchSetup -> MatchSetup
matchSetupUpdate { userId, msg } match =
    case msg of
        JoinMatchSetup ->
            joinUser userId match

        LeaveMatchSetup ->
            leaveUser userId match |> Maybe.withDefault match

        SetPrimaryColor colorIndex ->
            updatePlayerData userId (\a -> { a | primaryColor = colorIndex }) match

        SetSecondaryColor colorIndex ->
            updatePlayerData userId (\a -> { a | secondaryColor = colorIndex }) match

        SetDecal decal ->
            updatePlayerData userId (\a -> { a | decal = decal }) match

        SetPlayerMode mode ->
            updatePlayerData userId (\a -> { a | mode = mode }) match

        StartMatch time ->
            startMatch time userId match

        MatchInputRequest serverTime input ->
            addInput userId serverTime input match

        SetMatchName matchName ->
            if isOwner userId match then
                setMatchName matchName match

            else
                match

        SendTextMessage message ->
            sendTextMessage userId message match

        MatchFinished placements ->
            matchFinished placements match

        SetMaxPlayers maxPlayerCount ->
            setMaxPlayers maxPlayerCount match


setMaxPlayers : Int -> MatchSetup -> MatchSetup
setMaxPlayers maxPlayerCount (MatchSetup matchSetup) =
    MatchSetup { matchSetup | maxPlayers = maxPlayerCount }


matchFinished : Dict (Id UserId) Place -> MatchSetup -> MatchSetup
matchFinished placements (MatchSetup matchSetup) =
    (case matchSetup.match of
        Just _ ->
            { matchSetup | match = Nothing, previousMatch = Just placements }

        Nothing ->
            matchSetup
    )
        |> MatchSetup


sendTextMessage : Id UserId -> TextMessage -> MatchSetup -> MatchSetup
sendTextMessage userId message (MatchSetup match) =
    { match | messages = { userId = userId, message = message } :: match.messages } |> MatchSetup


startMatch : ServerTime -> Id UserId -> MatchSetup -> MatchSetup
startMatch time userId (MatchSetup matchSetup) =
    let
        totalPlayers : Int
        totalPlayers =
            allUsers (MatchSetup matchSetup)
                |> List.Nonempty.toList
                |> List.count (\( _, player ) -> player.mode == PlayerMode)
    in
    if matchSetup.owner == userId && totalPlayers > 0 then
        { matchSetup | match = Just { startTime = time, timeline = Set.empty } }
            |> MatchSetup

    else
        MatchSetup matchSetup


setMatchName : MatchName -> MatchSetup -> MatchSetup
setMatchName matchName (MatchSetup matchSetup) =
    { matchSetup | name = matchName } |> MatchSetup


frameDuration : Duration
frameDuration =
    Duration.seconds (1 / 60)


serverTimeToFrameId : ServerTime -> Match -> Id FrameId
serverTimeToFrameId time match =
    time
        |> unwrapServerTime
        |> Duration.from (unwrapServerTime match.startTime)
        |> (\a -> Quantity.ratio a frameDuration)
        |> round
        |> Id.fromInt


addInput : Id UserId -> ServerTime -> Maybe (Direction2d WorldCoordinate) -> MatchSetup -> MatchSetup
addInput userId serverTime input (MatchSetup matchSetup) =
    { matchSetup
        | match =
            case ( allUsers_ (MatchSetup matchSetup) |> Dict.get userId, matchSetup.match ) of
                ( Just playerData, Just match ) ->
                    case playerData.mode of
                        PlayerMode ->
                            let
                                newFrameId =
                                    serverTimeToFrameId serverTime match
                            in
                            Just
                                { match
                                    | timeline =
                                        Set.insert
                                            ( newFrameId, { userId = userId, input = input } )
                                            match.timeline
                                            |> Set.filter
                                                (\( frameId_, _ ) ->
                                                    Id.toInt newFrameId - Timeline.maxCacheSize < Id.toInt frameId_
                                                )
                                }

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
