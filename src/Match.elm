module Match exposing
    ( Emote(..)
    , Input
    , LobbyPreview
    , Match
    , MatchActive
    , MatchState
    , Msg(..)
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
    , init
    , isOwner
    , joinUser
    , leaveUser
    , matchActive
    , matchSetupUpdate
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


type Match
    = Match Match_


type alias LobbyPreview =
    { name : MatchName, userCount : Int, maxUserCount : Int }


type alias Match_ =
    { name : MatchName
    , owner : Id UserId
    , ownerPlayerData : PlayerData
    , users : Dict (Id UserId) PlayerData
    , matchActive : Maybe MatchActive
    , messages : List { userId : Id UserId, message : TextMessage }
    , previousMatch : Maybe (Dict (Id UserId) Place)
    , maxPlayers : Int
    }


type Place
    = Finished (Id FrameId)
    | DidNotFinish


type alias TimelineEvent =
    { userId : Id UserId, input : Input }


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
    , lastEmote : Maybe { time : Id FrameId, emote : Emote }
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias PlayerData =
    { primaryColor : ColorIndex, secondaryColor : ColorIndex, decal : Maybe Decal, mode : PlayerMode }


type alias MatchActive =
    { startTime : ServerTime, timeline : Timeline TimelineEvent }


type Emote
    = SurpriseEmote
    | ImpEmote


type alias Input =
    { movement : Maybe (Direction2d WorldCoordinate)
    , emote : Maybe Emote
    }


type Msg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor ColorIndex
    | SetSecondaryColor ColorIndex
    | SetDecal (Maybe Decal)
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime Input
    | SetMatchName MatchName
    | SendTextMessage TextMessage
    | MatchFinished (Dict (Id UserId) Place)
    | SetMaxPlayers Int


type ServerTime
    = ServerTime Time.Posix


previousMatchFinishTimes : Match -> Maybe (Dict (Id UserId) Place)
previousMatchFinishTimes (Match matchSetup) =
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


maxPlayers : Match -> Int
maxPlayers (Match matchSetup) =
    matchSetup.maxPlayers


init : Id UserId -> Match
init owner =
    { name = MatchName.empty
    , owner = owner
    , ownerPlayerData = initPlayerData owner
    , users = Dict.empty
    , matchActive = Nothing
    , messages = []
    , previousMatch = Nothing
    , maxPlayers = 16
    }
        |> Match


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


joinUser : Id UserId -> Match -> Match
joinUser userId (Match lobby) =
    (if userId == lobby.owner then
        lobby

     else
        { lobby | users = Dict.insert userId (initPlayerData userId) lobby.users }
    )
        |> Match


leaveUser : Id UserId -> Match -> Maybe Match
leaveUser userId (Match lobby) =
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
                    |> Match
                    |> Just

            [] ->
                Nothing

    else
        { lobby | users = Dict.remove userId lobby.users } |> Match |> Just


matchActive : Match -> Maybe { startTime : ServerTime, timeline : Timeline TimelineEvent }
matchActive (Match matchSetup) =
    matchSetup.matchActive


name : Match -> MatchName
name (Match matchSetup) =
    matchSetup.name


isOwner : Id UserId -> Match -> Bool
isOwner userId (Match lobby) =
    lobby.owner == userId


preview : Match -> LobbyPreview
preview (Match lobby) =
    { name = lobby.name, userCount = Dict.size lobby.users + 1, maxUserCount = lobby.maxPlayers }


allUsers : Match -> Nonempty ( Id UserId, PlayerData )
allUsers (Match lobby) =
    Nonempty ( lobby.owner, lobby.ownerPlayerData ) (Dict.toList lobby.users)


allUsers_ : Match -> Dict (Id UserId) PlayerData
allUsers_ (Match lobby) =
    Dict.insert lobby.owner lobby.ownerPlayerData lobby.users


messagesOldestToNewest : Match -> List { userId : Id UserId, message : TextMessage }
messagesOldestToNewest (Match matchSetup) =
    List.reverse matchSetup.messages


matchSetupUpdate : { userId : Id UserId, msg : Msg } -> Match -> Match
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


setMaxPlayers : Int -> Match -> Match
setMaxPlayers maxPlayerCount (Match matchSetup) =
    Match { matchSetup | maxPlayers = maxPlayerCount }


matchFinished : Dict (Id UserId) Place -> Match -> Match
matchFinished placements (Match matchSetup) =
    (case matchSetup.matchActive of
        Just _ ->
            { matchSetup | matchActive = Nothing, previousMatch = Just placements }

        Nothing ->
            matchSetup
    )
        |> Match


sendTextMessage : Id UserId -> TextMessage -> Match -> Match
sendTextMessage userId message (Match match) =
    { match | messages = { userId = userId, message = message } :: match.messages } |> Match


startMatch : ServerTime -> Id UserId -> Match -> Match
startMatch time userId (Match matchSetup) =
    let
        totalPlayers : Int
        totalPlayers =
            allUsers (Match matchSetup)
                |> List.Nonempty.toList
                |> List.count (\( _, player ) -> player.mode == PlayerMode)
    in
    if matchSetup.owner == userId && totalPlayers > 0 then
        { matchSetup | matchActive = Just { startTime = time, timeline = Set.empty } }
            |> Match

    else
        Match matchSetup


setMatchName : MatchName -> Match -> Match
setMatchName matchName (Match matchSetup) =
    { matchSetup | name = matchName } |> Match


frameDuration : Duration
frameDuration =
    Duration.seconds (1 / 60)


serverTimeToFrameId : ServerTime -> MatchActive -> Id FrameId
serverTimeToFrameId time match =
    time
        |> unwrapServerTime
        |> Duration.from (unwrapServerTime match.startTime)
        |> (\a -> Quantity.ratio a frameDuration)
        |> round
        |> Id.fromInt


addInput : Id UserId -> ServerTime -> Input -> Match -> Match
addInput userId serverTime input (Match matchSetup) =
    { matchSetup
        | matchActive =
            case ( allUsers_ (Match matchSetup) |> Dict.get userId, matchSetup.matchActive ) of
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
                            matchSetup.matchActive

                _ ->
                    matchSetup.matchActive
    }
        |> Match


updatePlayerData : Id UserId -> (PlayerData -> PlayerData) -> Match -> Match
updatePlayerData userId updateFunc (Match matchSetup) =
    (if userId == matchSetup.owner then
        { matchSetup | ownerPlayerData = updateFunc matchSetup.ownerPlayerData }

     else
        { matchSetup | users = Dict.update userId (Maybe.map updateFunc) matchSetup.users }
    )
        |> Match
