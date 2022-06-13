module Frontend exposing (app)

import Angle exposing (Angle)
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Audio exposing (Audio, AudioCmd, AudioData)
import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Navigation
import Collision
import ColorIndex exposing (ColorIndex)
import Decal
import Dict as RegularDict
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Effect.Browser.Dom exposing (HtmlId)
import Effect.Browser.Events
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task
import Effect.Time
import Effect.WebGL as WebGL exposing (Mesh, Shader)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Touch
import Id exposing (Id)
import Json.Decode
import Keyboard exposing (Key)
import Keyboard.Arrows
import Lamdera
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import MatchName
import MatchSetup exposing (LobbyPreview, Match, MatchSetup, MatchSetupMsg, MatchState, Place(..), Player, PlayerData, PlayerMode(..), ServerTime(..), TimelineEvent, WorldCoordinate)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import NetworkModel exposing (NetworkModel)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Ports
import Quantity exposing (Quantity(..), Rate)
import Random
import Random.List as Random
import RasterShapes
import Sounds exposing (Sounds)
import TextMessage
import Time
import Timeline exposing (FrameId, TimelineCache)
import Types exposing (..)
import Ui
import Url exposing (Url)
import Url.Parser exposing ((<?>))
import Url.Parser.Query
import User exposing (UserId)
import Vector2d exposing (Vector2d)
import WebGL.Settings


app :
    { init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : Browser.UrlRequest -> FrontendMsg
    , onUrlChange : Url -> FrontendMsg
    }
app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        (Audio.lamderaFrontendWithAudio
            { init = init
            , onUrlRequest = UrlClicked
            , onUrlChange = UrlChanged
            , update = update
            , updateFromBackend = updateFromBackend
            , subscriptions = subscriptions
            , view = view
            , audio = audio
            , audioPort =
                { fromJS = Ports.audioFromJs
                , toJS = Ports.audioToJs
                }
            }
        )


getLocalState : MatchSetupPage_ -> MatchSetup
getLocalState matchPage =
    NetworkModel.localState
        (\a b -> MatchSetup.matchSetupUpdate a b |> Maybe.withDefault b)
        matchPage.networkModel


audio : AudioData -> FrontendModel_ -> Audio
audio audioData model =
    (case model of
        Loading _ ->
            Audio.silence

        Loaded loaded ->
            case loaded.page of
                MatchPage matchPage ->
                    --case ( MatchSetup.getMatch (getLocalState matchPage), matchPage.matchData ) of
                    --    ( Just match, MatchData matchData ) ->
                    --        let
                    --            ( _, state ) =
                    --                Timeline.getStateAt
                    --                    gameUpdate
                    --                    (timeToFrameId loaded match)
                    --                    matchData.timelineCache
                    --                    match.timeline
                    --        in
                    --        Dict.values state.players
                    --            |> List.filterMap .lastCollision
                    --            |> Set.fromList
                    --            |> Set.toList
                    --            |> List.map
                    --                (\frameId ->
                    --                    let
                    --                        collisionTime : Time.Posix
                    --                        collisionTime =
                    --                            Quantity.multiplyBy (Id.toInt frameId |> toFloat) MatchSetup.frameDuration
                    --                                |> Duration.addTo (MatchSetup.unwrapServerTime match.startTime)
                    --                                |> (\a -> Duration.subtractFrom a (pingOffset loaded))
                    --                                |> (\a -> Duration.subtractFrom a loaded.debugTimeOffset)
                    --                    in
                    --                    Audio.audio loaded.sounds.collision collisionTime
                    --                )
                    --            |> Audio.group
                    --
                    --    _ ->
                    Audio.silence

                LobbyPage _ ->
                    Audio.silence
    )
        |> Audio.offsetBy (Duration.milliseconds 30)


loadedInit :
    FrontendLoading
    -> Time.Posix
    -> Sounds
    -> ( Id UserId, LobbyData )
    -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit loading time sounds ( userId, lobbyData ) =
    ( { key = loading.key
      , currentKeys = []
      , previousKeys = []
      , windowSize = loading.windowSize
      , devicePixelRatio = loading.devicePixelRatio
      , time = time
      , debugTimeOffset = loading.debugTimeOffset
      , page = LobbyPage lobbyData
      , sounds = sounds
      , userId = userId
      , pingStartTime = Nothing
      , pingData = Nothing
      }
        |> (\a -> { a | pingStartTime = actualTime a |> Just })
        |> Loaded
    , Effect.Lamdera.sendToBackend PingRequest
    , Audio.cmdNone
    )


tryLoadedInit : FrontendLoading -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
tryLoadedInit loading =
    Maybe.map3
        (loadedInit loading)
        loading.time
        (Sounds.loadingFinished loading.sounds)
        loading.initData
        |> Maybe.withDefault ( Loading loading, Command.none, Audio.cmdNone )


urlParser : Url.Parser.Parser (Maybe Int -> b) b
urlParser =
    Url.Parser.top <?> Url.Parser.Query.int "offset"


init : Url -> Effect.Browser.Navigation.Key -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
init url key =
    let
        offset =
            case Url.Parser.parse urlParser url of
                Just (Just offset_) ->
                    Duration.milliseconds (toFloat offset_)

                _ ->
                    Quantity.zero
    in
    ( Loading
        { key = key
        , windowSize = { width = Pixels.pixels 1920, height = Pixels.pixels 1080 }
        , devicePixelRatio = Quantity 1
        , time = Nothing
        , initData = Nothing
        , sounds = Dict.empty
        , debugTimeOffset = offset
        }
    , Command.batch
        [ Task.perform
            (\{ viewport } ->
                WindowResized
                    { width = round viewport.width |> Pixels.pixels
                    , height = round viewport.height |> Pixels.pixels
                    }
            )
            Effect.Browser.Dom.getViewport
        , Effect.Time.now |> Task.perform GotTime
        ]
    , Sounds.requestSounds SoundLoaded
    )


update : AudioData -> FrontendMsg_ -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
update audioData msg model =
    case model of
        Loading loadingModel ->
            case msg of
                WindowResized windowSize ->
                    windowResizedUpdate windowSize loadingModel |> (\( a, b ) -> ( Loading a, b, Audio.cmdNone ))

                GotDevicePixelRatio devicePixelRatio ->
                    devicePixelRatioUpdate devicePixelRatio loadingModel
                        |> (\( a, b ) -> ( Loading a, b, Audio.cmdNone ))

                SoundLoaded url result ->
                    { loadingModel | sounds = Dict.insert url result loadingModel.sounds }
                        |> tryLoadedInit

                GotTime time ->
                    { loadingModel | time = Just time }
                        |> tryLoadedInit

                _ ->
                    ( model, Command.none, Audio.cmdNone )

        Loaded frontendLoaded ->
            updateLoaded msg frontendLoaded
                |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))


updateLoaded : FrontendMsg_ -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoaded msg model =
    case msg of
        RandomInput _ ->
            ( { model
                | currentKeys =
                    if List.any ((==) Keyboard.ArrowUp) model.currentKeys then
                        List.remove Keyboard.ArrowUp model.currentKeys

                    else
                        Keyboard.ArrowUp :: model.currentKeys
              }
            , Command.none
            )

        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Command.batch [ Effect.Browser.Navigation.pushUrl model.key (Url.toString url) ]
                    )

                Browser.External url ->
                    ( model
                    , Effect.Browser.Navigation.load url
                    )

        UrlChanged _ ->
            ( model, Command.none )

        NoOpFrontendMsg ->
            ( model, Command.none )

        KeyMsg keyMsg ->
            ( { model | currentKeys = Keyboard.update keyMsg model.currentKeys }
            , Command.none
            )

        WindowResized windowSize ->
            windowResizedUpdate windowSize model

        GotDevicePixelRatio devicePixelRatio ->
            devicePixelRatioUpdate devicePixelRatio model

        AnimationFrame time_ ->
            let
                model2 =
                    { model | time = time_, previousKeys = model.currentKeys }
            in
            case model2.page of
                MatchPage matchSetupPage ->
                    case ( matchSetupPage.matchData, MatchSetup.getMatch (getLocalState matchSetupPage) ) of
                        ( MatchData matchData, Just match ) ->
                            case matchData.timelineCache of
                                Ok cache ->
                                    let
                                        previousInput : Maybe (Direction2d WorldCoordinate)
                                        previousInput =
                                            getInputDirection model.windowSize model.previousKeys matchData.previousTouchPosition

                                        input : Maybe (Direction2d WorldCoordinate)
                                        input =
                                            getInputDirection model.windowSize model.currentKeys matchData.touchPosition

                                        inputUnchanged : Bool
                                        inputUnchanged =
                                            previousInput == input

                                        model3 : FrontendLoaded
                                        model3 =
                                            { model2
                                                | page =
                                                    { matchSetupPage
                                                        | matchData =
                                                            { matchData
                                                                | previousTouchPosition = matchData.touchPosition
                                                                , timelineCache = Ok newCache
                                                            }
                                                                |> MatchData
                                                    }
                                                        |> MatchPage
                                            }

                                        ( newCache, matchState ) =
                                            Timeline.getStateAt
                                                gameUpdate
                                                (timeToFrameId model2 match)
                                                cache
                                                match.timeline

                                        currentFrameId =
                                            timeToFrameId model3 match
                                    in
                                    (if inputUnchanged then
                                        ( model3, Command.none )

                                     else
                                        matchSetupUpdate (MatchSetup.MatchInputRequest (timeToServerTime model3) input) model3
                                    )
                                        |> (\( model4, cmd ) ->
                                                case
                                                    ( matchTimeLeft currentFrameId matchState
                                                    , matchTimeLeft (Id.decrement currentFrameId) matchState
                                                    )
                                                of
                                                    ( Just timeLeft, Just previousTimeLeft ) ->
                                                        if Quantity.lessThanZero timeLeft && not (Quantity.lessThanZero previousTimeLeft) then
                                                            matchSetupUpdate
                                                                (MatchSetup.MatchFinished
                                                                    (Dict.map
                                                                        (\_ player -> player.finishTime)
                                                                        matchState.players
                                                                    )
                                                                )
                                                                model4
                                                                |> Tuple.mapSecond (\cmd2 -> Command.batch [ cmd, cmd2, scrollToBottom ])

                                                        else
                                                            ( model4, cmd )

                                                    _ ->
                                                        ( model4, cmd )
                                           )

                                Err _ ->
                                    ( model2, Command.none )

                        _ ->
                            ( model2, Command.none )

                --( { model2
                --    | page =
                --        MatchPage
                --            { match
                --                | timeline = newTimeline
                --                , timelineCache = cache
                --                , zoom = newZoom
                --                , previousTouchPosition = match.touchPosition
                --            }
                --  }
                --, if inputUnchanged then
                --    Command.none
                --
                --  else
                --    MatchInputRequest match.matchId (timeToFrameId model match) input
                --        |> Effect.Lamdera.sendToBackend
                --)
                LobbyPage _ ->
                    ( model2, Command.none )

        PressedCreateLobby ->
            ( model, Effect.Lamdera.sendToBackend CreateMatchRequest )

        PressedJoinLobby lobbyId ->
            ( model
            , MatchSetupRequest lobbyId (Id.fromInt -1) MatchSetup.JoinMatchSetup |> Effect.Lamdera.sendToBackend
            )

        SoundLoaded _ _ ->
            -- Shouldn't happen
            ( model, Command.none )

        GotTime _ ->
            ( model, Command.none )

        MatchMsg matchMsg ->
            case model.page of
                MatchPage matchPage ->
                    ( { model
                        | page =
                            { matchPage
                                | matchData =
                                    case matchPage.matchData of
                                        MatchData matchData ->
                                            matchUpdate matchMsg matchData |> MatchData

                                        MatchSetupData _ ->
                                            matchPage.matchData
                            }
                                |> MatchPage
                      }
                    , Command.none
                    )

                _ ->
                    ( model, Command.none )

        MatchSetupMsg matchSetupMsg_ ->
            case model.page of
                MatchPage matchPage ->
                    case matchSetupMsg_ of
                        PressedStartMatchSetup ->
                            matchSetupUpdate (MatchSetup.StartMatch (timeToServerTime model)) model

                        PressedLeaveMatchSetup ->
                            matchSetupUpdate MatchSetup.LeaveMatchSetup model

                        PressedPrimaryColor colorIndex ->
                            matchSetupUpdate (MatchSetup.SetPrimaryColor colorIndex) model

                        PressedSecondaryColor colorIndex ->
                            matchSetupUpdate (MatchSetup.SetSecondaryColor colorIndex) model

                        PressedDecal decal ->
                            matchSetupUpdate (MatchSetup.SetDecal decal) model

                        PressedPlayerMode mode ->
                            matchSetupUpdate (MatchSetup.SetPlayerMode mode) model

                        TypedMatchName matchName ->
                            ( { model
                                | page =
                                    { matchPage
                                        | matchData =
                                            case matchPage.matchData of
                                                MatchData _ ->
                                                    matchPage.matchData

                                                MatchSetupData matchSetupData ->
                                                    { matchSetupData | matchName = matchName } |> MatchSetupData
                                    }
                                        |> MatchPage
                              }
                            , Command.none
                            )

                        PressedSaveMatchName matchName ->
                            matchSetupUpdate (MatchSetup.SetMatchName matchName) model

                        PressedResetMatchName ->
                            ( { model
                                | page =
                                    { matchPage
                                        | matchData =
                                            case matchPage.matchData of
                                                MatchData _ ->
                                                    matchPage.matchData

                                                MatchSetupData matchSetupData ->
                                                    { matchSetupData
                                                        | matchName =
                                                            MatchSetup.name (getLocalState matchPage)
                                                                |> MatchName.toString
                                                    }
                                                        |> MatchSetupData
                                    }
                                        |> MatchPage
                              }
                            , Command.none
                            )

                        TypedTextMessage text ->
                            ( { model
                                | page =
                                    { matchPage
                                        | matchData =
                                            case matchPage.matchData of
                                                MatchData _ ->
                                                    matchPage.matchData

                                                MatchSetupData matchSetupData ->
                                                    { matchSetupData | message = text } |> MatchSetupData
                                    }
                                        |> MatchPage
                              }
                            , Command.none
                            )

                        SubmittedTextMessage message ->
                            matchSetupUpdate
                                (MatchSetup.SendTextMessage message)
                                { model
                                    | page =
                                        { matchPage
                                            | matchData =
                                                case matchPage.matchData of
                                                    MatchData _ ->
                                                        matchPage.matchData

                                                    MatchSetupData matchSetupData ->
                                                        { matchSetupData | message = "" } |> MatchSetupData
                                        }
                                            |> MatchPage
                                }
                                |> Tuple.mapSecond (\cmd -> Command.batch [ cmd, scrollToBottom ])

                _ ->
                    ( model, Command.none )

        ScrolledToBottom ->
            ( model, Command.none )


matchTimeLeft : Id FrameId -> MatchState -> Maybe Duration
matchTimeLeft currentFrameId matchState =
    let
        finishes : List Duration
        finishes =
            Dict.toList matchState.players
                |> List.filterMap
                    (\( _, player ) ->
                        case player.finishTime of
                            Finished finishTime ->
                                Quantity.multiplyBy
                                    (Id.toInt currentFrameId - Id.toInt finishTime |> toFloat)
                                    MatchSetup.frameDuration
                                    |> Just

                            DidNotFinish ->
                                Nothing
                    )

        earliestFinish =
            Quantity.maximum finishes |> Maybe.withDefault Quantity.zero

        latestFinish =
            Quantity.minimum finishes |> Maybe.withDefault Quantity.zero

        allFinished =
            Dict.size matchState.players == List.length finishes

        allFinishedTimeLeft =
            Duration.seconds 3 |> Quantity.minus latestFinish

        earliestFinishTimeLeft =
            Duration.seconds 10 |> Quantity.minus earliestFinish
    in
    if allFinished then
        Quantity.min earliestFinishTimeLeft allFinishedTimeLeft |> Just

    else if List.isEmpty finishes then
        Nothing

    else
        Just earliestFinishTimeLeft


textMessageContainerId : HtmlId
textMessageContainerId =
    Effect.Browser.Dom.id "textMessageContainer"


matchSetupUpdate : MatchSetupMsg -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
matchSetupUpdate msg model =
    case model.page of
        LobbyPage _ ->
            ( model, Command.none )

        MatchPage matchSetup ->
            let
                { eventId, newNetworkModel } =
                    NetworkModel.updateFromUser { userId = model.userId, msg = msg } matchSetup.networkModel
            in
            ( { model
                | page =
                    { matchSetup
                        | networkModel = newNetworkModel
                        , matchData =
                            updateMatchData
                                msg
                                newNetworkModel
                                matchSetup.networkModel
                                matchSetup.matchData
                    }
                        |> MatchPage
              }
            , MatchSetupRequest matchSetup.lobbyId eventId msg |> Effect.Lamdera.sendToBackend
            )


matchUpdate : MatchMsg -> MatchPage_ -> MatchPage_
matchUpdate msg model =
    case msg of
        PointerDown event ->
            { model
                | touchPosition =
                    case event.targetTouches ++ event.changedTouches ++ event.touches of
                        head :: _ ->
                            Point2d.fromTuple Pixels.pixels head.clientPos |> Just

                        _ ->
                            model.touchPosition
            }

        PointerUp _ ->
            { model | touchPosition = Nothing }

        PointerMoved event ->
            { model
                | touchPosition =
                    case event.targetTouches ++ event.changedTouches ++ event.touches of
                        head :: _ ->
                            Point2d.fromTuple Pixels.pixels head.clientPos |> Just

                        _ ->
                            model.touchPosition
            }


getInputDirection : WindowSize -> List Key -> Maybe (Point2d Pixels ScreenCoordinate) -> Maybe (Direction2d WorldCoordinate)
getInputDirection windowSize keys maybeTouchPosition =
    let
        input : Keyboard.Arrows.Direction
        input =
            Keyboard.Arrows.arrowsDirection keys
    in
    if input == Keyboard.Arrows.NoDirection then
        case maybeTouchPosition of
            Just touchPosition ->
                Direction2d.from
                    ({ x = toFloat (Pixels.inPixels windowSize.width) / 2
                     , y = toFloat (Pixels.inPixels windowSize.height) / 2
                     }
                        |> Point2d.fromPixels
                    )
                    touchPosition
                    |> Maybe.map
                        (Direction2d.mirrorAcross Axis2d.y
                            >> Direction2d.toAngle
                            >> Angle.inDegrees
                            >> (*) (1 / 10)
                            >> round
                            >> (*) 10
                            >> toFloat
                            >> Direction2d.degrees
                        )

            Nothing ->
                Nothing

    else
        directionToOffset input


timeToFrameId : FrontendLoaded -> Match -> Id FrameId
timeToFrameId model match =
    timeToServerTime model
        |> MatchSetup.unwrapServerTime
        |> Duration.from (MatchSetup.unwrapServerTime match.startTime)
        |> (\a -> Quantity.ratio a MatchSetup.frameDuration)
        |> round
        |> Id.fromInt


timeToServerTime : FrontendLoaded -> ServerTime
timeToServerTime model =
    pingOffset model |> Duration.addTo (actualTime model) |> ServerTime


pingOffset : FrontendLoaded -> Duration
pingOffset model =
    case model.pingData of
        Just pingData ->
            Quantity.plus pingData.lowEstimate pingData.highEstimate
                |> Quantity.divideBy 2
                |> Quantity.negate

        Nothing ->
            Quantity.zero


windowResizedUpdate : WindowSize -> { b | windowSize : WindowSize } -> ( { b | windowSize : WindowSize }, Command FrontendOnly toMsg FrontendMsg_ )
windowResizedUpdate windowSize model =
    ( { model | windowSize = windowSize }, Ports.devicePixelRatioRequest )


devicePixelRatioUpdate :
    Quantity Float (Rate WorldPixel Pixels)
    -> { b | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels) }
    -> ( { b | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels) }, Command FrontendOnly toMsg msg )
devicePixelRatioUpdate devicePixelRatio model =
    ( { model | devicePixelRatio = devicePixelRatio }
    , Command.none
    )


updateFromBackend : AudioData -> ToFrontend -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
updateFromBackend audioData msg model =
    case ( model, msg ) of
        ( Loading loading, ClientInit userId initData ) ->
            { loading | initData = Just ( userId, initData ) } |> tryLoadedInit

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))

        _ ->
            ( model, Command.none, Audio.cmdNone )


initMatchSetupData : MatchSetup -> { matchName : String, message : String }
initMatchSetupData lobby =
    { matchName = MatchSetup.name lobby |> MatchName.toString
    , message = ""
    }


scrollToBottom =
    Effect.Browser.Dom.setViewportOf textMessageContainerId 0 99999
        |> Task.attempt (\_ -> ScrolledToBottom)


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoadedFromBackend msg model =
    case msg of
        ClientInit _ _ ->
            -- Handled in updateFromBackend
            ( model, Command.none )

        CreateLobbyResponse lobbyId lobby ->
            ( case model.page of
                LobbyPage _ ->
                    { model
                        | page =
                            MatchPage
                                { lobbyId = lobbyId
                                , networkModel = NetworkModel.init lobby
                                , matchData = initMatchSetupData lobby |> MatchSetupData
                                }
                    }

                _ ->
                    model
            , Command.none
            )

        JoinLobbyResponse lobbyId result ->
            case model.page of
                LobbyPage _ ->
                    case result of
                        Ok lobby ->
                            let
                                networkModel =
                                    NetworkModel.init lobby
                            in
                            ( { model
                                | page =
                                    MatchPage
                                        { lobbyId = lobbyId
                                        , networkModel = networkModel
                                        , matchData =
                                            updateMatchData
                                                MatchSetup.JoinMatchSetup
                                                networkModel
                                                networkModel
                                                (initMatchSetupData lobby |> MatchSetupData)
                                        }
                              }
                            , scrollToBottom
                            )

                        Err LobbyNotFound ->
                            ( model, Command.none )

                _ ->
                    ( model, Command.none )

        CreateLobbyBroadcast lobbyId lobbyPreview ->
            ( case model.page of
                LobbyPage lobbyData ->
                    { model
                        | page =
                            LobbyPage
                                { lobbyData | lobbies = Dict.insert lobbyId lobbyPreview lobbyData.lobbies }
                    }

                _ ->
                    model
            , Command.none
            )

        PingResponse serverTime ->
            case model.pingStartTime of
                Just pingStartTime ->
                    let
                        keepPinging =
                            (pingCount < 5)
                                || (newHighEstimate
                                        |> Quantity.minus newLowEstimate
                                        |> Quantity.greaterThan (Duration.milliseconds 200)
                                   )

                        {- The time stored in the model is potentially out of date by an animation frame. We want to make sure our high estimate overestimates rather than underestimates the true time so we add an extra animation frame here. -}
                        localTimeHighEstimate =
                            Duration.addTo (actualTime model) MatchSetup.frameDuration

                        serverTime2 =
                            MatchSetup.unwrapServerTime serverTime

                        ( newLowEstimate, newHighEstimate, pingCount ) =
                            case model.pingData of
                                Just oldPingData ->
                                    ( Duration.from serverTime2 pingStartTime |> Quantity.max oldPingData.lowEstimate
                                    , Duration.from serverTime2 localTimeHighEstimate |> Quantity.min oldPingData.highEstimate
                                    , oldPingData.pingCount + 1
                                    )

                                Nothing ->
                                    ( Duration.from serverTime2 pingStartTime
                                    , Duration.from serverTime2 localTimeHighEstimate
                                    , 1
                                    )
                    in
                    ( { model
                        | pingData =
                            -- This seems to happen if the user tabs away. I'm not sure how to prevent it so here we just start over if we end up in this state.
                            if newHighEstimate |> Quantity.lessThan newLowEstimate then
                                Nothing

                            else
                                Just
                                    { roundTripTime = Duration.from pingStartTime (actualTime model)
                                    , lowEstimate = newLowEstimate
                                    , highEstimate = newHighEstimate
                                    , serverTime = serverTime2
                                    , sendTime = pingStartTime
                                    , receiveTime = actualTime model
                                    , pingCount = pingCount
                                    }
                        , pingStartTime =
                            if keepPinging then
                                Just (actualTime model)

                            else
                                Nothing
                      }
                    , if keepPinging then
                        Effect.Lamdera.sendToBackend PingRequest

                      else
                        Command.none
                    )

                Nothing ->
                    ( model, Command.none )

        MatchSetupBroadcast lobbyId userId matchSetupMsg ->
            case model.page of
                MatchPage matchSetup ->
                    let
                        updateHelper =
                            (if lobbyId == matchSetup.lobbyId then
                                let
                                    newNetworkModel : NetworkModel { userId : Id UserId, msg : MatchSetupMsg } MatchSetup
                                    newNetworkModel =
                                        NetworkModel.updateFromBackend
                                            (\a b -> MatchSetup.matchSetupUpdate a b |> Maybe.withDefault b)
                                            Nothing
                                            { userId = userId, msg = matchSetupMsg }
                                            matchSetup.networkModel
                                in
                                { matchSetup
                                    | networkModel = newNetworkModel
                                    , matchData =
                                        updateMatchData
                                            matchSetupMsg
                                            newNetworkModel
                                            matchSetup.networkModel
                                            matchSetup.matchData
                                }

                             else
                                matchSetup
                            )
                                |> MatchPage
                    in
                    case matchSetupMsg of
                        MatchSetup.SendTextMessage _ ->
                            ( { model | page = updateHelper }, scrollToBottom )

                        _ ->
                            ( { model | page = updateHelper }, Command.none )

                LobbyPage _ ->
                    ( model, Command.none )

        RemoveLobbyBroadcast lobbyId ->
            ( case model.page of
                LobbyPage lobbyData ->
                    { model | page = LobbyPage { lobbyData | lobbies = Dict.remove lobbyId lobbyData.lobbies } }

                MatchPage _ ->
                    model
            , Command.none
            )

        MatchSetupResponse lobbyId userId matchSetupMsg maybeLobbyData eventId ->
            case model.page of
                MatchPage matchSetup ->
                    let
                        updateHelper =
                            (if lobbyId == matchSetup.lobbyId then
                                let
                                    newNetworkModel : NetworkModel { userId : Id UserId, msg : MatchSetupMsg } MatchSetup
                                    newNetworkModel =
                                        NetworkModel.updateFromBackend
                                            (\a b -> MatchSetup.matchSetupUpdate a b |> Maybe.withDefault b)
                                            (Just eventId)
                                            { userId = userId, msg = matchSetupMsg }
                                            matchSetup.networkModel
                                in
                                { matchSetup
                                    | networkModel = newNetworkModel
                                    , matchData =
                                        updateMatchData
                                            matchSetupMsg
                                            newNetworkModel
                                            matchSetup.networkModel
                                            matchSetup.matchData
                                }

                             else
                                matchSetup
                            )
                                |> MatchPage
                    in
                    case ( maybeLobbyData, matchSetupMsg ) of
                        ( Just lobbyData, MatchSetup.LeaveMatchSetup ) ->
                            ( { model | page = LobbyPage lobbyData }, Command.none )

                        _ ->
                            ( { model | page = updateHelper }, Command.none )

                LobbyPage _ ->
                    ( model, Command.none )


updateMatchData :
    MatchSetupMsg
    -> NetworkModel { userId : Id UserId, msg : MatchSetupMsg } MatchSetup
    -> NetworkModel { userId : Id UserId, msg : MatchSetupMsg } MatchSetup
    -> MatchData
    -> MatchData
updateMatchData newMsg newNetworkModel oldNetworkModel oldMatchData =
    let
        newMatchState : MatchSetup
        newMatchState =
            NetworkModel.localState
                (\a b -> MatchSetup.matchSetupUpdate a b |> Maybe.withDefault b)
                newNetworkModel

        oldMatchState : MatchSetup
        oldMatchState =
            NetworkModel.localState
                (\a b -> MatchSetup.matchSetupUpdate a b |> Maybe.withDefault b)
                oldNetworkModel

        newUserIds : Nonempty ( Id UserId, PlayerData )
        newUserIds =
            MatchSetup.allUsers newMatchState

        initHelper : ServerTime -> MatchData
        initHelper serverTime =
            { timelineCache = initMatch serverTime newUserIds |> Timeline.init |> Ok
            , userIds =
                List.Nonempty.toList newUserIds
                    |> List.filterMap
                        (\( id, playerData ) ->
                            case playerData.mode of
                                PlayerMode ->
                                    Just ( id, playerMesh playerData )

                                SpectatorMode ->
                                    Nothing
                        )
                    |> Dict.fromList
            , wallMesh = wallMesh (Math.Vector3.vec3 1 0 0) wallSegments
            , touchPosition = Nothing
            , previousTouchPosition = Nothing
            }
                |> MatchData
    in
    case ( MatchSetup.getMatch newMatchState, MatchSetup.getMatch oldMatchState ) of
        ( Just newMatch, Just _ ) ->
            case oldMatchData of
                MatchData matchData ->
                    case ( matchData.timelineCache, newMsg ) of
                        ( Ok timelineCache, MatchSetup.MatchInputRequest serverTime _ ) ->
                            { matchData
                                | timelineCache =
                                    Timeline.addInput
                                        (MatchSetup.serverTimeToFrameId serverTime newMatch)
                                        timelineCache
                            }
                                |> MatchData

                        _ ->
                            MatchData matchData

                MatchSetupData _ ->
                    initHelper newMatch.startTime

        ( Just newMatch, Nothing ) ->
            initHelper newMatch.startTime

        ( Nothing, Just _ ) ->
            initMatchSetupData newMatchState |> MatchSetupData

        _ ->
            oldMatchData


actualTime : { a | time : Time.Posix, debugTimeOffset : Duration } -> Time.Posix
actualTime { time, debugTimeOffset } =
    Duration.addTo time debugTimeOffset


initMatch : ServerTime -> Nonempty ( Id UserId, PlayerData ) -> MatchState
initMatch startTime users =
    { players =
        Random.step
            (List.Nonempty.toList users
                |> List.filterMap
                    (\( userId, playerData ) ->
                        case playerData.mode of
                            PlayerMode ->
                                Just userId

                            SpectatorMode ->
                                Nothing
                    )
                |> Random.shuffle
            )
            (MatchSetup.unwrapServerTime startTime |> Time.posixToMillis |> Random.initialSeed)
            |> Tuple.first
            |> List.indexedMap
                (\index userId ->
                    let
                        playersPerRow =
                            6

                        spacing =
                            Length.inMeters playerRadius * 2.1

                        x =
                            modBy playersPerRow index

                        y =
                            index // playersPerRow
                    in
                    ( userId
                    , { position =
                            Point2d.translateBy
                                (Vector2d.fromMeters { x = toFloat x * spacing, y = toFloat y * spacing })
                                playerStart
                      , velocity = Vector2d.zero
                      , rotationalVelocity = Quantity.zero
                      , rotation = Quantity.zero
                      , input = Nothing
                      , finishTime = DidNotFinish
                      , lastCollision = Nothing
                      }
                    )
                )
            |> Dict.fromList
    }


lostConnection : FrontendLoaded -> Bool
lostConnection model =
    False


view : AudioData -> FrontendModel_ -> Browser.Document FrontendMsg_
view _ model =
    { title =
        case model of
            Loading _ ->
                "Air Hockey Racing"

            Loaded loadedModel ->
                if lostConnection loadedModel then
                    "(offline) Air Hockey Racing"

                else
                    "Air Hockey Racing"
    , body =
        [ case model of
            Loading loading ->
                Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.padding 16
                    ]
                    (if Dict.values loading.sounds |> List.any isErr then
                        Element.text "Loading failed"

                     else
                        Element.text "Loading"
                    )

            Loaded loadedModel ->
                loadedView loadedModel
        ]
    }


isErr : Result error value -> Bool
isErr a =
    case a of
        Err _ ->
            True

        Ok _ ->
            False


loadedView : FrontendLoaded -> Html FrontendMsg_
loadedView model =
    let
        displayType =
            Ui.displayType model.windowSize
    in
    Element.layout
        [ canvasView model |> Element.behindContent, Element.clip ]
        (case model.page of
            MatchPage matchSetup ->
                matchSetupView model matchSetup

            LobbyPage lobbyData ->
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 16
                    , Element.padding (Ui.ifMobile displayType 8 16)
                    ]
                    [ Element.el [ Element.Font.bold ] (Element.text "Air Hockey Racing")
                    , button PressedCreateLobby (Element.text "Create new match")
                    , Element.column
                        [ Element.width Element.fill, Element.height Element.fill, Element.spacing 8 ]
                        [ Element.text "Or join existing match"
                        , if Dict.isEmpty lobbyData.lobbies then
                            Element.paragraph
                                [ Element.Font.center, Element.centerY ]
                                [ Element.text "There are currently no existing matches" ]
                                |> Element.el
                                    [ Element.width (Element.maximum 800 Element.fill)
                                    , Element.height Element.fill
                                    , Element.Border.width 1
                                    ]

                          else
                            Dict.toList lobbyData.lobbies
                                |> List.indexedMap (\index lobby -> lobbyRowView (modBy 2 index == 0) lobby)
                                |> Element.column
                                    [ Element.width (Element.maximum 800 Element.fill)
                                    , Element.height Element.fill
                                    , Element.Border.width 1
                                    ]
                        ]
                    ]
        )


matchSetupView : FrontendLoaded -> MatchSetupPage_ -> Element FrontendMsg_
matchSetupView model matchSetup =
    let
        displayType =
            Ui.displayType model.windowSize

        lobby : MatchSetup
        lobby =
            getLocalState matchSetup

        matchName : String
        matchName =
            MatchName.toString (MatchSetup.name lobby)

        users : List ( Id UserId, PlayerData )
        users =
            MatchSetup.allUsers lobby |> List.Nonempty.toList
    in
    case ( MatchSetup.getMatch lobby, matchSetup.matchData, MatchSetup.allUsers_ lobby |> Dict.get model.userId ) of
        ( Just match, MatchData matchData, _ ) ->
            case matchData.timelineCache of
                Ok cache ->
                    let
                        ( _, matchState ) =
                            Timeline.getStateAt
                                gameUpdate
                                (timeToFrameId model match)
                                cache
                                match.timeline
                    in
                    Element.el
                        (Element.width Element.fill
                            :: Element.height Element.fill
                            :: Element.htmlAttribute (Html.Events.Extra.Touch.onStart PointerDown)
                            :: Element.htmlAttribute (Html.Events.Extra.Touch.onCancel PointerUp)
                            :: Element.htmlAttribute (Html.Events.Extra.Touch.onEnd PointerUp)
                            :: Element.inFront (countdown model match)
                            :: (case matchData.touchPosition of
                                    Just _ ->
                                        [ Element.htmlAttribute (Html.Events.Extra.Touch.onMove PointerMoved) ]

                                    Nothing ->
                                        []
                               )
                        )
                        (matchEndText match matchState model)
                        |> Element.map MatchMsg

                Err _ ->
                    Element.text "An error occurred during the match :("

        ( Nothing, MatchSetupData matchSetupData, Just currentPlayerData ) ->
            let
                places : Dict (Id UserId) Int
                places =
                    MatchSetup.previousMatchFinishTimes lobby
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList
                        |> List.filterMap
                            (\( userId, place ) ->
                                case place of
                                    Finished finishTime ->
                                        ( userId, Id.toInt finishTime ) |> Just

                                    DidNotFinish ->
                                        Nothing
                            )
                        |> List.sortBy Tuple.second
                        |> List.indexedMap (\index ( userId, _ ) -> ( userId, index + 1 ))
                        |> Dict.fromList
            in
            Element.column
                [ Element.spacing 8
                , Element.padding (Ui.ifMobile displayType 8 16)
                , Element.width (Element.maximum 800 Element.fill)
                , Element.height Element.fill
                ]
                [ case Dict.get model.userId places of
                    Just place ->
                        placementText place

                    Nothing ->
                        Element.none
                , if MatchSetup.isOwner model.userId lobby then
                    Element.row
                        [ Element.spacing 8, Element.width Element.fill ]
                        (Element.Input.text
                            [ Element.padding 4, Element.width Element.fill ]
                            { onChange = TypedMatchName
                            , text = matchSetupData.matchName
                            , placeholder = Element.Input.placeholder [] unnamedMatchText |> Just
                            , label = Element.Input.labelHidden "Match name"
                            }
                            :: (case
                                    ( MatchName.fromString matchSetupData.matchName
                                    , matchSetupData.matchName == matchName
                                    )
                                of
                                    ( Ok matchName_, False ) ->
                                        [ button (PressedSaveMatchName matchName_) (Element.text "Save")
                                        , button PressedResetMatchName (Element.text "Reset")
                                        ]

                                    _ ->
                                        []
                               )
                        )

                  else
                    Element.row [ Element.Font.bold ]
                        [ Element.text "Match: "
                        , if matchName == "" then
                            unnamedMatchText

                          else
                            Element.text matchName
                        ]
                , Element.wrappedRow
                    [ Element.spacing 8 ]
                    [ if MatchSetup.isOwner model.userId lobby then
                        button PressedStartMatchSetup (Element.text "Start match")

                      else
                        Element.none
                    , button PressedLeaveMatchSetup (Element.text "Leave")
                    , case currentPlayerData.mode of
                        PlayerMode ->
                            button (PressedPlayerMode SpectatorMode) (Element.text "Switch to spectator")

                        SpectatorMode ->
                            button (PressedPlayerMode PlayerMode) (Element.text "Switch to player")
                    ]
                , Element.column
                    [ Element.spacing 8 ]
                    [ Element.column
                        [ Element.spacing 8
                        , Element.alpha
                            (case currentPlayerData.mode of
                                PlayerMode ->
                                    1

                                SpectatorMode ->
                                    0.5
                            )
                        ]
                        [ Element.column
                            [ Element.spacing 4, Element.Font.size 16, Element.Font.bold ]
                            [ Element.text "Primary color"
                            , colorSelector PressedPrimaryColor currentPlayerData.primaryColor
                            ]
                        , Element.column
                            [ Element.spacing 4, Element.Font.size 16, Element.Font.bold ]
                            [ Element.text "Secondary color"
                            , colorSelector PressedSecondaryColor currentPlayerData.secondaryColor
                            ]
                        , Element.column
                            [ Element.spacing 4, Element.width Element.fill ]
                            [ Element.el [ Element.Font.size 16, Element.Font.bold ] (Element.text "Decal")
                            , Nothing
                                :: List.map Just (List.Nonempty.toList Decal.allDecals)
                                |> List.map
                                    (\maybeDecal ->
                                        Ui.button
                                            [ Element.paddingXY 4 4
                                            , Element.Background.color
                                                (if maybeDecal == currentPlayerData.decal then
                                                    Element.rgb 0.6 0.7 1

                                                 else
                                                    Element.rgb 0.8 0.8 0.8
                                                )
                                            ]
                                            { onPress = PressedDecal maybeDecal
                                            , label =
                                                (case maybeDecal of
                                                    Just decal ->
                                                        Decal.toString decal

                                                    Nothing ->
                                                        "None"
                                                )
                                                    |> Element.text
                                            }
                                    )
                                |> Element.row [ Element.spacing 8, Element.width Element.fill ]
                            ]
                        ]
                    ]
                , Element.row
                    [ Element.spacing 16, Element.width Element.fill, Element.height Element.fill ]
                    [ Element.column
                        [ Element.spacing 8, Element.alignTop, Element.Font.size 16 ]
                        [ Element.text "Participants:"
                        , Element.column
                            []
                            (List.map
                                (\( userId, playerData ) ->
                                    "User "
                                        ++ String.fromInt (Id.toInt userId)
                                        ++ (case playerData.mode of
                                                PlayerMode ->
                                                    ""

                                                SpectatorMode ->
                                                    " (spectator)"
                                           )
                                        ++ (case Dict.get userId places of
                                                Just place ->
                                                    " (" ++ placeToText place ++ ")"

                                                Nothing ->
                                                    ""
                                           )
                                        |> Element.text
                                )
                                users
                            )
                        ]
                    , textChat matchSetupData lobby
                    ]
                ]
                |> Element.map MatchSetupMsg

        _ ->
            Element.text "Loading..."


textChat : { matchName : String, message : String } -> MatchSetup -> Element MatchSetupMsg_
textChat matchSetupData lobby =
    Element.column
        [ Element.scrollbarY
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 4
        ]
        [ MatchSetup.messagesOldestToNewest lobby
            |> List.map
                (\{ userId, message } ->
                    let
                        userName : String
                        userName =
                            Id.toInt userId |> String.fromInt |> (++) "User "
                    in
                    Element.row
                        [ Element.Font.size 16 ]
                        [ (if MatchSetup.isOwner userId lobby then
                            userName ++ " (host)" ++ " "

                           else
                            userName ++ " "
                          )
                            |> Element.text
                            |> Element.el [ Element.Font.bold, Element.alignTop ]
                        , TextMessage.toString message |> Element.text |> List.singleton |> Element.paragraph []
                        ]
                )
            |> Element.column
                [ Element.spacing 4
                , Element.scrollbarY
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingXY 0 8
                , Element.htmlAttribute (Effect.Browser.Dom.idToAttribute textMessageContainerId)
                ]
        , Element.Input.text
            (Element.Font.size 16
                :: Element.padding 8
                :: (case TextMessage.fromString matchSetupData.message of
                        Ok message ->
                            [ Html.Events.on "keydown"
                                (Json.Decode.field "keyCode" Json.Decode.int
                                    |> Json.Decode.andThen
                                        (\key ->
                                            if key == 13 then
                                                SubmittedTextMessage message |> Json.Decode.succeed

                                            else
                                                Json.Decode.fail ""
                                        )
                                )
                                |> Element.htmlAttribute
                            ]

                        Err _ ->
                            []
                   )
            )
            { onChange = TypedTextMessage
            , text = matchSetupData.message
            , placeholder = Element.Input.placeholder [] (Element.text "Press enter to send") |> Just
            , label = Element.Input.labelHidden "Write message"
            }
        ]


placementText : Int -> Element msg
placementText place =
    placeToText place
        |> Element.text
        |> Element.el
            [ Element.Font.size 64
            , Element.Font.shadow
                { offset = ( 0, 0 )
                , blur = 2
                , color = Element.rgba 0 0 0 1
                }
            , Element.Font.color
                (case place of
                    1 ->
                        Element.rgb 1 0.9 0

                    2 ->
                        Element.rgb 0.79 0.79 0.8

                    3 ->
                        Element.rgb 0.7 0.5 0.2

                    _ ->
                        Element.rgb 0 0 0
                )
            , Element.Font.bold
            ]


matchEndText : Match -> MatchState -> FrontendLoaded -> Element msg
matchEndText match matchState model =
    let
        maybeFinish : Maybe { place : Int, userId : Id UserId, finishTime : Id FrameId }
        maybeFinish =
            Dict.toList matchState.players
                |> List.filterMap
                    (\( userId, player ) ->
                        case player.finishTime of
                            Finished finishTime ->
                                Just ( userId, finishTime )

                            DidNotFinish ->
                                Nothing
                    )
                |> List.sortBy (Tuple.second >> Id.toInt)
                |> List.indexedMap
                    (\index ( userId, finishTime ) ->
                        { place = index + 1, userId = userId, finishTime = finishTime }
                    )
                |> List.find (.userId >> (==) model.userId)

        maybeTimeLeft : Maybe Duration
        maybeTimeLeft =
            matchTimeLeft (timeToFrameId model match) matchState
    in
    case maybeFinish of
        Just finish ->
            Element.column
                [ Element.width Element.fill
                , Element.spacing 16
                , noPointerEvents
                , Element.moveDown 24
                ]
                [ Element.el [ Element.centerX ] (placementText finish.place)
                , Quantity.multiplyBy (Id.toInt finish.finishTime |> toFloat) MatchSetup.frameDuration
                    |> timestamp_
                    |> Element.text
                    |> Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 24 ]
                , case maybeTimeLeft of
                    Just timeLeft ->
                        "Match will end in "
                            ++ String.fromInt (round (Duration.inSeconds timeLeft))
                            |> Element.text
                            |> Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 24 ]

                    Nothing ->
                        Element.none
                ]

        Nothing ->
            case maybeTimeLeft of
                Just timeLeft ->
                    "Someone finished! The match will end in "
                        ++ String.fromInt (round (Duration.inSeconds timeLeft))
                        |> Element.text
                        |> Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 24 ]

                Nothing ->
                    Element.none


placeToText : Int -> String
placeToText place =
    case place of
        1 ->
            "Winner!"

        2 ->
            "2nd place!"

        3 ->
            "3rd place!"

        21 ->
            "21st place"

        22 ->
            "22nd place"

        23 ->
            "23rd place"

        _ ->
            String.fromInt place ++ "th place"


countdownDelay =
    Duration.seconds 3


countdown : FrontendLoaded -> Match -> Element msg
countdown model match =
    let
        elapsed : Duration
        elapsed =
            Quantity.multiplyBy (timeToFrameId model match |> Id.toInt |> toFloat) MatchSetup.frameDuration

        countdownValue =
            Duration.inSeconds elapsed |> floor |> (-) 3
    in
    if elapsed |> Quantity.lessThan countdownDelay then
        String.fromInt countdownValue
            |> Element.text
            |> Element.el
                [ Element.Font.size 100
                , Element.Font.bold
                , Element.centerX
                , Element.centerY
                , Element.Font.color (Element.rgb 1 1 1)
                , Element.Font.glow (Element.rgb 0 0 0) 2
                , Element.moveUp 100
                , noPointerEvents
                ]

    else if elapsed |> Quantity.lessThan (Quantity.plus Duration.second countdownDelay) then
        "GO"
            |> Element.text
            |> Element.el
                [ Element.Font.size 100
                , Element.Font.bold
                , Element.centerX
                , Element.centerY
                , Element.Font.color (Element.rgb 1 1 1)
                , Element.Font.glow (Element.rgb 0 0 0) 2
                , Element.moveUp 100
                , noPointerEvents
                ]

    else
        Element.none


timestamp_ : Duration -> String
timestamp_ difference =
    let
        minutes =
            Duration.inMinutes difference |> floor

        minutesRemainder =
            difference |> Quantity.minus (Duration.minutes (toFloat minutes))

        seconds =
            Duration.inSeconds minutesRemainder |> floor

        secondsRemainder =
            minutesRemainder |> Quantity.minus (Duration.seconds (toFloat seconds))

        milliseconds =
            Duration.inMilliseconds secondsRemainder |> floor
    in
    String.fromInt minutes
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt seconds)
        ++ "."
        ++ String.padLeft 3 '0' (String.fromInt milliseconds)


noPointerEvents =
    Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")


colorSelector : (ColorIndex -> msg) -> ColorIndex -> Element msg
colorSelector onSelect currentColor =
    List.Nonempty.toList ColorIndex.allColors
        |> List.map
            (\colorIndex ->
                Ui.button
                    [ Element.width (Element.px 36)
                    , Element.height (Element.px 36)
                    , Element.Border.width
                        (if currentColor == colorIndex then
                            3

                         else
                            0
                        )
                    , Element.Border.color (Element.rgb 1 1 1)
                    , ColorIndex.toElColor colorIndex |> Element.Background.color
                    ]
                    { onPress = onSelect colorIndex
                    , label = Element.none
                    }
            )
        |> Element.wrappedRow []


timestamp : Time.Posix -> String
timestamp time =
    String.fromInt (Time.toHour Time.utc time)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute Time.utc time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toSecond Time.utc time))
        ++ "."
        ++ String.padLeft 4 '0' (String.fromInt (Time.toMillis Time.utc time))



--inputsView : MatchPage_ -> Element msg
--inputsView matchPage =
--    List.map
--        (\( frameId, event ) ->
--            String.fromInt (Id.toInt frameId)
--                ++ ", User "
--                ++ String.fromInt (Id.toInt event.userId)
--                ++ ", "
--                ++ (case event.input of
--                        Just input ->
--                            Direction2d.toAngle input |> Angle.inDegrees |> round |> String.fromInt |> (\a -> a ++ "°")
--
--                        Nothing ->
--                            "No input"
--                   )
--                |> Element.text
--        )
--        (Set.toList matchPage.timeline)
--        |> Element.column [ Element.alignTop ]


button : msg -> Element msg -> Element msg
button onPress label =
    Ui.button
        [ Element.Background.color <| Element.rgb 0.9 0.9 0.85
        , Element.padding 4
        ]
        { onPress = onPress
        , label = label
        }


lobbyRowView : Bool -> ( Id LobbyId, LobbyPreview ) -> Element FrontendMsg_
lobbyRowView evenRow ( lobbyId, lobby ) =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color
            (if evenRow then
                Element.rgb 1 1 1

             else
                Element.rgb 0.95 0.95 0.95
            )
        , Element.padding 4
        ]
        [ if lobby.name == MatchName.empty then
            unnamedMatchText

          else
            Element.text (MatchName.toString lobby.name)
        , Element.row
            [ Element.alignRight, Element.spacing 8 ]
            [ Element.text <| "Players: " ++ String.fromInt lobby.userCount
            , button (PressedJoinLobby lobbyId) (Element.text "Join")
            ]
        ]


unnamedMatchText : Element msg
unnamedMatchText =
    Element.el
        [ Element.Font.italic, Element.Font.color (Element.rgb 0.6 0.6 0.6) ]
        (Element.text "Unnamed match")


findPixelPerfectSize : FrontendLoaded -> { canvasSize : ( Quantity Int Pixels, Quantity Int Pixels ), actualCanvasSize : ( Int, Int ) }
findPixelPerfectSize frontendModel =
    let
        (Quantity pixelRatio) =
            frontendModel.devicePixelRatio

        findValue : Quantity Int Pixels -> ( Int, Int )
        findValue value =
            List.range 0 9
                |> List.map ((+) (Pixels.inPixels value))
                |> List.find
                    (\v ->
                        let
                            a =
                                toFloat v * pixelRatio
                        in
                        a == toFloat (round a) && modBy 2 (round a) == 0
                    )
                |> Maybe.map (\v -> ( v, toFloat v * pixelRatio |> round ))
                |> Maybe.withDefault ( Pixels.inPixels value, toFloat (Pixels.inPixels value) * pixelRatio |> round )

        ( w, actualW ) =
            findValue frontendModel.windowSize.width

        ( h, actualH ) =
            findValue frontendModel.windowSize.height
    in
    { canvasSize = ( Pixels.pixels w, Pixels.pixels h ), actualCanvasSize = ( actualW, actualH ) }


canvasView : FrontendLoaded -> Element msg
canvasView model =
    let
        ( windowWidth, windowHeight ) =
            actualCanvasSize

        ( cssWindowWidth, cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize model
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False ]
        [ Html.Attributes.width windowWidth
        , Html.Attributes.height windowHeight
        , Html.Attributes.style "width" (String.fromInt (Pixels.inPixels cssWindowWidth) ++ "px")
        , Html.Attributes.style "height" (String.fromInt (Pixels.inPixels cssWindowHeight) ++ "px")
        ]
        (case model.page of
            MatchPage matchSetup ->
                case ( MatchSetup.getMatch (getLocalState matchSetup), matchSetup.matchData ) of
                    ( Just match, MatchData matchData ) ->
                        case matchData.timelineCache of
                            Ok cache ->
                                let
                                    ( _, state ) =
                                        Timeline.getStateAt
                                            gameUpdate
                                            (timeToFrameId model match)
                                            cache
                                            match.timeline

                                    { x, y } =
                                        case Dict.get model.userId state.players of
                                            Just player ->
                                                Point2d.toMeters player.position

                                            Nothing ->
                                                let
                                                    vectorAndDistance =
                                                        Dict.values state.players
                                                            |> List.map
                                                                (\player ->
                                                                    { vector = Vector2d.from Point2d.origin player.position
                                                                    , distance =
                                                                        BoundingBox2d.centerPoint finishLine
                                                                            |> Point2d.distanceFrom player.position
                                                                    }
                                                                )

                                                    vectorAndWeight =
                                                        List.map
                                                            (\{ vector, distance } ->
                                                                { vector = vector
                                                                , weight = 1000 / max 100 (Quantity.unwrap distance)
                                                                }
                                                            )
                                                            vectorAndDistance

                                                    totalWeight =
                                                        List.map .weight vectorAndWeight |> List.sum
                                                in
                                                List.map
                                                    (\{ vector, weight } ->
                                                        Vector2d.scaleBy weight vector
                                                    )
                                                    vectorAndWeight
                                                    |> Vector2d.sum
                                                    |> Vector2d.scaleBy (1 / totalWeight)
                                                    |> (\v -> Point2d.translateBy v Point2d.origin)
                                                    |> Point2d.toMeters

                                    zoom =
                                        toFloat (max windowWidth windowHeight) / 2000

                                    viewMatrix =
                                        Mat4.makeScale3
                                            (zoom * 2 / toFloat windowWidth)
                                            (zoom * 2 / toFloat windowHeight)
                                            1
                                            |> Mat4.translate3 -x -y 0

                                    playerRadius_ : Float
                                    playerRadius_ =
                                        Length.inMeters playerRadius

                                    input : Maybe (Direction2d WorldCoordinate)
                                    input =
                                        getInputDirection model.windowSize model.currentKeys matchData.touchPosition
                                in
                                WebGL.entityWith
                                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                    backgroundVertexShader
                                    backgroundFragmentShader
                                    squareMesh
                                    { view = Math.Vector2.vec2 x y
                                    , viewZoom = zoom
                                    , windowSize = Math.Vector2.vec2 (toFloat windowWidth) (toFloat windowHeight)
                                    }
                                    :: WebGL.entityWith
                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                        vertexShader
                                        fragmentShader
                                        finishLineMesh
                                        { view = viewMatrix
                                        , model =
                                            Mat4.makeTranslate3
                                                (BoundingBox2d.minX finishLine |> Length.inMeters)
                                                (BoundingBox2d.minY finishLine |> Length.inMeters)
                                                0
                                        }
                                    :: WebGL.entityWith
                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                        vertexShader
                                        fragmentShader
                                        matchData.wallMesh
                                        { view = viewMatrix
                                        , model = Mat4.identity
                                        }
                                    :: List.filterMap
                                        (\( userId, player ) ->
                                            case Dict.get userId matchData.userIds of
                                                Just mesh ->
                                                    WebGL.entityWith
                                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                                        vertexShader
                                                        fragmentShader
                                                        mesh
                                                        { view = viewMatrix
                                                        , model =
                                                            pointToMatrix player.position
                                                                |> Mat4.scale3 playerRadius_ playerRadius_ playerRadius_
                                                                |> Mat4.rotate (Angle.inRadians player.rotation) (Math.Vector3.vec3 0 0 1)
                                                        }
                                                        |> Just

                                                Nothing ->
                                                    Nothing
                                        )
                                        (Dict.toList state.players)
                                    ++ (case ( Dict.get model.userId state.players, input ) of
                                            ( Just player, Just direction ) ->
                                                case player.finishTime of
                                                    Finished _ ->
                                                        []

                                                    DidNotFinish ->
                                                        [ WebGL.entityWith
                                                            [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                                            vertexShader
                                                            fragmentShader
                                                            arrow
                                                            { view = viewMatrix
                                                            , model =
                                                                pointToMatrix player.position
                                                                    |> Mat4.scale3 30 30 30
                                                                    |> Mat4.rotate
                                                                        (Direction2d.toAngle direction |> Angle.inRadians |> (+) (pi / 2))
                                                                        (Math.Vector3.vec3 0 0 1)
                                                            }
                                                        ]

                                            _ ->
                                                []
                                       )

                            Err _ ->
                                []

                    _ ->
                        []

            LobbyPage _ ->
                []
        )
        |> Element.html


wall : Polygon2d Meters WorldCoordinate
wall =
    Polygon2d.withHoles
        [ [ Point2d.meters 4600 2500
          , Point2d.meters 4700 2500
          , Point2d.meters 4700 2600
          , Point2d.meters 4600 2600
          ]
            |> List.map (Point2d.rotateAround (Point2d.meters 4650 2550) (Angle.degrees 45))
        ]
        ([ Point2d.meters 1187 461
         , Point2d.meters 1187 328
         , Point2d.meters 1078 328
         , Point2d.meters 1078 453
         , Point2d.meters 875 453
         , Point2d.meters 771 424
         , Point2d.meters 563 424
         , Point2d.meters 631 300
         , Point2d.meters 631 141
         , Point2d.meters 438 141
         , Point2d.meters 438 270
         , Point2d.meters 455 300
         , Point2d.meters 438 300
         , Point2d.meters 438 487
         , Point2d.meters 509 560
         , Point2d.meters 1090 560
         ]
            |> List.map (Point2d.scaleAbout Point2d.origin 5)
        )


playerStart : Point2d Meters WorldCoordinate
playerStart =
    Point2d.fromMeters { x = 2300, y = 800 }


wallSegments : List (LineSegment2d Meters WorldCoordinate)
wallSegments =
    verticesToLineSegments (Polygon2d.outerLoop wall)
        ++ List.concatMap verticesToLineSegments (Polygon2d.innerLoops wall)


gridSize : Length
gridSize =
    Quantity.multiplyBy 2 playerRadius


pointToGrid : Point2d Meters coordinates -> { x : Int, y : Int }
pointToGrid point =
    let
        { x, y } =
            Point2d.scaleAbout Point2d.origin (Quantity.ratio Length.meter gridSize) point |> Point2d.toMeters
    in
    { x = floor x, y = floor y }


wallLookUp : RegularDict.Dict ( Int, Int ) (Set (LineSegment2d Meters WorldCoordinate))
wallLookUp =
    List.foldl
        (\segment dict ->
            let
                ( start, end ) =
                    LineSegment2d.endpoints segment

                addPoint x y =
                    RegularDict.update ( x, y ) (Maybe.withDefault Set.empty >> Set.insert segment >> Just)
            in
            RasterShapes.line (pointToGrid start) (pointToGrid end)
                |> List.foldl
                    (\{ x, y } dict2 ->
                        dict2
                            |> addPoint (x - 1) (y - 1)
                            |> addPoint x (y - 1)
                            |> addPoint (x + 1) (y - 1)
                            |> addPoint (x - 1) y
                            |> addPoint x y
                            |> addPoint (x + 1) y
                            |> addPoint (x - 1) (y + 1)
                            |> addPoint x (y + 1)
                            |> addPoint (x + 1) (y + 1)
                    )
                    dict
        )
        RegularDict.empty
        wallSegments


getCollisionCandidates : Point2d Meters coordinates -> Set (LineSegment2d Meters WorldCoordinate)
getCollisionCandidates point =
    RegularDict.get (pointToGrid point |> (\{ x, y } -> ( x, y ))) wallLookUp |> Maybe.withDefault Set.empty


verticesToLineSegments : List (Point2d units coordinates) -> List (LineSegment2d units coordinates)
verticesToLineSegments points =
    case ( List.head points, List.reverse points |> List.head ) of
        ( Just head, Just last ) ->
            points
                |> List.groupsOfWithStep 2 1
                |> (::) [ last, head ]
                |> List.filterMap
                    (\list ->
                        case list of
                            [ first, second ] ->
                                LineSegment2d.from first second |> Just

                            _ ->
                                Nothing
                    )

        _ ->
            []


wallMesh : Vec3 -> List (LineSegment2d Meters WorldCoordinate) -> Mesh Vertex
wallMesh color lines =
    List.concatMap (lineMesh color) lines |> WebGL.triangles


lineMesh : Vec3 -> LineSegment2d Meters WorldCoordinate -> List ( Vertex, Vertex, Vertex )
lineMesh color line =
    let
        ( p0, p1 ) =
            LineSegment2d.endpoints line

        perpendicular : Vector2d units coordinates
        perpendicular =
            Vector2d.from p0 p1
                |> Vector2d.perpendicularTo
                |> Vector2d.normalize
                |> Vector2d.scaleBy 10
                |> Vector2d.unwrap
                |> Vector2d.unsafe
    in
    [ ( Point2d.translateBy perpendicular p0
      , Point2d.translateBy (Vector2d.reverse perpendicular) p0
      , Point2d.translateBy (Vector2d.reverse perpendicular) p1
      )
    , ( Point2d.translateBy (Vector2d.reverse perpendicular) p1
      , Point2d.translateBy perpendicular p1
      , Point2d.translateBy perpendicular p0
      )
    ]
        |> List.map
            (\( a, b, c ) ->
                ( { position = pointToVec a, color = color }
                , { position = pointToVec b, color = color }
                , { position = pointToVec c, color = color }
                )
            )


pointToVec : Point2d units coordinate -> Vec2
pointToVec point2d =
    let
        { x, y } =
            Point2d.unwrap point2d
    in
    Math.Vector2.vec2 x y


gameUpdate : Id FrameId -> List TimelineEvent -> MatchState -> MatchState
gameUpdate frameId inputs model =
    let
        newModel : { players : Dict (Id UserId) Player }
        newModel =
            List.foldl
                (\{ userId, input } model2 ->
                    { players = Dict.update userId (Maybe.map (\a -> { a | input = input })) model2.players }
                )
                model
                inputs

        updatedVelocities_ : Dict (Id UserId) Player
        updatedVelocities_ =
            updateVelocities frameId newModel.players
    in
    { players =
        Dict.map
            (\id player ->
                Dict.remove id updatedVelocities_
                    |> Dict.values
                    |> List.foldl (\a b -> handleCollision frameId b a |> Tuple.first) player
            )
            updatedVelocities_
    }


updateVelocities : Id FrameId -> Dict (Id UserId) Player -> Dict (Id UserId) Player
updateVelocities frameId players =
    let
        checkFinish : Player -> Place
        checkFinish player =
            case player.finishTime of
                Finished _ ->
                    player.finishTime

                DidNotFinish ->
                    if BoundingBox2d.contains player.position finishLine then
                        Finished frameId

                    else
                        player.finishTime

        elapsed =
            Quantity.multiplyBy (Id.toInt frameId |> toFloat) MatchSetup.frameDuration
    in
    Dict.map
        (\_ a ->
            let
                nearestCollision :
                    Maybe
                        { collisionVelocity : Vector2d Meters WorldCoordinate
                        , collisionPosition : Point2d Meters WorldCoordinate
                        }
                nearestCollision =
                    getCollisionCandidates a.position
                        |> Set.toList
                        |> List.filterMap
                            (\line ->
                                let
                                    lineCollision =
                                        Collision.circleLine playerRadius a.position a.velocity line
                                in
                                case ( lineCollision, LineSegment2d.direction line ) of
                                    ( Just collisionPosition, Just lineDirection ) ->
                                        { collisionPosition = collisionPosition
                                        , collisionVelocity =
                                            Vector2d.mirrorAcross
                                                (Axis2d.withDirection lineDirection (LineSegment2d.startPoint line))
                                                newVelocity
                                        }
                                            |> Just

                                    _ ->
                                        let
                                            point : Point2d Meters WorldCoordinate
                                            point =
                                                LineSegment2d.startPoint line
                                        in
                                        case Collision.circlePoint playerRadius a.position a.velocity point of
                                            Just collisionPoint ->
                                                case Direction2d.from collisionPoint point of
                                                    Just direction ->
                                                        { collisionPosition = collisionPoint
                                                        , collisionVelocity =
                                                            Vector2d.mirrorAcross
                                                                (Axis2d.withDirection
                                                                    (Direction2d.perpendicularTo direction)
                                                                    collisionPoint
                                                                )
                                                                newVelocity
                                                        }
                                                            |> Just

                                                    Nothing ->
                                                        Nothing

                                            Nothing ->
                                                Nothing
                            )
                        |> Quantity.sortBy (.collisionPosition >> Point2d.distanceFrom a.position)
                        |> List.head

                newVelocity : Vector2d Meters WorldCoordinate
                newVelocity =
                    (case ( a.finishTime, a.input, elapsed |> Quantity.lessThan countdownDelay ) of
                        ( DidNotFinish, Just input, False ) ->
                            Direction2d.toVector input
                                |> Vector2d.scaleBy 0.2
                                |> Vector2d.unwrap
                                |> Vector2d.unsafe

                        _ ->
                            Vector2d.zero
                    )
                        |> Vector2d.plus a.velocity
                        |> Vector2d.scaleBy 0.99
            in
            case nearestCollision of
                Just { collisionVelocity, collisionPosition } ->
                    let
                        angleChange : Angle
                        angleChange =
                            Maybe.map2 Direction2d.angleFrom
                                (Vector2d.direction collisionVelocity)
                                (Vector2d.direction a.velocity)
                                |> Maybe.withDefault (Angle.turns 0.5)
                    in
                    { position = collisionPosition
                    , velocity = collisionVelocity
                    , rotation = Quantity.plus a.rotation a.rotationalVelocity
                    , rotationalVelocity =
                        Angle.turns 0.5
                            |> Quantity.minus (Quantity.abs angleChange)
                            |> Quantity.multiplyBy
                                (if Quantity.lessThanZero angleChange then
                                    -0.01 * Length.inMeters (Vector2d.length collisionVelocity)

                                 else
                                    0.01 * Length.inMeters (Vector2d.length collisionVelocity)
                                )
                    , finishTime = checkFinish a
                    , input = a.input
                    , lastCollision = Just frameId
                    }

                Nothing ->
                    { position = Point2d.translateBy a.velocity a.position
                    , velocity = newVelocity
                    , rotation = Quantity.plus a.rotation a.rotationalVelocity
                    , rotationalVelocity = Quantity.multiplyBy 0.995 a.rotationalVelocity
                    , finishTime = checkFinish a
                    , input = a.input
                    , lastCollision = a.lastCollision
                    }
        )
        players


playerRadius : Length
playerRadius =
    Length.meters 50


arrow : WebGL.Mesh Vertex
arrow =
    [ { v0 = ( -1, 1 ), v1 = ( 0, 0 ), v2 = ( 1, 1 ) }
    , { v0 = ( -0.5, 1 ), v1 = ( 0.5, 1 ), v2 = ( 0.5, 2 ) }
    , { v0 = ( -0.5, 2 ), v1 = ( -0.5, 1 ), v2 = ( 0.5, 2 ) }
    ]
        |> List.map
            (\{ v0, v1, v2 } ->
                ( { position = Math.Vector2.vec2 (Tuple.first v0) (Tuple.second v0), color = Math.Vector3.vec3 1 0.8 0.1 }
                , { position = Math.Vector2.vec2 (Tuple.first v1) (Tuple.second v1), color = Math.Vector3.vec3 1 0.8 0.1 }
                , { position = Math.Vector2.vec2 (Tuple.first v2) (Tuple.second v2), color = Math.Vector3.vec3 1 0.8 0.1 }
                )
            )
        |> WebGL.triangles


handleCollision : Id FrameId -> Player -> Player -> ( Player, Player )
handleCollision frameId playerA playerB =
    case Collision.circleCircle playerRadius playerA.position playerA.velocity playerB.position playerB.velocity of
        Just ( v1, v2 ) ->
            ( { playerA | velocity = v1, lastCollision = Just frameId }, { playerB | velocity = v2 } )

        Nothing ->
            ( playerA, playerB )


pointToMatrix : Point2d units coordinates -> Mat4
pointToMatrix point =
    let
        { x, y } =
            Point2d.unwrap point
    in
    Mat4.makeTranslate3 x y 0


directionToOffset : Keyboard.Arrows.Direction -> Maybe (Direction2d WorldCoordinate)
directionToOffset direction =
    case direction of
        Keyboard.Arrows.North ->
            Vector2d.meters 0 1 |> Vector2d.direction

        Keyboard.Arrows.NorthEast ->
            Vector2d.meters 1 1 |> Vector2d.direction

        Keyboard.Arrows.East ->
            Vector2d.meters 1 0 |> Vector2d.direction

        Keyboard.Arrows.SouthEast ->
            Vector2d.meters 1 -1 |> Vector2d.direction

        Keyboard.Arrows.South ->
            Vector2d.meters 0 -1 |> Vector2d.direction

        Keyboard.Arrows.SouthWest ->
            Vector2d.meters -1 -1 |> Vector2d.direction

        Keyboard.Arrows.West ->
            Vector2d.meters -1 0 |> Vector2d.direction

        Keyboard.Arrows.NorthWest ->
            Vector2d.meters -1 1 |> Vector2d.direction

        Keyboard.Arrows.NoDirection ->
            Nothing


squareMesh : WebGL.Mesh { position : Vec2 }
squareMesh =
    WebGL.triangleFan
        [ { position = Math.Vector2.vec2 -1 -1 }
        , { position = Math.Vector2.vec2 1 -1 }
        , { position = Math.Vector2.vec2 1 1 }
        , { position = Math.Vector2.vec2 -1 1 }
        ]


playerMesh : PlayerData -> WebGL.Mesh Vertex
playerMesh playerData =
    let
        primaryColor : Vec3
        primaryColor =
            ColorIndex.toVec3 playerData.primaryColor
    in
    circleMesh 1 (Math.Vector3.vec3 0 0 0)
        ++ circleMesh 0.95 primaryColor
        ++ (case playerData.decal of
                Just decal ->
                    Decal.triangles playerData.secondaryColor decal

                Nothing ->
                    []
           )
        |> WebGL.triangles


circleMesh : Float -> Vec3 -> List ( Vertex, Vertex, Vertex )
circleMesh size color =
    let
        detail =
            64
    in
    List.range 0 (detail - 3)
        |> List.map
            (\index ->
                let
                    t0 =
                        0

                    t1 =
                        pi * 2 * toFloat (index + 1) / detail

                    t2 =
                        pi * 2 * toFloat (index + 2) / detail
                in
                ( { position = Math.Vector2.vec2 (cos t0 * size) (sin t0 * size), color = color }
                , { position = Math.Vector2.vec2 (cos t1 * size) (sin t1 * size), color = color }
                , { position = Math.Vector2.vec2 (cos t2 * size) (sin t2 * size), color = color }
                )
            )


finishLine : BoundingBox2d Meters WorldCoordinate
finishLine =
    BoundingBox2d.from (Point2d.fromMeters { x = 5300, y = 1600 }) (Point2d.fromMeters { x = 6000, y = 2000 })


finishLineMesh : Mesh Vertex
finishLineMesh =
    let
        squareSize =
            50

        helper =
            Quantity.divideBy squareSize >> Quantity.unwrap >> ceiling

        ( squaresWide, squaresTall ) =
            BoundingBox2d.dimensions finishLine |> Tuple.mapBoth helper helper
    in
    List.range 0 (squaresWide - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (squaresTall - 1)
                    |> List.concatMap
                        (\y ->
                            let
                                color =
                                    if x + y |> modBy 2 |> (==) 0 then
                                        Math.Vector3.vec3 0.2 0.2 0.2

                                    else
                                        Math.Vector3.vec3 1 1 1

                                offsetX =
                                    x * squareSize |> toFloat

                                offsetY =
                                    y * squareSize |> toFloat

                                v0 =
                                    Math.Vector2.vec2 offsetX offsetY

                                v1 =
                                    Math.Vector2.vec2 (squareSize + offsetX) offsetY

                                v2 =
                                    Math.Vector2.vec2 (squareSize + offsetX) (squareSize + offsetY)

                                v3 =
                                    Math.Vector2.vec2 offsetX (squareSize + offsetY)
                            in
                            [ ( { position = v0, color = color }
                              , { position = v1, color = color }
                              , { position = v2, color = color }
                              )
                            , ( { position = v0, color = color }
                              , { position = v2, color = color }
                              , { position = v3, color = color }
                              )
                            ]
                        )
            )
        |> WebGL.triangles


type alias PlayerUniforms =
    { view : Mat4, model : Mat4 }


vertexShader : Shader Vertex PlayerUniforms { vcolor : Vec4 }
vertexShader =
    [glsl|
attribute vec2 position;
attribute vec3 color;
varying vec4 vcolor;
uniform mat4 view;
uniform mat4 model;


void main () {
    gl_Position = view * model * vec4(position, 0.0, 1.0);

    vcolor = vec4(color.xyz,1.0);


}

|]


fragmentShader : Shader {} PlayerUniforms { vcolor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec4 vcolor;

        void main () {
            gl_FragColor = vcolor;
        }
    |]


backgroundVertexShader : Shader { position : Vec2 } { view : Vec2, viewZoom : Float, windowSize : Vec2 } { worldCoordinate : Vec2 }
backgroundVertexShader =
    [glsl|
attribute vec2 position;
varying vec2 worldCoordinate;
uniform vec2 view;
uniform float viewZoom;
uniform vec2 windowSize;

void main () {
    gl_Position = vec4(position, 0.0, 1.0);

    worldCoordinate = windowSize * position / viewZoom + view * 2.0;
}

|]


backgroundFragmentShader : Shader {} { a | windowSize : Vec2 } { worldCoordinate : Vec2 }
backgroundFragmentShader =
    [glsl|
        precision mediump float;
        varying vec2 worldCoordinate;

        float modI(float a,float b) {
            float m=a-floor((a+0.5)/b)*b;
            return floor(m+0.5);
        }

        void main () {
            float primaryThickness = 9.0;
            float secondaryThickness = 3.0;
            int x0 = modI(worldCoordinate.x + primaryThickness * 0.5, 800.0) <= primaryThickness ? 1 : 0;
            int y0 = modI(worldCoordinate.y + primaryThickness * 0.5, 800.0) <= primaryThickness ? 1 : 0;
            int x1 = modI(worldCoordinate.x + secondaryThickness * 0.5, 200.0) <= secondaryThickness ? 1 : 0;
            int y1 = modI(worldCoordinate.y + secondaryThickness * 0.5, 200.0) <= secondaryThickness ? 1 : 0;
            float value = x0 + y0 >= 1 ? 0.5 : x1 + y1 >= 1 ? 0.7 : 0.95;
            gl_FragColor = vec4(value, value, value, 1.0);
        }
    |]


subscriptions : AudioData -> FrontendModel_ -> Subscription FrontendOnly FrontendMsg_
subscriptions _ model =
    Subscription.batch
        [ Ports.devicePixelRatioResponse (Quantity.Quantity >> Quantity.per Pixels.pixel >> GotDevicePixelRatio)
        , Effect.Time.every (Duration.milliseconds 100) RandomInput
        , Effect.Browser.Events.onResize
            (\width height -> WindowResized { width = Pixels.pixels width, height = Pixels.pixels height })
        , case model of
            Loading _ ->
                Subscription.none

            Loaded _ ->
                Subscription.batch
                    [ Subscription.map KeyMsg Keyboard.subscriptions
                    , Effect.Browser.Events.onAnimationFrame AnimationFrame
                    ]
        ]
