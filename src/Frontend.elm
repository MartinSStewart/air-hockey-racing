module Frontend exposing (app, init, update, updateFromBackend, view)

import Angle
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Audio exposing (Audio, AudioCmd, AudioData)
import Axis2d
import Browser exposing (UrlRequest(..))
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Effect.Browser.Dom
import Effect.Browser.Events
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Effect.WebGL as WebGL exposing (Shader)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Input
import Env
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Pointer
import Html.Events.Extra.Touch
import Id exposing (Id)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Lobby exposing (LobbyPreview)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Ports
import Quantity exposing (Quantity(..), Rate)
import Sounds exposing (Sounds)
import Time
import Timeline exposing (FrameId)
import Types exposing (..)
import UiColors
import Url exposing (Url)
import User exposing (UserId)
import Vector2d exposing (Vector2d)
import WebGL.Settings


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


audio : AudioData -> FrontendModel_ -> Audio
audio audioData model =
    (case model of
        Loading _ ->
            Audio.silence

        Loaded loaded ->
            case loaded.lastButtonPress of
                Just lastButtonPress ->
                    Audio.audio loaded.sounds.buttonPress lastButtonPress

                Nothing ->
                    Audio.silence
    )
        |> Audio.offsetBy (Duration.milliseconds 30)


loadedInit :
    FrontendLoading
    -> Sounds
    -> ( Id UserId, LobbyData )
    -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit loading sounds ( userId, lobbyData ) =
    ( Loaded
        { key = loading.key
        , currentKeys = []
        , previousKeys = []
        , windowSize = loading.windowSize
        , devicePixelRatio = loading.devicePixelRatio
        , time = loading.time
        , page = LobbyPage lobbyData
        , sounds = sounds
        , lastButtonPress = Nothing
        , userId = userId
        , pingStartTime = Just loading.time
        , pingData = Nothing
        }
    , Effect.Lamdera.sendToBackend PingRequest
    , Audio.cmdNone
    )


tryLoadedInit : FrontendLoading -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
tryLoadedInit loading =
    Maybe.map2
        (loadedInit loading)
        (Sounds.loadingFinished loading.sounds)
        loading.initData
        |> Maybe.withDefault ( Loading loading, Command.none, Audio.cmdNone )


init : Url -> Effect.Browser.Navigation.Key -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
init url key =
    ( Loading
        { key = key
        , windowSize = { width = Pixels.pixels 1920, height = Pixels.pixels 1080 }
        , devicePixelRatio = Quantity 1
        , time = Effect.Time.millisToPosix 0
        , initData = Nothing
        , sounds = Dict.empty
        }
    , Effect.Task.perform
        (\{ viewport } ->
            WindowResized
                { width = round viewport.width |> Pixels.pixels
                , height = round viewport.height |> Pixels.pixels
                }
        )
        Effect.Browser.Dom.getViewport
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

                _ ->
                    ( model, Command.none, Audio.cmdNone )

        Loaded frontendLoaded ->
            updateLoaded msg frontendLoaded
                |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))


updateLoaded : FrontendMsg_ -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoaded msg model =
    case msg of
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

        AnimationFrame time ->
            let
                model2 =
                    { model | time = time, previousKeys = model.currentKeys }
            in
            case model2.page of
                MatchPage match ->
                    let
                        ( cache, _ ) =
                            Timeline.getStateAt
                                gameUpdate
                                (timeToFrameId time match)
                                match.timelineCache
                                newTimeline

                        newZoom =
                            if keyPressed (Character "Q") model then
                                match.zoom * 2

                            else if keyPressed (Character "W") model then
                                match.zoom / 2

                            else
                                match.zoom

                        previousInput : Maybe (Direction2d WorldCoordinate)
                        previousInput =
                            getInputDirection model.windowSize model.previousKeys match.previousTouchPosition

                        input : Maybe (Direction2d WorldCoordinate)
                        input =
                            getInputDirection model.windowSize model.currentKeys match.touchPosition

                        inputUnchanged : Bool
                        inputUnchanged =
                            previousInput == input

                        newTimeline =
                            if inputUnchanged then
                                match.timeline

                            else
                                Timeline.addInput_
                                    (timeToFrameId model2.time match)
                                    { userId = model2.userId, input = input }
                                    match.timeline
                    in
                    ( { model2
                        | page =
                            MatchPage
                                { match
                                    | timeline = newTimeline
                                    , timelineCache = cache
                                    , zoom = newZoom
                                    , previousTouchPosition = match.touchPosition
                                }
                      }
                    , if inputUnchanged then
                        Command.none

                      else
                        Effect.Lamdera.sendToBackend (MatchInputRequest match.matchId time input)
                    )

                LobbyPage _ ->
                    ( model2, Command.none )

        PressedCreateLobby ->
            ( model, Effect.Lamdera.sendToBackend CreateLobbyRequest )

        PressedJoinLobby lobbyId ->
            ( model, Effect.Lamdera.sendToBackend (JoinLobbyRequest lobbyId) )

        PressedStartMatch ->
            ( model, Effect.Lamdera.sendToBackend StartMatchRequest )

        SoundLoaded _ _ ->
            -- Shouldn't happen
            ( model, Command.none )

        PointerDown event ->
            case model.page of
                MatchPage matchPage ->
                    ( { model
                        | page =
                            MatchPage
                                { matchPage
                                    | touchPosition =
                                        case event.targetTouches ++ event.changedTouches ++ event.touches of
                                            head :: _ ->
                                                Point2d.fromTuple Pixels.pixels head.clientPos |> Just

                                            _ ->
                                                matchPage.touchPosition
                                }
                      }
                    , Command.none
                    )

                LobbyPage _ ->
                    ( model, Command.none )

        PointerMoved event ->
            case model.page of
                MatchPage matchPage ->
                    ( { model
                        | page =
                            MatchPage
                                { matchPage
                                    | touchPosition = Point2d.fromTuple Pixels.pixels event.pointer.clientPos |> Just
                                }
                      }
                    , Command.none
                    )

                LobbyPage _ ->
                    ( model, Command.none )

        PointerUp _ ->
            case model.page of
                MatchPage matchPage ->
                    ( { model | page = MatchPage { matchPage | touchPosition = Nothing } }
                    , Command.none
                    )

                LobbyPage _ ->
                    ( model, Command.none )


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
                            >> round
                            >> toFloat
                            >> Direction2d.degrees
                        )

            Nothing ->
                Nothing

    else
        directionToOffset input


timeToFrameId : Effect.Time.Posix -> MatchPage_ -> Id FrameId
timeToFrameId time matchState =
    Duration.from matchState.startTime time
        |> (\a -> Quantity.ratio a frameDuration)
        |> round
        |> Id.fromInt


frameIdToTime : Id FrameId -> MatchPage_ -> Effect.Time.Posix
frameIdToTime frame matchState =
    Quantity.multiplyBy (Id.toInt frame |> toFloat) frameDuration
        |> Duration.addTo matchState.startTime


frameDuration : Duration
frameDuration =
    Duration.seconds (1 / 60)


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


keyPressed : Keyboard.Key -> { a | currentKeys : List Keyboard.Key, previousKeys : List Keyboard.Key } -> Bool
keyPressed key { currentKeys, previousKeys } =
    List.any ((==) key) currentKeys && not (List.any ((==) key) previousKeys)


updateFromBackend : AudioData -> ToFrontend -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
updateFromBackend audioData msg model =
    case ( model, msg ) of
        ( Loading loading, ClientInit userId initData ) ->
            { loading | initData = Just ( userId, initData ) } |> tryLoadedInit

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))

        _ ->
            ( model, Command.none, Audio.cmdNone )


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoadedFromBackend msg model =
    case msg of
        ClientInit _ _ ->
            -- Handled in updateFromBackend
            ( model, Command.none )

        CreateLobbyResponse lobbyId lobby ->
            ( case model.page of
                LobbyPage lobbyData ->
                    { model
                        | page =
                            LobbyPage
                                { lobbyData
                                    | lobbies = Dict.insert lobbyId (Lobby.preview lobby) lobbyData.lobbies
                                    , currentLobby = Just { id = lobbyId, lobby = lobby }
                                }
                    }

                _ ->
                    model
            , Command.none
            )

        JoinLobbyResponse lobbyId result ->
            ( case model.page of
                LobbyPage lobbyData ->
                    case result of
                        Ok lobby ->
                            { model
                                | page =
                                    LobbyPage
                                        { lobbyData | currentLobby = Just { id = lobbyId, lobby = lobby } }
                            }

                        Err LobbyNotFound ->
                            model

                _ ->
                    model
            , Command.none
            )

        JoinLobbyBroadcast lobbyId userId ->
            ( case model.page of
                LobbyPage lobbyData ->
                    { model
                        | page =
                            LobbyPage
                                { lobbyData
                                    | currentLobby =
                                        case lobbyData.currentLobby of
                                            Just currentLobby ->
                                                if currentLobby.id == lobbyId then
                                                    Just { currentLobby | lobby = Lobby.joinUser userId currentLobby.lobby }

                                                else
                                                    Just currentLobby

                                            Nothing ->
                                                Nothing
                                }
                    }

                MatchPage matchState ->
                    model
            , Command.none
            )

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

        StartMatchBroadcast matchId serverStartTime userIds ->
            case model.page of
                LobbyPage lobbyData ->
                    case lobbyData.currentLobby of
                        Just _ ->
                            ( { model
                                | page =
                                    MatchPage
                                        { startTime = serverStartTime
                                        , localStartTime = model.time
                                        , timeline = Set.empty
                                        , timelineCache = initMatch userIds |> Timeline.init
                                        , userIds = userIds
                                        , matchId = matchId
                                        , zoom = 1
                                        , touchPosition = Nothing
                                        , previousTouchPosition = Nothing
                                        }
                              }
                            , Command.none
                            )

                        Nothing ->
                            ( model, Command.none )

                MatchPage _ ->
                    ( model, Command.none )

        MatchInputBroadcast matchId time event ->
            ( case model.page of
                MatchPage match ->
                    if match.matchId == matchId then
                        let
                            ( newCache, newTimeline ) =
                                Timeline.addInput (timeToFrameId time match) event match.timelineCache match.timeline
                        in
                        { model | page = MatchPage { match | timelineCache = newCache, timeline = newTimeline } }

                    else
                        model

                LobbyPage _ ->
                    model
            , Command.none
            )

        PingResponse serverTime ->
            case model.pingStartTime of
                Just pingStartTime ->
                    let
                        ( newLowEstimate, newHighEstimate ) =
                            case model.pingData of
                                Just oldPingData ->
                                    ( Duration.from pingStartTime serverTime |> Quantity.min oldPingData.lowEstimate
                                    , Duration.from model.time serverTime |> Quantity.max oldPingData.highEstimate
                                    )

                                Nothing ->
                                    ( Duration.from pingStartTime serverTime
                                    , Duration.from model.time serverTime
                                    )
                    in
                    ( { model
                        | pingData =
                            Just
                                { roundTripTime = Duration.from pingStartTime model.time
                                , lowEstimate = newLowEstimate
                                , highEstimate = newHighEstimate
                                , serverTime = serverTime
                                , sendTime = pingStartTime
                                , receiveTime = model.time
                                }
                        , pingStartTime = Just model.time
                      }
                    , Effect.Lamdera.sendToBackend PingRequest
                    )

                Nothing ->
                    ( model, Command.none )


initMatch : Nonempty (Id UserId) -> MatchState
initMatch userIds =
    { players =
        List.Nonempty.toList userIds
            |> List.map
                (\userId ->
                    ( userId
                    , { position = Point2d.origin
                      , velocity = Vector2d.zero
                      , input = Nothing
                      }
                    )
                )
            |> Dict.fromList
    }


lostConnection : FrontendLoaded -> Bool
lostConnection model =
    False


view : AudioData -> FrontendModel_ -> Browser.Document FrontendMsg_
view audioData model =
    { title =
        case model of
            Loading _ ->
                "Hockey Puck Racer"

            Loaded loadedModel ->
                if lostConnection loadedModel then
                    "(offline) Hockey Puck Racer"

                else
                    "Hockey Puck Racer"
    , body =
        [ case model of
            Loading loading ->
                Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
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
    Element.layout
        [ (if Env.isProduction then
            Element.none

           else
            Element.column
                [ Element.alignRight, Element.padding 4, Element.spacing 4 ]
                [ Id.toInt model.userId
                    |> String.fromInt
                    |> (++) "You are User "
                    |> Element.text
                , case model.pingData of
                    Just pingData ->
                        Element.column
                            [ Element.spacing 4 ]
                            [ "Ping (ms): "
                                ++ String.fromInt (round (Duration.inMilliseconds pingData.roundTripTime))
                                |> Element.text
                            , "Server offset (low): "
                                ++ String.fromInt (round (Duration.inMilliseconds pingData.lowEstimate))
                                |> Element.text
                            , "Server offset (high): "
                                ++ String.fromInt (round (Duration.inMilliseconds pingData.highEstimate))
                                |> Element.text
                            , "Send time: " ++ timestamp pingData.sendTime |> Element.text
                            , "Receive time: " ++ timestamp pingData.receiveTime |> Element.text
                            , "Server time: " ++ timestamp pingData.serverTime |> Element.text
                            ]

                    Nothing ->
                        Element.text "No ping"
                ]
          )
            |> Element.inFront
        , canvasView model |> Element.behindContent
        , Element.clip
        ]
        (case model.page of
            LobbyPage lobbyData ->
                case lobbyData.currentLobby of
                    Just currentLobby ->
                        let
                            users : List (Id UserId)
                            users =
                                Lobby.allUsers currentLobby.lobby |> List.Nonempty.toList
                        in
                        Element.column
                            []
                            [ button PressedStartMatch (Element.text "Start match")
                            , Element.column
                                []
                                (List.map
                                    (\userId -> Id.toInt userId |> String.fromInt |> (++) "User " |> Element.text)
                                    users
                                )
                            ]

                    Nothing ->
                        Element.column
                            [ Element.spacing 16 ]
                            [ button PressedCreateLobby (Element.text "Create lobby")
                            , Element.column
                                [ Element.spacing 8 ]
                                [ Element.text "Lobbies"
                                , lobbyData
                                    |> .lobbies
                                    |> Dict.toList
                                    |> List.map lobbyRowView
                                    |> Element.column []
                                ]
                            ]

            MatchPage matchPage ->
                Element.el
                    (Element.width Element.fill
                        :: Element.height Element.fill
                        :: Element.htmlAttribute (Html.Events.Extra.Touch.onStart PointerDown)
                        :: Element.htmlAttribute (Html.Events.Extra.Pointer.onCancel PointerUp)
                        :: Element.htmlAttribute (Html.Events.Extra.Pointer.onLeave PointerUp)
                        :: Element.htmlAttribute (Html.Events.Extra.Pointer.onUp PointerUp)
                        :: (case matchPage.touchPosition of
                                Just _ ->
                                    [ Element.htmlAttribute (Html.Events.Extra.Pointer.onMove PointerMoved) ]

                                Nothing ->
                                    []
                           )
                    )
                    Element.none
         --Element.column
         --    []
         --    [ Element.text "Players: "
         --    , Element.row
         --        [ Element.spacing 16 ]
         --        [ List.Nonempty.toList matchPage.userIds
         --            |> List.map (Id.toInt >> String.fromInt >> (++) "User " >> Element.text)
         --            |> Element.column [ Element.spacing 4 ]
         --        , Element.column
         --            []
         --            [ "Start time:  " ++ timestamp matchPage.startTime |> Element.text
         --            , "Local time: " ++ timestamp matchPage.localStartTime |> Element.text
         --            ]
         --        ]
         --    , Element.row
         --        [ Element.Font.size 14, Element.spacing 16 ]
         --        [ inputsView matchPage
         --        , List.map
         --            (\( frameId, _ ) -> Id.toInt frameId |> String.fromInt |> Element.text)
         --            matchPage.timelineCache.cache
         --            |> Element.column [ Element.alignTop ]
         --        ]
         --    ]
        )


timestamp : Time.Posix -> String
timestamp time =
    String.fromInt (Time.toHour Time.utc time)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute Time.utc time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toSecond Time.utc time))
        ++ "."
        ++ String.padLeft 4 '0' (String.fromInt (Time.toMillis Time.utc time))


inputsView : MatchPage_ -> Element msg
inputsView matchPage =
    List.map
        (\( frameId, event ) ->
            String.fromInt (Id.toInt frameId)
                ++ ", User "
                ++ String.fromInt (Id.toInt event.userId)
                ++ ", "
                ++ (case event.input of
                        Just input ->
                            Direction2d.toAngle input |> Angle.inDegrees |> round |> String.fromInt |> (\a -> a ++ "°")

                        Nothing ->
                            "No input"
                   )
                |> Element.text
        )
        (Set.toList matchPage.timeline)
        |> Element.column [ Element.alignTop ]


button : msg -> Element msg -> Element msg
button onPress label =
    Element.Input.button
        [ Element.Background.color <| Element.rgb 0.9 0.9 0.85
        , Element.padding 4
        ]
        { onPress = Just onPress
        , label = label
        }


lobbyRowView : ( Id LobbyId, LobbyPreview ) -> Element FrontendMsg_
lobbyRowView ( lobbyId, lobby ) =
    Element.row
        []
        [ Element.text <| "Players: " ++ String.fromInt lobby.userCount
        , button (PressedJoinLobby lobbyId) (Element.text "Join")
        ]


offlineWarningView : Element msg
offlineWarningView =
    Element.text "⚠ Unable to reach server. Your changes won't be saved."
        |> Element.el
            [ Element.Background.color UiColors.warning
            , Element.padding 8
            , Element.Border.rounded 4
            , Element.centerX
            , Element.moveUp 8
            ]


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
        [ WebGL.alpha False, WebGL.antialias ]
        [ Html.Attributes.width windowWidth
        , Html.Attributes.height windowHeight
        , Html.Attributes.style "width" (String.fromInt (Pixels.inPixels cssWindowWidth) ++ "px")
        , Html.Attributes.style "height" (String.fromInt (Pixels.inPixels cssWindowHeight) ++ "px")
        ]
        (case model.page of
            MatchPage match ->
                let
                    ( _, state ) =
                        Timeline.getStateAt gameUpdate (timeToFrameId model.time match) match.timelineCache match.timeline

                    { x, y } =
                        case Dict.get model.userId state.players of
                            Just player ->
                                Point2d.toMeters player.position

                            Nothing ->
                                { x = 0, y = 0 }

                    viewMatrix =
                        Mat4.makeScale3
                            (match.zoom * 2 / toFloat windowWidth)
                            (match.zoom * 2 / toFloat windowHeight)
                            1
                            |> Mat4.translate3 -x -y 0
                in
                WebGL.entityWith
                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                    backgroundVertexShader
                    backgroundFragmentShader
                    square
                    { view = Math.Vector2.vec2 x y
                    , viewZoom = match.zoom
                    , windowSize = Math.Vector2.vec2 (toFloat windowWidth) (toFloat windowHeight)
                    }
                    :: List.map
                        (\player ->
                            WebGL.entityWith
                                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                playerVertexShader
                                playerFragmentShader
                                circle
                                { view = viewMatrix
                                , model = pointToMatrix player.position |> Mat4.scale3 100 100 100
                                , color = Math.Vector3.vec3 0.2 0.3 0
                                }
                        )
                        (Dict.values state.players)

            LobbyPage _ ->
                []
        )
        |> Element.html


wall =
    []


gameUpdate : List TimelineEvent -> MatchState -> MatchState
gameUpdate inputs model =
    let
        newModel =
            List.foldl
                (\{ userId, input } model2 ->
                    { players = Dict.update userId (Maybe.map (\a -> { a | input = input })) model2.players }
                )
                model
                inputs
    in
    { players =
        Dict.map
            (\_ a ->
                { a
                    | position = Point2d.translateBy a.velocity a.position
                    , velocity =
                        (case a.input of
                            Just input ->
                                Direction2d.toVector input
                                    |> Vector2d.scaleBy 0.5
                                    |> Vector2d.unwrap
                                    |> Vector2d.unsafe

                            Nothing ->
                                Vector2d.zero
                        )
                            |> Vector2d.plus a.velocity
                            |> Vector2d.scaleBy 0.99
                }
            )
            newModel.players
    }


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


square : WebGL.Mesh { position : Vec2 }
square =
    WebGL.triangleFan
        [ { position = Math.Vector2.vec2 -1 -1 }
        , { position = Math.Vector2.vec2 1 -1 }
        , { position = Math.Vector2.vec2 1 1 }
        , { position = Math.Vector2.vec2 -1 1 }
        ]


circle =
    let
        detail =
            64
    in
    List.range 0 (detail - 1)
        |> List.map
            (\index ->
                let
                    t =
                        pi * 2 * toFloat index / detail
                in
                { position = Math.Vector2.vec2 (cos t) (sin t) }
            )
        |> WebGL.triangleFan


type alias Vertex =
    { position : Vec2 }


type alias PlayerUniforms =
    { view : Mat4, model : Mat4, color : Vec3 }


playerVertexShader : Shader Vertex PlayerUniforms { vcolor : Vec4 }
playerVertexShader =
    [glsl|
attribute vec2 position;
varying vec4 vcolor;
uniform mat4 view;
uniform mat4 model;
uniform vec3 color;

void main () {
    gl_Position = view * model * vec4(position, 0.0, 1.0);

    vcolor = vec4(color.xyz,1.0);


}

|]


playerFragmentShader : Shader {} PlayerUniforms { vcolor : Vec4 }
playerFragmentShader =
    [glsl|
        precision mediump float;
        varying vec4 vcolor;

        void main () {
            gl_FragColor = vcolor;
        }
    |]


backgroundVertexShader : Shader Vertex { view : Vec2, viewZoom : Float, windowSize : Vec2 } { worldCoordinate : Vec2 }
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
            float value = x0 + y0 >= 1 ? 0.3 : x1 + y1 >= 1 ? 0.7 : 1.0;
            gl_FragColor = vec4(value, value, value, 1.0);
        }
    |]


subscriptions : AudioData -> FrontendModel_ -> Subscription FrontendOnly FrontendMsg_
subscriptions _ model =
    Subscription.batch
        [ Ports.devicePixelRatioResponse (Quantity.Quantity >> Quantity.per Pixels.pixel >> GotDevicePixelRatio)
        , Effect.Browser.Events.onResize
            (\width height -> WindowResized { width = Pixels.pixels width, height = Pixels.pixels height })
        , case model of
            Loading _ ->
                Subscription.none

            Loaded loaded ->
                Subscription.batch
                    [ Subscription.map KeyMsg Keyboard.subscriptions
                    , Effect.Browser.Events.onAnimationFrame AnimationFrame
                    ]
        ]
