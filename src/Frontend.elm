module Frontend exposing (app, init, update, updateFromBackend, view)

import AssocList as Dict exposing (Dict)
import AssocSet as Set
import Audio exposing (Audio, AudioCmd, AudioData)
import Browser exposing (UrlRequest(..))
import Duration exposing (Duration)
import Effect.Browser.Dom
import Effect.Browser.Events
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Input
import Html exposing (Html)
import Id exposing (Id)
import Keyboard
import Lamdera
import List.Extra as List
import List.Nonempty
import Lobby exposing (LobbyPreview)
import Pixels exposing (Pixels)
import Ports
import Quantity exposing (Quantity(..), Rate)
import Sounds exposing (Sounds)
import Timeline exposing (FrameId)
import Types exposing (..)
import UiColors
import Url exposing (Url)
import User exposing (UserId)


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
    -> ( FrontendModel_, Command FrontendOnly toMsg FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit loading sounds ( userId, lobbyData ) =
    ( Loaded
        { key = loading.key
        , pressedKeys = []
        , previousKeys = []
        , windowSize = loading.windowSize
        , devicePixelRatio = loading.devicePixelRatio
        , time = loading.time
        , page = LobbyPage lobbyData
        , sounds = sounds
        , lastButtonPress = Nothing
        , userId = userId
        }
    , Command.none
    , Audio.cmdNone
    )


tryLoadedInit : FrontendLoading -> ( FrontendModel_, Command FrontendOnly toMsg FrontendMsg_, AudioCmd FrontendMsg_ )
tryLoadedInit loading =
    Maybe.map2
        (loadedInit loading)
        (Sounds.loadingFinished loading.sounds)
        loading.initData
        |> Maybe.withDefault ( Loading loading, Command.none, Audio.cmdNone )


init : Url -> Effect.Browser.Navigation.Key -> ( FrontendModel_, Command FrontendOnly toMsg FrontendMsg_, AudioCmd FrontendMsg_ )
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
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }
            , Command.none
            )

        WindowResized windowSize ->
            windowResizedUpdate windowSize model

        GotDevicePixelRatio devicePixelRatio ->
            devicePixelRatioUpdate devicePixelRatio model

        AnimationFrame time ->
            ( { model
                | time = time
                , previousKeys = model.pressedKeys
              }
            , Command.none
            )

        PressedCreateLobby ->
            ( model, Effect.Lamdera.sendToBackend CreateLobbyRequest )

        PressedJoinLobby lobbyId ->
            ( model, Effect.Lamdera.sendToBackend (JoinLobbyRequest lobbyId) )

        PressedStartMatch ->
            ( model, Effect.Lamdera.sendToBackend StartMatchRequest )

        SoundLoaded _ _ ->
            -- Shouldn't happen
            ( model, Command.none )


timeToFrameId : Effect.Time.Posix -> MatchState -> Id FrameId
timeToFrameId time matchState =
    Duration.from matchState.startTime time
        |> (\a -> Quantity.ratio a frameDuration)
        |> round
        |> Id.fromInt


frameIdToTime : Id FrameId -> MatchState -> Effect.Time.Posix
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


keyDown : Keyboard.Key -> { a | pressedKeys : List Keyboard.Key } -> Bool
keyDown key { pressedKeys } =
    List.any ((==) key) pressedKeys


updateFromBackend : AudioData -> ToFrontend -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly toMsg FrontendMsg_, AudioCmd FrontendMsg_ )
updateFromBackend audioData msg model =
    let
        _ =
            Debug.log "updateFromBackend" msg
    in
    case ( model, msg ) of
        ( Loading loading, ClientInit userId initData ) ->
            { loading | initData = Just ( userId, initData ) } |> tryLoadedInit

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))

        _ ->
            ( model, Command.none, Audio.cmdNone )


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly toMsg FrontendMsg_ )
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

        StartMatchBroadcast userIds ->
            case model.page of
                LobbyPage lobbyData ->
                    case lobbyData.currentLobby of
                        Just _ ->
                            ( { model
                                | page =
                                    MatchPage
                                        { startTime = model.time
                                        , timeline = []
                                        , timelineCache = Timeline.init { players = Dict.empty }
                                        , userIds = userIds
                                        }
                              }
                            , Command.none
                            )

                        Nothing ->
                            ( model, Command.none )

                MatchPage _ ->
                    ( model, Command.none )


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


stepSize : Int
stepSize =
    33


loadedView : FrontendLoaded -> Html FrontendMsg_
loadedView model =
    Element.layout
        [ Id.toInt model.userId
            |> String.fromInt
            |> (++) "You are User "
            |> Element.text
            |> Element.el [ Element.alignRight, Element.padding 4 ]
            |> Element.inFront
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
                                (List.map (\userId -> Id.toInt userId |> String.fromInt |> (++) "User " |> Element.text) users)
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

            MatchPage matchState ->
                Element.column
                    []
                    [ Element.text "Players: "
                    , List.Nonempty.toList matchState.userIds
                        |> List.map (Id.toInt >> String.fromInt >> (++) "User " >> Element.text)
                        |> Element.column [ Element.spacing 4 ]
                    ]
        )


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



--canvasView : FrontendLoaded -> Html FrontendMsg
--canvasView model =
--    let
--        ( windowWidth, windowHeight ) =
--            actualCanvasSize
--
--        ( cssWindowWidth, cssWindowHeight ) =
--            canvasSize
--
--        { canvasSize, actualCanvasSize } =
--            findPixelPerfectSize model
--
--        { x, y } =
--            { x = 0, y = 0 }
--
--        zoomFactor =
--            1
--
--        viewMatrix =
--            Mat4.makeScale3 (toFloat zoomFactor * 2 / toFloat windowWidth) (toFloat zoomFactor * -2 / toFloat windowHeight) 1
--                |> Mat4.translate3
--                    (negate <| toFloat <| round x)
--                    (negate <| toFloat <| round y)
--                    0
--    in
--    WebGL.toHtmlWith
--        [ WebGL.alpha False, WebGL.antialias ]
--        [ Html.Attributes.width windowWidth
--        , Html.Attributes.height windowHeight
--        , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
--        , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
--        ]
--        []


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
