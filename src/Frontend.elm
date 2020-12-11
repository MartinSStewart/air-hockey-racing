port module Frontend exposing (app, init, update, updateFromBackend, view)

import Angle
import Audio exposing (Audio, AudioCmd, AudioData)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Camera3d
import Color
import Dict
import Direction3d
import Duration exposing (Duration)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Input
import Html exposing (Html)
import Id exposing (Id)
import IdDict exposing (IdDict)
import Illuminance
import Json.Decode
import Json.Encode
import Keyboard
import Keyboard.Arrows
import Lamdera
import Length
import List.Extra as List
import List.Nonempty
import LocalModel exposing (Config, LocalModel)
import Match exposing (Match)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity(..), Rate)
import Scene3d
import Scene3d.Light
import Sounds exposing (Sounds)
import Task
import Time
import Types exposing (..)
import UiColors
import Url exposing (Url)
import User exposing (UserId)
import Viewpoint3d


port martinsstewart_elm_device_pixel_ratio_from_js : (Float -> msg) -> Sub msg


port martinsstewart_elm_device_pixel_ratio_to_js : () -> Cmd msg


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


app =
    Audio.lamderaFrontendWithAudio
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        , audio = audio
        , audioPort = { fromJS = audioPortFromJS, toJS = audioPortToJS }
        }


audio : AudioData -> FrontendModel_ -> Audio
audio audioData model =
    (case model of
        Loading _ ->
            Audio.silence

        Loaded loaded ->
            let
                _ =
                    Debug.log " " (Audio.length loaded.sounds.buttonPress audioData)
            in
            case loaded.lastButtonPress of
                Just lastButtonPress ->
                    Audio.audio loaded.sounds.buttonPress lastButtonPress

                Nothing ->
                    Audio.silence
    )
        |> Audio.offsetBy (Duration.milliseconds 30)


loadedInit : FrontendLoading -> Sounds -> ClientInitData -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit loading sounds { userId, lobbies } =
    ( Loaded
        { key = loading.key
        , pressedKeys = []
        , previousKeys = []
        , windowSize = loading.windowSize
        , devicePixelRatio = loading.devicePixelRatio
        , time = loading.time
        , localModel = LocalModel.init { userId = userId, lobbies = lobbies, match = Nothing }
        , matchCache = IdDict.empty
        , visibleMatch = Nothing
        , sounds = sounds
        , lastButtonPress = Nothing
        }
    , Cmd.none
    , Audio.cmdNone
    )


tryLoadedInit : FrontendLoading -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
tryLoadedInit loading =
    Maybe.map2
        (loadedInit loading)
        (Sounds.loadingFinished loading.sounds)
        loading.initData
        |> Maybe.withDefault ( Loading loading, Cmd.none, Audio.cmdNone )


init : Url -> Browser.Navigation.Key -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
init url key =
    ( Loading
        { key = key
        , windowSize = { width = Pixels.pixels 1920, height = Pixels.pixels 1080 }
        , devicePixelRatio = Quantity 1
        , time = Time.millisToPosix 0
        , initData = Nothing
        , sounds = Dict.empty
        }
    , Task.perform
        (\{ viewport } ->
            WindowResized
                { width = round viewport.width |> Pixels.pixels
                , height = round viewport.height |> Pixels.pixels
                }
        )
        Browser.Dom.getViewport
    , Sounds.requestSounds SoundLoaded
    )


update : AudioData -> FrontendMsg_ -> FrontendModel_ -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
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
                    ( model, Cmd.none, Audio.cmdNone )

        Loaded frontendLoaded ->
            updateLoaded msg frontendLoaded
                |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))


updateLoaded : FrontendMsg_ -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg_ )
updateLoaded msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Browser.Navigation.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }
            , Cmd.none
            )

        WindowResized windowSize ->
            windowResizedUpdate windowSize model

        GotDevicePixelRatio devicePixelRatio ->
            devicePixelRatioUpdate devicePixelRatio model

        AnimationFrame time ->
            let
                newModel =
                    { model
                        | time = time
                        , previousKeys = model.pressedKeys
                        , matchCache =
                            case ( model.visibleMatch, LocalModel.localModel model.localModel |> .match ) of
                                ( Just visibleMatch, Just match ) ->
                                    IdDict.insert
                                        (timeToFrameId model.time match)
                                        visibleMatch
                                        model.matchCache
                                        |> IdDict.filter
                                            (\frameId _ ->
                                                frameIdToTime frameId match
                                                    |> Duration.from time
                                                    |> Quantity.lessThan Duration.second
                                            )

                                _ ->
                                    model.matchCache
                    }

                localModel =
                    LocalModel.localModel model.localModel

                newModel2 =
                    { newModel
                        | visibleMatch =
                            case localModel.match of
                                Just match ->
                                    let
                                        users =
                                            localModel.userId :: match.otherUsers
                                    in
                                    getMatch (timeToFrameId model.time match) newModel.matchCache users match |> Just

                                Nothing ->
                                    model.visibleMatch
                    }

                move =
                    Keyboard.Arrows.wasdDirection model.pressedKeys
            in
            case (LocalModel.localModel newModel.localModel).match of
                Just matchState ->
                    if move == Keyboard.Arrows.wasdDirection model.previousKeys then
                        ( newModel2, Cmd.none )

                    else
                        localChange (MatchInput (timeToFrameId time matchState) move) newModel2

                Nothing ->
                    ( newModel2, Cmd.none )

        CreateLobbyPressed ->
            localChange CreateLobby model
                |> Tuple.mapFirst (\m -> { m | lastButtonPress = Just model.time })

        JoinLobbyPressed lobbyId ->
            localChange (JoinLobby lobbyId) model
                |> Tuple.mapFirst (\m -> { m | lastButtonPress = Just model.time })

        StartMatchPressed ->
            localChange (StartMatch model.time) model
                |> Tuple.mapFirst (\m -> { m | lastButtonPress = Just model.time })

        SoundLoaded _ _ ->
            -- Shouldn't happen
            ( model, Cmd.none )


timeToFrameId : Time.Posix -> MatchState -> Id FrameId
timeToFrameId time matchState =
    Duration.from matchState.startTime time
        |> (\a -> Quantity.ratio a frameDuration)
        |> round
        |> Id.fromInt


frameIdToTime : Id FrameId -> MatchState -> Time.Posix
frameIdToTime frame matchState =
    Quantity.multiplyBy (Id.toInt frame |> toFloat) frameDuration
        |> Duration.addTo matchState.startTime


frameDuration : Duration
frameDuration =
    Duration.seconds (1 / 60)


localChange :
    SessionChange
    -> FrontendLoaded
    -> ( FrontendLoaded, Cmd FrontendMsg_ )
localChange change model =
    ( { model
        | localModel =
            LocalModel.update
                localModelConfig
                model.time
                (SessionChange change)
                model.localModel
      }
    , Lamdera.sendToBackend (SessionChange_ change)
    )


localModelConfig : Config ToFrontendChange Local
localModelConfig =
    let
        addEvent : TimelineEvent -> MatchState -> MatchState
        addEvent event match =
            { match | events = event :: match.events }
    in
    { msgEqual = (==)
    , update =
        \msg model ->
            case msg of
                SessionChange CreateLobby ->
                    { model
                        | lobbies =
                            IdDict.insert
                                (IdDict.size model.lobbies |> Id.fromInt)
                                { users = IdDict.singleton model.userId () }
                                model.lobbies
                    }

                BroadcastChange (BroadcastCreateLobby userId) ->
                    { model
                        | lobbies =
                            IdDict.insert
                                (IdDict.size model.lobbies |> Id.fromInt)
                                { users = IdDict.singleton userId () }
                                model.lobbies
                    }

                SessionChange (JoinLobby lobbyId) ->
                    { model
                        | lobbies =
                            IdDict.update
                                lobbyId
                                (Maybe.map (\lobby -> { lobby | users = IdDict.insert model.userId () lobby.users }))
                                model.lobbies
                    }

                BroadcastChange (BroadcastJoinLobby userId lobbyId) ->
                    { model
                        | lobbies =
                            IdDict.update
                                lobbyId
                                (Maybe.map (\lobby -> { lobby | users = IdDict.insert userId () lobby.users }))
                                model.lobbies
                    }

                SessionChange (StartMatch startTime) ->
                    IdDict.toList model.lobbies
                        |> List.find (Tuple.second >> .users >> IdDict.member model.userId)
                        |> Maybe.map
                            (\( lobbyId, lobby ) ->
                                { model
                                    | lobbies = IdDict.remove lobbyId model.lobbies
                                    , match =
                                        Just
                                            { startTime = startTime
                                            , events = []
                                            , otherUsers = IdDict.keys lobby.users
                                            }
                                }
                            )
                        |> Maybe.withDefault model

                BroadcastChange (BroadcastStartMatch time lobbyId) ->
                    case IdDict.get lobbyId model.lobbies of
                        Just lobby ->
                            { model | lobbies = IdDict.remove lobbyId model.lobbies }

                        Nothing ->
                            model

                SessionChange (MatchInput frameId input) ->
                    { model
                        | match =
                            Maybe.map
                                (addEvent { userId = model.userId, frameId = frameId, input = input })
                                model.match
                    }

                BroadcastChange (BroadcastMatchInput event) ->
                    { model | match = Maybe.map (addEvent event) model.match }
    }


windowResizedUpdate : WindowSize -> { b | windowSize : WindowSize } -> ( { b | windowSize : WindowSize }, Cmd msg )
windowResizedUpdate windowSize model =
    ( { model | windowSize = windowSize }, martinsstewart_elm_device_pixel_ratio_to_js () )


devicePixelRatioUpdate :
    Quantity Float (Rate WorldPixel Pixels)
    -> { b | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels) }
    -> ( { b | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels) }, Cmd msg )
devicePixelRatioUpdate devicePixelRatio model =
    ( { model | devicePixelRatio = devicePixelRatio }
    , Cmd.none
    )


keyDown : Keyboard.Key -> { a | pressedKeys : List Keyboard.Key } -> Bool
keyDown key { pressedKeys } =
    List.any ((==) key) pressedKeys


updateFromBackend : AudioData -> ToFrontend -> FrontendModel_ -> ( FrontendModel_, Cmd FrontendMsg_, AudioCmd FrontendMsg_ )
updateFromBackend audioData msg model =
    let
        _ =
            Debug.log "updateFromBackend" msg
    in
    case ( model, msg ) of
        ( Loading loading, ClientInit initData ) ->
            { loading | initData = Just initData } |> tryLoadedInit

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))

        _ ->
            ( model, Cmd.none, Audio.cmdNone )


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg_ )
updateLoadedFromBackend msg model =
    case msg of
        Change change ->
            ( { model
                | localModel =
                    LocalModel.updateFromBackend
                        localModelConfig
                        (List.Nonempty.fromElement change)
                        model.localModel
              }
            , Cmd.none
            )

        ClientInit _ ->
            -- Handled in updateFromBackend
            ( model, Cmd.none )


lostConnection : FrontendLoaded -> Bool
lostConnection model =
    False


view : AudioData -> FrontendModel_ -> Browser.Document FrontendMsg_
view audioData model =
    { title =
        case model of
            Loading _ ->
                "Weasel tanks"

            Loaded loadedModel ->
                if lostConnection loadedModel then
                    "Weasel tanks (offline)"

                else
                    "Weasel tanks"
    , body =
        [ case model of
            Loading _ ->
                Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (Element.text "Loading")

            Loaded loadedModel ->
                loadedView loadedModel
        ]
    }


getMatch :
    Id FrameId
    -> IdDict FrameId Match
    -> List (Id UserId)
    -> MatchState
    -> Match
getMatch frameId matchCache users matchState =
    if Id.toInt frameId <= 0 then
        Match.init users

    else
        case
            IdDict.toList matchCache
                |> List.filter (\( key, _ ) -> Id.toInt key > Id.toInt frameId)
                |> List.maximumBy (Tuple.first >> Id.toInt)
        of
            Just ( currentFrame, cachedMatch ) ->
                getMatchHelper frameId currentFrame cachedMatch

            Nothing ->
                Match.init users



--List.foldl
--    (\{ userId, time, input } ( currentTime, match ) ->
--        ( Time.posixToMillis currentTime |> (+) 16 |> Time.millisToPosix
--        , Match.step match
--        )
--    )
--    ( startTime, Match.init users )
--    (List.sortBy (.time >> Time.posixToMillis) events)
--    |> Tuple.second
--    |> Just


stepSize : Int
stepSize =
    33


getMatchHelper : Id FrameId -> Id FrameId -> Match -> Match
getMatchHelper targetTime currentTime match =
    if Id.toInt currentTime + stepSize < Id.toInt targetTime then
        getMatchHelper targetTime (Id.toInt currentTime + 1 |> Id.fromInt) (Match.step match)

    else
        match


loadedView : FrontendLoaded -> Html FrontendMsg_
loadedView model =
    let
        localModel =
            LocalModel.localModel model.localModel
    in
    Element.layout
        [ Element.clip
        , Element.behindContent <|
            case model.visibleMatch of
                Just match ->
                    let
                        { canvasSize, actualCanvasSize } =
                            findPixelPerfectSize model
                    in
                    Scene3d.custom
                        { dimensions = canvasSize
                        , camera =
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.lookAt
                                        { focalPoint = Point3d.origin
                                        , eyePoint = Point3d.meters 0 15 20
                                        , upDirection = Direction3d.z
                                        }
                                , verticalFieldOfView = Angle.degrees 30
                                }
                        , clipDepth = Length.meters 0.1
                        , background =
                            Scene3d.backgroundColor (Color.rgb 0.85 0.87 0.95)
                        , entities = Match.entities match
                        , antialiasing = model.devicePixelRatio |> (\(Quantity a) -> Scene3d.supersampling a)
                        , lights =
                            Scene3d.twoLights
                                (Scene3d.Light.directional
                                    (Scene3d.Light.castsShadows True)
                                    { chromaticity = Scene3d.Light.sunlight
                                    , intensity = Illuminance.lux 80000
                                    , direction = Direction3d.negativeZ
                                    }
                                )
                                (Scene3d.Light.soft
                                    { upDirection = Direction3d.z
                                    , chromaticity = Scene3d.Light.skylight
                                    , intensityAbove = Illuminance.lux 40000
                                    , intensityBelow = Illuminance.lux 10000
                                    }
                                )
                        , exposure = Scene3d.exposureValue 15
                        , toneMapping = Scene3d.noToneMapping
                        , whiteBalance = Scene3d.Light.sunlight
                        }
                        |> Element.html
                        |> Element.el [ Element.width Element.fill, Element.height Element.shrink ]

                Nothing ->
                    Element.none
        ]
        (case model.visibleMatch of
            Just _ ->
                Element.none

            Nothing ->
                Element.column
                    [ Element.spacing 16 ]
                    [ case getCurrentLobby localModel of
                        Just _ ->
                            button StartMatchPressed (Element.text "Start match")

                        Nothing ->
                            button CreateLobbyPressed (Element.text "Create lobby")
                    , Element.column
                        [ Element.spacing 8 ]
                        [ Element.text "Lobbies"
                        , localModel
                            |> .lobbies
                            |> IdDict.toList
                            |> List.map lobbyRowView
                            |> Element.column []
                        ]
                    ]
        )


getCurrentLobby : Local -> Maybe ( Id LobbyId, Lobby )
getCurrentLobby local =
    IdDict.toList local.lobbies
        |> List.find (Tuple.second >> .users >> IdDict.member local.userId)


button : msg -> Element msg -> Element msg
button onPress label =
    Element.Input.button
        [ Element.Background.color <| Element.rgb 0.9 0.9 0.85
        , Element.padding 4
        ]
        { onPress = Just onPress
        , label = label
        }


lobbyRowView : ( Id LobbyId, Lobby ) -> Element FrontendMsg_
lobbyRowView ( lobbyId, lobby ) =
    Element.row
        []
        [ Element.text <| "Players: " ++ String.fromInt (IdDict.size lobby.users)
        , button (JoinLobbyPressed lobbyId) (Element.text "Join")
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


subscriptions : AudioData -> FrontendModel_ -> Sub FrontendMsg_
subscriptions audioData model =
    Sub.batch
        [ martinsstewart_elm_device_pixel_ratio_from_js
            (Quantity.Quantity >> Quantity.per Pixels.pixel >> GotDevicePixelRatio)
        , Browser.Events.onResize
            (\width height -> WindowResized { width = Pixels.pixels width, height = Pixels.pixels height })
        , case model of
            Loading _ ->
                Sub.none

            Loaded _ ->
                Sub.batch
                    [ Sub.map KeyMsg Keyboard.subscriptions
                    , Browser.Events.onAnimationFrame AnimationFrame
                    ]
        ]
