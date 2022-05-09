port module Frontend exposing (app, init, update, updateFromBackend, view)

import Audio exposing (Audio, AudioCmd, AudioData)
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Dict
import Duration exposing (Duration)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Input
import Html exposing (Html)
import Id exposing (Id)
import IdDict exposing (IdDict)
import Json.Decode
import Json.Encode
import Keyboard
import Keyboard.Arrows
import Lamdera
import List.Extra as List
import List.Nonempty
import LocalModel exposing (Config, LocalModel)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity(..), Rate)
import Sounds exposing (Sounds)
import Task
import Time
import Timeline exposing (FrameId)
import Types exposing (..)
import UiColors
import Url exposing (Url)


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
                    }

                localModel =
                    LocalModel.localModel model.localModel

                move =
                    Keyboard.Arrows.wasdDirection model.pressedKeys
            in
            case (LocalModel.localModel newModel.localModel).match of
                Just matchState ->
                    if move == Keyboard.Arrows.wasdDirection model.previousKeys then
                        ( model, Cmd.none )

                    else
                        localChange (MatchInput (timeToFrameId time matchState) move) newModel

                Nothing ->
                    ( newModel, Cmd.none )

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
                                            , timeline = []
                                            , otherUsers = IdDict.keys lobby.users
                                            }
                                }
                            )
                        |> Maybe.withDefault model

                BroadcastChange (BroadcastStartMatch time lobbyId) ->
                    case IdDict.get lobbyId model.lobbies of
                        Just _ ->
                            { model | lobbies = IdDict.remove lobbyId model.lobbies }

                        Nothing ->
                            model

                SessionChange (MatchInput frameId input) ->
                    { model
                        | match =
                            Maybe.map
                                (\match ->
                                    { match
                                        | timeline =
                                            Timeline.addInput_
                                                frameId
                                                { userId = model.userId, input = input }
                                                match.timeline
                                    }
                                )
                                model.match
                    }

                BroadcastChange (BroadcastMatchInput frame timelineEvent) ->
                    { model
                        | match =
                            Maybe.map
                                (\match ->
                                    { match | timeline = Timeline.addInput_ frame timelineEvent match.timeline }
                                )
                                model.match
                    }
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
    let
        localModel =
            LocalModel.localModel model.localModel
    in
    Element.layout
        [ Element.clip
        , Element.behindContent <|
            Element.none
        ]
        (Element.column
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
