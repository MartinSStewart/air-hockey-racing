port module Frontend exposing (app, init, update, updateFromBackend, view)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Duration exposing (Duration)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Input
import Html exposing (Html)
import Html.Attributes
import Id exposing (Id)
import IdDict
import Keyboard
import Lamdera
import List.Extra as List
import List.Nonempty
import LocalModel exposing (Config)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity(..), Rate)
import Task
import Time
import Types exposing (..)
import UiColors
import Url exposing (Url)
import WebGL exposing (Shader)


port martinsstewart_elm_device_pixel_ratio_from_js : (Float -> msg) -> Sub msg


port martinsstewart_elm_device_pixel_ratio_to_js : () -> Cmd msg


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


loadedInit : FrontendLoading -> ClientInitData -> ( FrontendModel, Cmd FrontendMsg )
loadedInit loading initData =
    ( Loaded
        { key = loading.key
        , pressedKeys = []
        , windowSize = loading.windowSize
        , devicePixelRatio = loading.devicePixelRatio
        , time = loading.time
        , localModel = LocalModel.init initData
        }
    , Cmd.none
    )


tryLoadedInit : FrontendLoading -> ( FrontendModel, Cmd FrontendMsg )
tryLoadedInit loading =
    Maybe.map
        (loadedInit loading)
        loading.initData
        |> Maybe.withDefault ( Loading loading, Cmd.none )


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( Loading
        { key = key
        , windowSize = { width = Pixels.pixels 1920, height = Pixels.pixels 1080 }
        , devicePixelRatio = Quantity 1
        , time = Time.millisToPosix 0
        , initData = Nothing
        }
    , Task.perform
        (\{ viewport } ->
            WindowResized
                { width = round viewport.width |> Pixels.pixels
                , height = round viewport.height |> Pixels.pixels
                }
        )
        Browser.Dom.getViewport
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case model of
        Loading loadingModel ->
            case msg of
                WindowResized windowSize ->
                    windowResizedUpdate windowSize loadingModel |> Tuple.mapFirst Loading

                GotDevicePixelRatio devicePixelRatio ->
                    devicePixelRatioUpdate devicePixelRatio loadingModel |> Tuple.mapFirst Loading

                _ ->
                    ( model, Cmd.none )

        Loaded frontendLoaded ->
            updateLoaded msg frontendLoaded
                |> Tuple.mapFirst Loaded


updateLoaded : FrontendMsg -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
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
            ( { model | time = time }
            , Cmd.none
            )

        CreateLobbyPressed ->
            ( { model
                | localModel =
                    LocalModel.update
                        localModelConfig
                        model.time
                        (SessionChange CreateLobby)
                        model.localModel
              }
            , Lamdera.sendToBackend (SessionChange_ CreateLobby)
            )

        JoinLobbyPressed lobbyId ->
            Debug.todo ""


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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( model, msg ) of
        ( Loading loading, ClientInit initData ) ->
            { loading | initData = Just initData } |> tryLoadedInit

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded |> Tuple.mapFirst Loaded

        _ ->
            ( model, Cmd.none )


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Cmd FrontendMsg )
updateLoadedFromBackend msg model =
    case msg of
        Change change ->
            ( { model
                | localModel =
                    LocalModel.updateFromBackend localModelConfig (List.Nonempty.fromElement change) model.localModel
              }
            , Cmd.none
            )

        ClientInit _ ->
            ( model, Cmd.none )


lostConnection : FrontendLoaded -> Bool
lostConnection model =
    False



--case LocalModel.localMsgs model.localModel of
--    ( time, _ ) :: _ ->
--        Duration.from time model.time |> Quantity.greaterThan (Duration.seconds 10)
--
--    [] ->
--        False


view : FrontendModel -> Browser.Document FrontendMsg
view model =
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


loadedView : FrontendLoaded -> Html FrontendMsg
loadedView model =
    Element.layout
        [ Element.behindContent (Element.html (canvasView model))
        ]
        (Element.column
            [ Element.spacing 16 ]
            [ button CreateLobbyPressed (Element.text "Create lobby")
            , Element.column
                [ Element.spacing 8 ]
                [ Element.text "Lobbies"
                , LocalModel.localModel model.localModel
                    |> .lobbies
                    |> IdDict.toList
                    |> List.map lobbyRowView
                    |> Element.column []
                ]
            ]
        )


button : msg -> Element msg -> Element msg
button onPress label =
    Element.Input.button
        [ Element.Background.color <| Element.rgb 0.2 0.2 0.2 ]
        { onPress = Just onPress
        , label = label
        }


lobbyRowView : ( Id LobbyId, Lobby ) -> Element FrontendMsg
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


findPixelPerfectSize : FrontendLoaded -> { canvasSize : ( Int, Int ), actualCanvasSize : ( Int, Int ) }
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
    { canvasSize = ( w, h ), actualCanvasSize = ( actualW, actualH ) }


canvasView : FrontendLoaded -> Html FrontendMsg
canvasView model =
    let
        ( windowWidth, windowHeight ) =
            actualCanvasSize

        ( cssWindowWidth, cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize model

        { x, y } =
            { x = 0, y = 0 }

        zoomFactor =
            1

        viewMatrix =
            Mat4.makeScale3 (toFloat zoomFactor * 2 / toFloat windowWidth) (toFloat zoomFactor * -2 / toFloat windowHeight) 1
                |> Mat4.translate3
                    (negate <| toFloat <| round x)
                    (negate <| toFloat <| round y)
                    0
    in
    WebGL.toHtmlWith
        [ WebGL.alpha False, WebGL.antialias ]
        [ Html.Attributes.width windowWidth
        , Html.Attributes.height windowHeight
        , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
        , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
        ]
        []


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
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