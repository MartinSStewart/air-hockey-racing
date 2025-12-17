module Frontend exposing (app, app_)

import Audio exposing (Audio, AudioCmd, AudioData)
import Browser
import Browser.Navigation
import Duration
import EditorPage
import Effect.Browser.Dom as Dom
import Effect.Browser.Events
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Process
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task
import Effect.Time
import Html exposing (Html)
import Id exposing (Id)
import Json.Decode
import Keyboard
import Lamdera
import List.Extra as List
import Match exposing (LobbyPreview)
import MatchName
import MatchPage exposing (MatchId, WorldPixel)
import MyUi
import Pixels exposing (Pixels)
import Point2d
import Ports
import Quantity exposing (Quantity(..), Rate)
import Route exposing (Route(..))
import SeqDict
import Size exposing (Size)
import Sounds exposing (Sounds)
import Time
import Types exposing (..)
import Ui
import Ui.Anim
import Ui.Font
import Ui.Layout
import Ui.Prose
import Url exposing (Url)
import Url.Parser exposing ((<?>))
import Url.Parser.Query
import User exposing (UserId)


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
    Effect.Lamdera.frontend Lamdera.sendToBackend app_


app_ :
    { init : Url -> Effect.Browser.Navigation.Key -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , view : Audio.Model FrontendMsg_ FrontendModel_ -> Browser.Document (Audio.Msg FrontendMsg_)
    , update : Audio.Msg FrontendMsg_ -> Audio.Model FrontendMsg_ FrontendModel_ -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , updateFromBackend : ToFrontend -> Audio.Model FrontendMsg_ FrontendModel_ -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , subscriptions : Audio.Model FrontendMsg_ FrontendModel_ -> Subscription FrontendOnly (Audio.Msg FrontendMsg_)
    , onUrlRequest : Browser.UrlRequest -> Audio.Msg FrontendMsg_
    , onUrlChange : Url -> Audio.Msg FrontendMsg_
    }
app_ =
    Audio.lamderaFrontendWithAudio
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


audio : AudioData -> FrontendModel_ -> Audio
audio _ model =
    (case model of
        Loading _ ->
            Audio.silence

        Loaded loaded ->
            case loaded.page of
                MatchPage matchPage ->
                    MatchPage.audio loaded matchPage

                MainLobbyPage _ ->
                    Audio.silence

                EditorPage _ ->
                    Audio.silence
    )
        |> Audio.offsetBy (Duration.milliseconds 30)


loadedInit :
    FrontendLoading
    -> Time.Posix
    -> Sounds
    -> ( Id UserId, MainLobbyInitData )
    -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit loading time sounds ( userId, lobbyData ) =
    let
        model : FrontendLoaded
        model =
            { navigationKey = loading.navigationKey
            , currentKeys = []
            , previousKeys = []
            , currentMouse = { position = Point2d.origin, primaryDown = False, secondaryDown = False }
            , previousMouse = { position = Point2d.origin, primaryDown = False, secondaryDown = False }
            , windowSize = loading.windowSize
            , devicePixelRatio = loading.devicePixelRatio
            , time = time
            , debugTimeOffset = loading.debugTimeOffset
            , page = MainLobbyPage { lobbies = lobbyData.lobbies, joinLobbyError = Nothing }
            , sounds = sounds
            , userId = userId
            , pingStartTime = Nothing
            , pingData = Nothing
            , route = loading.route
            , loadMatchError = Nothing
            }
                |> (\a -> { a | pingStartTime = MatchPage.actualTime a |> Just })

        ( model2, cmd ) =
            routeChanged loading.route model
    in
    ( Loaded model2
    , Command.batch [ cmd, Effect.Lamdera.sendToBackend PingRequest ]
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
        { navigationKey = key
        , windowSize = { width = Pixels.pixels 1920, height = Pixels.pixels 1080 }
        , devicePixelRatio = Quantity 1
        , time = Nothing
        , initData = Nothing
        , sounds = SeqDict.empty
        , debugTimeOffset = offset
        , route = Route.decode url
        }
    , Command.batch
        [ Task.perform
            (\{ viewport } ->
                WindowResized
                    { width = round viewport.width |> Pixels.pixels
                    , height = round viewport.height |> Pixels.pixels
                    }
            )
            Dom.getViewport
        , Effect.Time.now |> Task.perform GotTime
        ]
    , Sounds.requestSounds SoundLoaded
    )


routeChanged : Route -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
routeChanged route model =
    case route of
        HomePageRoute ->
            ( model, Command.none )

        InMatchRoute matchId ->
            ( model
            , Command.batch
                [ MatchPage.MatchRequest matchId (Id.fromInt -1) Match.JoinMatchSetup
                    |> MatchPageToBackend
                    |> Effect.Lamdera.sendToBackend
                , Effect.Process.sleep (Duration.seconds 5) |> Task.perform (\() -> RejoinMatchTimedOut matchId)
                ]
            )


update : AudioData -> FrontendMsg_ -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
update _ msg model =
    case model of
        Loading loadingModel ->
            case msg of
                WindowResized windowSize ->
                    windowResizedUpdate windowSize loadingModel |> (\( a, b ) -> ( Loading a, b, Audio.cmdNone ))

                GotDevicePixelRatio devicePixelRatio ->
                    devicePixelRatioUpdate devicePixelRatio loadingModel
                        |> (\( a, b ) -> ( Loading a, b, Audio.cmdNone ))

                SoundLoaded url result ->
                    { loadingModel | sounds = SeqDict.insert url result loadingModel.sounds }
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
                    , Effect.Browser.Navigation.pushUrl model.navigationKey (Route.decode url |> Route.encode)
                    )

                Browser.External url ->
                    ( model
                    , Effect.Browser.Navigation.load url
                    )

        UrlChanged url ->
            let
                route =
                    Route.decode url

                ( model2, cmd ) =
                    routeChanged route model
            in
            ( { model2 | route = route }, cmd )

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
                    { model | time = time_, previousKeys = model.currentKeys, previousMouse = model.currentMouse }
            in
            case model2.page of
                MatchPage matchSetupPage ->
                    MatchPage.animationFrame { model | time = time_ } matchSetupPage
                        |> Tuple.mapBoth
                            (\a -> { model2 | page = MatchPage a })
                            (\cmd -> Command.map MatchPageToBackend MatchPageMsg cmd)

                MainLobbyPage _ ->
                    ( model2, Command.none )

                EditorPage editorPage ->
                    EditorPage.animationFrame { model | time = time_ } editorPage
                        |> Tuple.mapBoth
                            (\a -> { model2 | page = EditorPage a })
                            (\cmd -> Command.map EditorPageToBackend EditorPageMsg cmd)

        PressedCreateLobby ->
            ( model, Effect.Lamdera.sendToBackend CreateMatchRequest )

        PressedOpenLevelEditor ->
            ( case model.page of
                MainLobbyPage _ ->
                    { model | page = EditorPage EditorPage.init }

                _ ->
                    model
            , Command.none
            )

        SoundLoaded _ _ ->
            -- Shouldn't happen
            ( model, Command.none )

        GotTime _ ->
            ( model, Command.none )

        MatchPageMsg matchSetupMsg_ ->
            case model.page of
                MatchPage matchPage ->
                    let
                        ( newMatchPage, cmd ) =
                            MatchPage.update model matchSetupMsg_ matchPage
                    in
                    ( { model | page = MatchPage newMatchPage }
                    , Command.map MatchPageToBackend MatchPageMsg cmd
                    )

                _ ->
                    ( model, Command.none )

        EditorPageMsg editorPageMsg ->
            case model.page of
                EditorPage editorPage ->
                    let
                        ( newEditorPage, cmd ) =
                            EditorPage.update model editorPageMsg editorPage
                    in
                    ( { model | page = EditorPage newEditorPage }
                    , Command.map EditorPageToBackend EditorPageMsg cmd
                    )

                _ ->
                    ( model, Command.none )

        RejoinMatchTimedOut matchId ->
            case model.route of
                HomePageRoute ->
                    ( model, Command.none )

                InMatchRoute a ->
                    if a == matchId then
                        ( { model | loadMatchError = Just model.time }
                        , Effect.Browser.Navigation.replaceUrl model.navigationKey (Route.encode HomePageRoute)
                        )

                    else
                        ( model, Command.none )


windowResizedUpdate : Size -> { b | windowSize : Size } -> ( { b | windowSize : Size }, Command FrontendOnly toMsg FrontendMsg_ )
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
updateFromBackend _ msg model =
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
            case model.page of
                MainLobbyPage _ ->
                    let
                        ( match, cmd ) =
                            MatchPage.init lobbyId lobby Nothing
                    in
                    ( { model | page = MatchPage match }
                    , Command.batch
                        [ Command.map identity MatchPageMsg cmd
                        , Effect.Browser.Navigation.pushUrl model.navigationKey (Route.encode (InMatchRoute lobbyId))
                        ]
                    )

                _ ->
                    ( model, Command.none )

        JoinLobbyResponse lobbyId result ->
            case model.page of
                MainLobbyPage lobbyPage ->
                    case result of
                        JoinedLobby lobby ->
                            MatchPage.init lobbyId lobby Nothing
                                |> Tuple.mapBoth
                                    (\a -> { model | page = MatchPage a })
                                    (\cmd -> Command.map identity MatchPageMsg cmd)

                        JoinLobbyError error ->
                            ( { model | page = MainLobbyPage { lobbyPage | joinLobbyError = Just error } }
                            , Command.none
                            )

                        JoinedActiveMatch match frameId matchState ->
                            MatchPage.init lobbyId match (Just ( frameId, matchState ))
                                |> Tuple.mapBoth
                                    (\a -> { model | page = MatchPage a })
                                    (\cmd -> Command.map identity MatchPageMsg cmd)

                _ ->
                    ( model, Command.none )

        CreateLobbyBroadcast lobbyId lobbyPreview ->
            ( case model.page of
                MainLobbyPage lobbyData ->
                    { model
                        | page =
                            MainLobbyPage
                                { lobbyData | lobbies = SeqDict.insert lobbyId lobbyPreview lobbyData.lobbies }
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
                            Duration.addTo (MatchPage.actualTime model) Match.frameDuration

                        serverTime2 =
                            Match.unwrapServerTime serverTime

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
                                    { roundTripTime = Duration.from pingStartTime (MatchPage.actualTime model)
                                    , lowEstimate = newLowEstimate
                                    , highEstimate = newHighEstimate
                                    , serverTime = serverTime2
                                    , sendTime = pingStartTime
                                    , receiveTime = MatchPage.actualTime model
                                    , pingCount = pingCount
                                    }
                        , pingStartTime =
                            if keepPinging then
                                Just (MatchPage.actualTime model)

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

        RemoveLobbyBroadcast lobbyId ->
            ( case model.page of
                MainLobbyPage lobbyData ->
                    { model | page = MainLobbyPage { lobbyData | lobbies = SeqDict.remove lobbyId lobbyData.lobbies } }

                MatchPage _ ->
                    model

                EditorPage _ ->
                    model
            , Command.none
            )

        UpdateLobbyBroadcast lobbyId lobbyPreview ->
            ( case model.page of
                MainLobbyPage lobbyPage ->
                    { model
                        | page =
                            { lobbyPage | lobbies = SeqDict.update lobbyId (\_ -> Just lobbyPreview) lobbyPage.lobbies }
                                |> MainLobbyPage
                    }

                MatchPage _ ->
                    model

                EditorPage _ ->
                    model
            , Command.none
            )

        MatchPageToFrontend toFrontend ->
            case model.page of
                MatchPage matchPage ->
                    MatchPage.updateFromBackend toFrontend matchPage
                        |> Tuple.mapBoth
                            (\a -> { model | page = MatchPage a })
                            (Command.map MatchPageToBackend MatchPageMsg)

                _ ->
                    ( model, Command.none )

        RejoinMainLobby mainLobbyInitData ->
            ( { model | page = MainLobbyPage { lobbies = mainLobbyInitData.lobbies, joinLobbyError = Nothing } }
            , Command.none
            )

        EditorPageToFrontend toFrontend ->
            case model.page of
                EditorPage editorPage ->
                    EditorPage.updateFromBackend toFrontend editorPage
                        |> Tuple.mapBoth
                            (\a -> { model | page = EditorPage a })
                            (Command.map EditorPageToBackend EditorPageMsg)

                _ ->
                    ( model, Command.none )


view : AudioData -> FrontendModel_ -> Browser.Document FrontendMsg_
view _ model =
    { title = "Air Hockey Racing"
    , body =
        [ case model of
            Loading loading ->
                Ui.layout
                    [ Ui.width Ui.fill
                    , Ui.height Ui.fill
                    , Ui.padding 16
                    ]
                    (if SeqDict.values loading.sounds |> List.any isErr then
                        Ui.text "Loading failed"

                     else
                        Ui.text "Loading"
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
            MyUi.displayType model.windowSize
    in
    Ui.layout
        [ Ui.clip, Ui.Font.family [ Ui.Font.sansSerif ], Ui.height Ui.fill ]
        (case model.page of
            MatchPage matchSetup ->
                MatchPage.view model matchSetup |> Ui.map MatchPageMsg

            MainLobbyPage lobbyData ->
                case model.route of
                    InMatchRoute _ ->
                        Ui.text "Loading match..."

                    HomePageRoute ->
                        Ui.column
                            [ Ui.height Ui.fill
                            , Ui.spacing 16
                            , Ui.padding (MyUi.ifMobile displayType 8 16)
                            , case model.loadMatchError of
                                Just time ->
                                    if Duration.from time model.time |> Quantity.lessThan (Duration.seconds 5) then
                                        Ui.inFront
                                            (Ui.el
                                                [ Ui.centerX
                                                , Ui.width Ui.shrink
                                                , Ui.padding 16
                                                , Ui.background (Ui.rgb 255 255 255)
                                                , Ui.border 1
                                                , Ui.rounded 4
                                                , Ui.move { x = 0, y = 8, z = 0 }
                                                ]
                                                (Ui.text "Failed to load match!")
                                            )

                                    else
                                        Ui.noAttr

                                Nothing ->
                                    Ui.noAttr
                            ]
                            [ Ui.el [ Ui.width Ui.shrink, Ui.Font.bold ] (Ui.text "Air Hockey Racing")
                            , MyUi.simpleButton (Dom.id "createNewMatch") PressedCreateLobby (Ui.text "Create new match")
                            , MyUi.simpleButton (Dom.id "openLevelEditor") PressedOpenLevelEditor (Ui.text "Open level editor")
                            , Ui.column
                                [ Ui.height Ui.fill, Ui.spacing 8 ]
                                [ Ui.text "Or join existing match"
                                , case lobbyData.joinLobbyError of
                                    Nothing ->
                                        Ui.none

                                    Just MatchNotFound ->
                                        Ui.el
                                            [ Ui.width Ui.shrink, Ui.Font.color (Ui.rgb 255 0 0) ]
                                            (Ui.text "Lobby not found!")

                                    Just MatchFull ->
                                        Ui.el
                                            [ Ui.width Ui.shrink, Ui.Font.color (Ui.rgb 255 0 0) ]
                                            (Ui.text "Lobby is full!")
                                , if SeqDict.isEmpty lobbyData.lobbies then
                                    Ui.Prose.paragraph
                                        [ Ui.Font.center, Ui.centerY ]
                                        [ Ui.text "There are currently no existing matches" ]
                                        |> Ui.el
                                            [ Ui.widthMax 800
                                            , Ui.height Ui.fill
                                            , Ui.border 1
                                            ]

                                  else
                                    SeqDict.toList lobbyData.lobbies
                                        |> List.indexedMap (\index lobby -> lobbyRowView (modBy 2 index == 0) lobby)
                                        |> Ui.column
                                            [ Ui.widthMax 800
                                            , Ui.height Ui.fill
                                            , Ui.border 1
                                            ]
                                ]
                            ]

            EditorPage editorPageModel ->
                EditorPage.view model editorPageModel |> Ui.map EditorPageMsg
        )


lobbyRowView : Bool -> ( Id MatchId, LobbyPreview ) -> Ui.Element FrontendMsg_
lobbyRowView evenRow ( lobbyId, lobby ) =
    Ui.row
        [ Ui.background
            (if evenRow then
                Ui.rgb 255 255 255

             else
                Ui.rgb 242 242 242
            )
        , Ui.padding 4
        ]
        [ if lobby.name == MatchName.empty then
            Ui.el [ Ui.Font.italic ] (Ui.text "Unnamed match")

          else
            Ui.text (MatchName.toString lobby.name)
        , Ui.row
            [ Ui.width Ui.shrink, Ui.alignRight, Ui.spacing 8 ]
            [ Ui.text <| String.fromInt lobby.userCount ++ " / " ++ String.fromInt lobby.maxUserCount
            , Ui.el
                [ Ui.link (Route.encode (Route.InMatchRoute lobbyId))
                , Ui.background <| Ui.rgb 230 230 217
                , Ui.padding 4
                ]
                (Ui.text "Join")
            ]
        ]


subscriptions : AudioData -> FrontendModel_ -> Subscription FrontendOnly FrontendMsg_
subscriptions _ model =
    Subscription.batch
        [ Ports.devicePixelRatioResponse (Quantity.Quantity >> Quantity.per Pixels.pixel >> GotDevicePixelRatio)

        --, Effect.Time.every (Duration.milliseconds 100) RandomInput
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
