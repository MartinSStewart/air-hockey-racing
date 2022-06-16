module Frontend exposing (app)

import AssocList as Dict exposing (Dict)
import Audio exposing (Audio, AudioCmd, AudioData)
import Browser
import Browser.Navigation
import Duration exposing (Duration)
import Effect.Browser.Dom exposing (HtmlId)
import Effect.Browser.Events
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task as Task
import Effect.Time
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Html exposing (Html)
import Id exposing (Id)
import Keyboard exposing (Key)
import Lamdera
import List.Extra as List
import Match exposing (LobbyPreview, Match, MatchActive, MatchSetupMsg, MatchState, Place(..), Player, PlayerData, PlayerMode(..), ServerTime(..), TimelineEvent, WorldCoordinate)
import MatchName
import MatchPage exposing (MatchId, MatchLocalOnly(..), ScreenCoordinate, WorldPixel)
import NetworkModel exposing (NetworkModel)
import Pixels exposing (Pixels)
import Ports
import Quantity exposing (Quantity(..), Rate)
import Sounds exposing (Sounds)
import Time
import Types exposing (..)
import Ui exposing (WindowSize)
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
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        (Audio.lamderaFrontendWithAudio
            { init = init
            , onUrlRequest = UrlClicked
            , onUrlChange = \_ -> UrlChanged
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
    )
        |> Audio.offsetBy (Duration.milliseconds 30)


loadedInit :
    FrontendLoading
    -> Time.Posix
    -> Sounds
    -> ( Id UserId, MainLobbyInitData )
    -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit loading time sounds ( userId, lobbyData ) =
    ( { key = loading.key
      , currentKeys = []
      , previousKeys = []
      , windowSize = loading.windowSize
      , devicePixelRatio = loading.devicePixelRatio
      , time = time
      , debugTimeOffset = loading.debugTimeOffset
      , page = MainLobbyPage { lobbies = lobbyData.lobbies, joinLobbyError = Nothing }
      , sounds = sounds
      , userId = userId
      , pingStartTime = Nothing
      , pingData = Nothing
      }
        |> (\a -> { a | pingStartTime = MatchPage.actualTime a |> Just })
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

        UrlChanged ->
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
                    MatchPage.animationFrame MatchSetupRequest { model | time = time_ } matchSetupPage
                        |> Tuple.mapBoth
                            (\a -> { model2 | page = MatchPage a })
                            (\cmd -> Command.map identity MatchSetupMsg cmd)

                MainLobbyPage _ ->
                    ( model2, Command.none )

        PressedCreateLobby ->
            ( model, Effect.Lamdera.sendToBackend CreateMatchRequest )

        PressedJoinLobby lobbyId ->
            ( model
            , MatchSetupRequest lobbyId (Id.fromInt -1) Match.JoinMatchSetup |> Effect.Lamdera.sendToBackend
            )

        SoundLoaded _ _ ->
            -- Shouldn't happen
            ( model, Command.none )

        GotTime _ ->
            ( model, Command.none )

        MatchSetupMsg matchSetupMsg_ ->
            case model.page of
                MatchPage matchPage ->
                    let
                        ( newMatchPage, cmd ) =
                            MatchPage.update MatchSetupRequest model matchSetupMsg_ matchPage
                    in
                    ( { model | page = MatchPage newMatchPage }
                    , Command.map identity MatchSetupMsg cmd
                    )

                _ ->
                    ( model, Command.none )


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
                    MatchPage.init lobbyId lobby
                        |> Tuple.mapBoth
                            (\a -> { model | page = MatchPage a })
                            (\cmd -> Command.map identity MatchSetupMsg cmd)

                _ ->
                    ( model, Command.none )

        JoinLobbyResponse lobbyId result ->
            case model.page of
                MainLobbyPage lobbyPage ->
                    case result of
                        Ok lobby ->
                            MatchPage.init lobbyId lobby
                                |> Tuple.mapBoth
                                    (\a -> { model | page = MatchPage a })
                                    (\cmd -> Command.map identity MatchSetupMsg cmd)

                        Err error ->
                            ( { model | page = MainLobbyPage { lobbyPage | joinLobbyError = Just error } }
                            , Command.none
                            )

                _ ->
                    ( model, Command.none )

        CreateLobbyBroadcast lobbyId lobbyPreview ->
            ( case model.page of
                MainLobbyPage lobbyData ->
                    { model
                        | page =
                            MainLobbyPage
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

        MatchSetupBroadcast lobbyId userId matchSetupMsg ->
            case model.page of
                MatchPage matchSetup ->
                    let
                        updateHelper =
                            (if lobbyId == matchSetup.lobbyId then
                                let
                                    newNetworkModel : NetworkModel { userId : Id UserId, msg : MatchSetupMsg } Match
                                    newNetworkModel =
                                        NetworkModel.updateFromBackend
                                            Match.matchSetupUpdate
                                            Nothing
                                            { userId = userId, msg = matchSetupMsg }
                                            matchSetup.networkModel
                                in
                                { matchSetup
                                    | networkModel = newNetworkModel
                                    , matchData =
                                        MatchPage.updateMatchData
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
                        Match.SendTextMessage _ ->
                            ( { model | page = updateHelper }, Command.map identity MatchSetupMsg MatchPage.scrollToBottom )

                        _ ->
                            ( { model | page = updateHelper }, Command.none )

                MainLobbyPage _ ->
                    ( model, Command.none )

        RemoveLobbyBroadcast lobbyId ->
            ( case model.page of
                MainLobbyPage lobbyData ->
                    { model | page = MainLobbyPage { lobbyData | lobbies = Dict.remove lobbyId lobbyData.lobbies } }

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
                                    newNetworkModel : NetworkModel { userId : Id UserId, msg : MatchSetupMsg } Match
                                    newNetworkModel =
                                        NetworkModel.updateFromBackend
                                            Match.matchSetupUpdate
                                            (Just eventId)
                                            { userId = userId, msg = matchSetupMsg }
                                            matchSetup.networkModel
                                in
                                { matchSetup
                                    | networkModel = newNetworkModel
                                    , matchData =
                                        MatchPage.updateMatchData
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
                        ( Just lobbyData, Match.LeaveMatchSetup ) ->
                            ( { model
                                | page = MainLobbyPage { lobbies = lobbyData.lobbies, joinLobbyError = Nothing }
                              }
                            , Command.none
                            )

                        _ ->
                            ( { model | page = updateHelper }, Command.none )

                MainLobbyPage _ ->
                    ( model, Command.none )

        UpdateLobbyBroadcast lobbyId lobbyPreview ->
            ( case model.page of
                MainLobbyPage lobbyPage ->
                    { model
                        | page =
                            { lobbyPage | lobbies = Dict.update lobbyId (\_ -> Just lobbyPreview) lobbyPage.lobbies }
                                |> MainLobbyPage
                    }

                MatchPage _ ->
                    model
            , Command.none
            )


view : AudioData -> FrontendModel_ -> Browser.Document FrontendMsg_
view _ model =
    { title = "Air Hockey Racing"
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
        [ Element.clip ]
        (case model.page of
            MatchPage matchSetup ->
                MatchPage.view model matchSetup |> Element.map MatchSetupMsg

            MainLobbyPage lobbyData ->
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 16
                    , Element.padding (Ui.ifMobile displayType 8 16)
                    ]
                    [ Element.el [ Element.Font.bold ] (Element.text "Air Hockey Racing")
                    , Ui.simpleButton PressedCreateLobby (Element.text "Create new match")
                    , Element.column
                        [ Element.width Element.fill, Element.height Element.fill, Element.spacing 8 ]
                        [ Element.text "Or join existing match"
                        , case lobbyData.joinLobbyError of
                            Nothing ->
                                Element.none

                            Just LobbyNotFound ->
                                Element.el
                                    [ Element.Font.color (Element.rgb 1 0 0) ]
                                    (Element.text "Lobby not found!")

                            Just LobbyFull ->
                                Element.el
                                    [ Element.Font.color (Element.rgb 1 0 0) ]
                                    (Element.text "Lobby is full!")
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


lobbyRowView : Bool -> ( Id MatchId, LobbyPreview ) -> Element FrontendMsg_
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
            MatchPage.unnamedMatchText

          else
            Element.text (MatchName.toString lobby.name)
        , Element.row
            [ Element.alignRight, Element.spacing 8 ]
            [ Element.text <| String.fromInt lobby.userCount ++ " / " ++ String.fromInt lobby.maxUserCount
            , Ui.simpleButton (PressedJoinLobby lobbyId) (Element.text "Join")
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

            Loaded _ ->
                Subscription.batch
                    [ Subscription.map KeyMsg Keyboard.subscriptions
                    , Effect.Browser.Events.onAnimationFrame AnimationFrame
                    ]
        ]
