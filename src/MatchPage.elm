module MatchPage exposing
    ( MatchId
    , MatchLocalOnly(..)
    , Model
    , Mouse
    , Msg
    , ScreenCoordinate
    , ToBackend(..)
    , ToFrontend(..)
    , Vertex
    , WorldPixel
    , actualTime
    , animationFrame
    , audio
    , backgroundGrid
    , camera
    , canvasView
    , fragmentShader
    , init
    , lineMesh
    , screenToWorld
    , update
    , updateFromBackend
    , validateBotCount
    , vertexShader
    , view
    )

import Acceleration exposing (Acceleration, MetersPerSecondSquared)
import Angle exposing (Angle)
import Array
import Audio
import Axis2d
import Axis3d
import Camera3d exposing (Camera3d)
import ColorIndex exposing (ColorIndex(..))
import Decal exposing (Decal)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Direction3d
import Duration exposing (Duration)
import Ease exposing (Easing)
import Effect.Browser.Dom as Dom exposing (HtmlId)
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Task as Task
import Effect.Time as Time
import Effect.WebGL as WebGL exposing (Mesh, Shader)
import Env
import FontRender
import Geometry
import Geometry.Interop.LinearAlgebra.Point2d
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer
import Id exposing (Id)
import Json.Decode
import Keyboard exposing (Key)
import KeyboardExtra as Keyboard
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Match exposing (Action(..), Emote(..), Input, LobbyPreview, Match, MatchActive, MatchState, Particle, Place(..), Player, PlayerData, PlayerMode(..), ServerTime(..), Snowball, Team(..), TimelineEvent, WorldCoordinate)
import MatchName exposing (MatchName)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import MyUi
import NetworkModel exposing (EventId, NetworkModel)
import PingData exposing (PingData)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..), Rate)
import Random
import Random.Extra
import Random.List as Random
import RasterShapes
import Rectangle2d exposing (Rectangle2d)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Shape exposing (RenderableShape)
import Size exposing (Size)
import Sounds exposing (Sounds)
import Speed exposing (MetersPerSecond)
import TextMessage exposing (TextMessage)
import Timeline exposing (FrameId, TimelineCache, getOldestCachedState)
import TriangularMesh exposing (TriangularMesh)
import Ui
import Ui.Events
import Ui.Font
import Ui.Input
import Ui.Prose
import User exposing (UserId)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Matrices
import WebGL.Settings
import WebGL.Settings.DepthTest


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor ColorIndex
    | PressedSecondaryColor ColorIndex
    | PressedDecal (Maybe Decal)
    | TypedMatchName String
    | PressedPlayerMode PlayerMode
    | PressedSaveMatchName MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage TextMessage
    | TypedMaxPlayers String
    | PressedSaveMaxPlayers Int
    | PressedResetMaxPlayers
    | ScrolledToBottom
    | PointerDown Html.Events.Extra.Pointer.Event
    | PointerUp Html.Events.Extra.Pointer.Event
    | PointerLeave Html.Events.Extra.Pointer.Event
    | PointerMoved Html.Events.Extra.Pointer.Event
    | PressedLeaveMatch
    | TypedBotCount String


type MatchId
    = LobbyId Never


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Id MatchId
    , networkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
    , matchData : MatchLocalOnly
    }


init : Id MatchId -> Match -> Maybe ( Id FrameId, MatchState ) -> ( Model, Command FrontendOnly toMsg Msg )
init lobbyId lobby maybeCache =
    let
        networkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
        networkModel =
            NetworkModel.init lobby
    in
    ( { lobbyId = lobbyId
      , networkModel = networkModel
      , matchData =
            case ( Match.matchActive lobby, maybeCache ) of
                ( Just { startTime, timeline }, Just cache ) ->
                    initMatchData startTime (Match.allUsersAndBots lobby) (Just cache)
                        |> MatchActiveLocal

                ( Nothing, Nothing ) ->
                    initMatchSetupData lobby |> MatchSetupLocal

                _ ->
                    MatchError
      }
    , scrollToBottom
    )


type alias MatchSetupLocal_ =
    { matchName : String, message : String, maxPlayers : String, botCount : String }


type alias Vertex =
    { position : Vec3, color : Vec3 }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchActiveLocal_ =
    { timelineCache : Result Timeline.Error (TimelineCache MatchState)
    , userIds : SeqDict (Id UserId) (Mesh Vertex)
    , wallMesh : Mesh Vertex
    , touchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    , primaryDown : Maybe Time.Posix
    , previousPrimaryDown : Maybe Time.Posix
    , desyncedAtFrame : Maybe (Id FrameId)
    }


type ToBackend
    = MatchRequest (Id MatchId) (Id EventId) Match.Msg
    | DesyncCheckRequest (Id MatchId) (Id Timeline.FrameId) (SeqDict (Id UserId) (Point2d Meters WorldCoordinate))
    | CurrentCache (Id MatchId) (Id FrameId) MatchState


type ToFrontend
    = MatchSetupBroadcast (Id MatchId) (Id UserId) Match.Msg
    | MatchSetupResponse (Id MatchId) (Id UserId) Match.Msg (Id EventId)
    | DesyncBroadcast (Id MatchId) (Id FrameId)
    | NeedCurrentCacheBroadcast (Id MatchId) (Id FrameId)


update : Config a -> Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update config msg model =
    case msg of
        PressedStartMatchSetup ->
            matchSetupUpdate config.userId (Match.StartMatch (timeToServerTime config)) model

        PressedLeaveMatchSetup ->
            matchSetupUpdate config.userId Match.LeaveMatchSetup model

        PressedPrimaryColor colorIndex ->
            matchSetupUpdate config.userId (Match.SetPrimaryColor colorIndex) model

        PressedSecondaryColor colorIndex ->
            matchSetupUpdate config.userId (Match.SetSecondaryColor colorIndex) model

        PressedDecal decal ->
            matchSetupUpdate config.userId (Match.SetDecal decal) model

        PressedPlayerMode mode ->
            matchSetupUpdate config.userId (Match.SetPlayerMode mode) model

        TypedMatchName matchName ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData | matchName = matchName } |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PressedSaveMatchName matchName ->
            matchSetupUpdate config.userId (Match.SetMatchName matchName) model

        PressedResetMatchName ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData
                                | matchName =
                                    Match.name (getLocalState model)
                                        |> MatchName.toString
                            }
                                |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        TypedTextMessage text ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData | message = text } |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        SubmittedTextMessage message ->
            matchSetupUpdate
                config.userId
                (Match.SendTextMessage message)
                { model
                    | matchData =
                        case model.matchData of
                            MatchActiveLocal _ ->
                                model.matchData

                            MatchSetupLocal matchSetupData ->
                                { matchSetupData | message = "" } |> MatchSetupLocal

                            MatchError ->
                                MatchError
                }
                |> Tuple.mapSecond (\cmd -> Command.batch [ cmd, scrollToBottom ])

        TypedMaxPlayers maxPlayersText ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData | maxPlayers = maxPlayersText } |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PressedSaveMaxPlayers maxPlayers ->
            matchSetupUpdate config.userId (Match.SetMaxPlayers maxPlayers) model

        PressedResetMaxPlayers ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData
                                | maxPlayers =
                                    Match.maxPlayers (getLocalState model)
                                        |> String.fromInt
                            }
                                |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerDown event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData
                                    | touchPosition = Point2d.fromTuple Pixels.pixels event.pointer.clientPos |> Just
                                    , primaryDown = Just (actualTime config)
                                }
                                    |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerUp event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData
                                    | touchPosition = Point2d.fromTuple Pixels.pixels event.pointer.clientPos |> Just
                                    , primaryDown = Nothing
                                }
                                    |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerLeave event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData | touchPosition = Nothing } |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerMoved event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData
                                    | touchPosition = Point2d.fromTuple Pixels.pixels event.pointer.clientPos |> Just
                                }
                                    |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        ScrolledToBottom ->
            ( model, Command.none )

        PressedLeaveMatch ->
            matchSetupUpdate config.userId Match.LeaveMatchSetup model

        TypedBotCount text ->
            let
                model2 =
                    { model
                        | matchData =
                            case model.matchData of
                                MatchActiveLocal _ ->
                                    model.matchData

                                MatchSetupLocal matchSetupData ->
                                    { matchSetupData | botCount = text } |> MatchSetupLocal

                                MatchError ->
                                    MatchError
                    }
            in
            case validateBotCount text of
                Ok botCount ->
                    matchSetupUpdate config.userId (Match.SetBotCount botCount) model2

                Err _ ->
                    ( model2, Command.none )


validateBotCount : String -> Result String Int
validateBotCount text =
    case String.toInt text of
        Just int ->
            if int >= 0 then
                if int > 16 then
                    Err "Max 16 bots"

                else
                    Ok int

            else
                Err "Must be an positive integer"

        Nothing ->
            Err "Must be an positive integer"


matchSetupUpdate : Id UserId -> Match.Msg -> Model -> ( Model, Command FrontendOnly ToBackend msg )
matchSetupUpdate userId msg matchSetup =
    let
        { eventId, newNetworkModel } =
            NetworkModel.updateFromUser { userId = userId, msg = msg } matchSetup.networkModel
    in
    ( { matchSetup
        | networkModel = newNetworkModel
        , matchData =
            updateMatchData
                msg
                newNetworkModel
                matchSetup.networkModel
                matchSetup.matchData
      }
    , MatchRequest matchSetup.lobbyId eventId msg |> Effect.Lamdera.sendToBackend
    )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
updateFromBackend msg matchSetup =
    case msg of
        MatchSetupBroadcast lobbyId userId matchSetupMsg ->
            let
                updateHelper =
                    if lobbyId == matchSetup.lobbyId then
                        let
                            newNetworkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
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
                                updateMatchData
                                    matchSetupMsg
                                    newNetworkModel
                                    matchSetup.networkModel
                                    matchSetup.matchData
                        }

                    else
                        matchSetup
            in
            case matchSetupMsg of
                Match.SendTextMessage _ ->
                    ( updateHelper, scrollToBottom )

                _ ->
                    ( updateHelper, Command.none )

        MatchSetupResponse lobbyId userId matchSetupMsg eventId ->
            ( if lobbyId == matchSetup.lobbyId then
                let
                    newNetworkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
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
                        updateMatchData
                            matchSetupMsg
                            newNetworkModel
                            matchSetup.networkModel
                            matchSetup.matchData
                }

              else
                matchSetup
            , Command.none
            )

        DesyncBroadcast lobbyId frameId ->
            ( if lobbyId == matchSetup.lobbyId then
                { matchSetup
                    | matchData =
                        case matchSetup.matchData of
                            MatchActiveLocal matchData ->
                                { matchData | desyncedAtFrame = Just frameId }
                                    |> MatchActiveLocal

                            MatchSetupLocal _ ->
                                matchSetup.matchData

                            MatchError ->
                                MatchError
                }

              else
                matchSetup
            , Command.none
            )

        NeedCurrentCacheBroadcast matchId frameId ->
            let
                networkModel : Match
                networkModel =
                    NetworkModel.localState Match.matchSetupUpdate matchSetup.networkModel
            in
            case ( matchSetup.matchData, Match.matchActive networkModel ) of
                ( MatchActiveLocal matchData, Just matchActive ) ->
                    case matchData.timelineCache of
                        Ok timelineCache ->
                            case Timeline.getStateAt gameUpdate frameId timelineCache matchActive.timeline of
                                Ok ( _, ok ) ->
                                    ( matchSetup
                                    , Effect.Lamdera.sendToBackend (CurrentCache matchId frameId ok)
                                    )

                                Err _ ->
                                    ( matchSetup, Command.none )

                        Err error ->
                            ( matchSetup, Command.none )

                _ ->
                    ( matchSetup, Command.none )


type alias Config a =
    { a
        | windowSize : Size
        , userId : Id UserId
        , pingData : Maybe PingData
        , time : Time.Posix
        , debugTimeOffset : Duration
        , sounds : Sounds
        , previousKeys : List Key
        , currentKeys : List Key
        , previousMouse : Mouse
        , currentMouse : Mouse
        , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    }


type alias Mouse =
    { position : Point2d Pixels ScreenCoordinate
    , primaryDown : Bool
    , secondaryDown : Bool
    }


type WorldPixel
    = WorldPixel Never


view : Config a -> Model -> Ui.Element Msg
view config model =
    let
        lobby : Match
        lobby =
            getLocalState model
    in
    case ( Match.matchActive lobby, model.matchData, Match.allUsers_ lobby |> SeqDict.get config.userId ) of
        ( Just match, MatchActiveLocal matchData, _ ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId config match) cache match.timeline of
                        Ok ( _, matchState ) ->
                            Ui.el
                                (Ui.height Ui.fill
                                    :: Ui.htmlAttribute (Html.Events.Extra.Pointer.onDown PointerDown)
                                    :: Ui.htmlAttribute (Html.Events.Extra.Pointer.onUp PointerUp)
                                    :: Ui.htmlAttribute (Html.Events.Extra.Pointer.onLeave PointerLeave)
                                    :: Ui.id "canvas"
                                    :: Ui.inFront (desyncWarning matchData.desyncedAtFrame)
                                    :: Ui.inFront
                                        (Ui.el
                                            [ Ui.Events.onClick PressedLeaveMatch ]
                                            (Ui.el [ Ui.width Ui.shrink, Ui.background (Ui.rgb 65025 65025 65025) ] (Ui.text "Leave match"))
                                        )
                                    :: Ui.behindContent
                                        (canvasView
                                            config.windowSize
                                            config.devicePixelRatio
                                            (canvasViewHelper config model)
                                        )
                                    :: (case matchData.touchPosition of
                                            Just _ ->
                                                [ Ui.htmlAttribute (Html.Events.Extra.Pointer.onMove PointerMoved) ]

                                            Nothing ->
                                                []
                                       )
                                )
                                Ui.none

                        Err _ ->
                            Ui.text "An error occurred during the match :("

                Err _ ->
                    Ui.text "An error occurred during the match :("

        ( Nothing, MatchSetupLocal matchSetupData, Just currentPlayerData ) ->
            matchSetupView config lobby matchSetupData currentPlayerData

        _ ->
            Ui.text "Loading..."


matchSetupView : Config a -> Match -> MatchSetupLocal_ -> PlayerData -> Ui.Element Msg
matchSetupView config lobby matchSetupData currentPlayerData =
    let
        displayType =
            MyUi.displayType config.windowSize

        matchName : String
        matchName =
            MatchName.toString (Match.name lobby)

        users : List ( Id UserId, PlayerData )
        users =
            Match.allUsers lobby |> List.Nonempty.toList

        places : SeqDict (Id UserId) Int
        places =
            Match.previousMatchFinishTimes lobby
                |> Maybe.withDefault SeqDict.empty
                |> SeqDict.toList
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
                |> SeqDict.fromList

        preview =
            Match.preview lobby
    in
    Ui.column
        [ Ui.spacing 8
        , Ui.padding (MyUi.ifMobile displayType 8 16)
        , Ui.widthMax 800
        , Ui.height Ui.fill
        ]
        [ if Match.isOwner config.userId lobby then
            Ui.row
                [ Ui.spacing 8 ]
                (Ui.Input.text
                    [ Ui.padding 4
                    , if matchSetupData.matchName == "" then
                        Ui.Font.italic

                      else
                        Ui.noAttr
                    ]
                    { onChange = TypedMatchName
                    , text = matchSetupData.matchName
                    , placeholder = Just "Unnamed match"
                    , label = Ui.Input.labelHidden "Match name"
                    }
                    :: (if matchSetupData.matchName == matchName then
                            []

                        else
                            MyUi.simpleButton (Dom.id "resetMatchName") PressedResetMatchName (Ui.text "Reset")
                                :: (case MatchName.fromString matchSetupData.matchName of
                                        Ok matchName_ ->
                                            [ MyUi.simpleButton (Dom.id "saveMatchName") (PressedSaveMatchName matchName_) (Ui.text "Save") ]

                                        _ ->
                                            []
                                   )
                       )
                )

          else
            Ui.row [ Ui.width Ui.shrink, Ui.Font.bold ]
                [ Ui.text "Match: "
                , if matchName == "" then
                    Ui.el [ Ui.Font.italic ] (Ui.text "Unnamed match")

                  else
                    Ui.text matchName
                ]
        , if Match.isOwner config.userId lobby then
            let
                label =
                    Ui.Input.label "maxPlayers" [ Ui.width Ui.shrink ] (Ui.text "Max players")
            in
            Ui.row
                [ Ui.width Ui.shrink, Ui.spacing 8 ]
                (Ui.row
                    [ Ui.spacing 4 ]
                    [ label.element
                    , Ui.Input.text
                        [ Ui.width (Ui.px 50), Ui.padding 4, Ui.Font.alignRight ]
                        { onChange = TypedMaxPlayers
                        , text = matchSetupData.maxPlayers
                        , placeholder = Nothing
                        , label = label.id
                        }
                    ]
                    :: (if matchSetupData.maxPlayers == String.fromInt preview.maxUserCount then
                            []

                        else
                            MyUi.simpleButton (Dom.id "resetMaxPlayers") PressedResetMaxPlayers (Ui.text "Reset")
                                :: (case String.toInt matchSetupData.maxPlayers of
                                        Just maxPlayers ->
                                            [ MyUi.simpleButton (Dom.id "saveMaxPlayers") (PressedSaveMaxPlayers maxPlayers) (Ui.text "Save") ]

                                        Nothing ->
                                            []
                                   )
                       )
                )

          else
            Ui.none
        , if Match.isOwner config.userId lobby then
            let
                label =
                    Ui.Input.label "maxPlayers" [ Ui.width Ui.shrink ] (Ui.text "Number of bots")
            in
            Ui.row
                [ Ui.width Ui.shrink, Ui.spacing 8 ]
                [ Ui.row
                    [ Ui.spacing 4 ]
                    [ label.element
                    , Ui.Input.text
                        [ Ui.width (Ui.px 50), Ui.padding 4, Ui.Font.alignRight ]
                        { onChange = TypedBotCount
                        , text = matchSetupData.botCount
                        , placeholder = Nothing
                        , label = label.id
                        }
                    ]
                , case validateBotCount matchSetupData.botCount of
                    Ok _ ->
                        Ui.none

                    Err error ->
                        Ui.text error
                ]

          else
            Ui.none
        , Ui.row
            [ Ui.width Ui.shrink, Ui.spacing 8 ]
            [ if Match.isOwner config.userId lobby then
                MyUi.simpleButton (Dom.id "startMatchSetup") PressedStartMatchSetup (Ui.text "Start match")

              else
                Ui.none
            , MyUi.simpleButton (Dom.id "leaveMatchSetup") PressedLeaveMatchSetup (Ui.text "Leave")
            , case currentPlayerData.mode of
                PlayerMode ->
                    MyUi.simpleButton (Dom.id "switchToSpectator") (PressedPlayerMode SpectatorMode) (Ui.text "Switch to spectator")

                SpectatorMode ->
                    MyUi.simpleButton (Dom.id "switchToPlayer") (PressedPlayerMode PlayerMode) (Ui.text "Switch to player")
            ]
        , Ui.column
            [ Ui.width Ui.shrink, Ui.spacing 8 ]
            [ Ui.column
                [ Ui.width Ui.shrink
                , Ui.spacing 8
                , Ui.opacity
                    (case currentPlayerData.mode of
                        PlayerMode ->
                            1

                        SpectatorMode ->
                            0.5
                    )
                ]
                [ Ui.column
                    [ Ui.width Ui.shrink, Ui.spacing 4, Ui.Font.size 16, Ui.Font.bold ]
                    [ Ui.text "Primary color"
                    , colorSelector PressedPrimaryColor currentPlayerData.primaryColor
                    ]
                , Ui.column
                    [ Ui.width Ui.shrink, Ui.spacing 4, Ui.Font.size 16, Ui.Font.bold ]
                    [ Ui.text "Secondary color"
                    , colorSelector PressedSecondaryColor currentPlayerData.secondaryColor
                    ]
                , Ui.column
                    [ Ui.spacing 4 ]
                    [ Ui.el [ Ui.width Ui.shrink, Ui.Font.size 16, Ui.Font.bold ] (Ui.text "Decal")
                    , Nothing
                        :: List.map Just (List.Nonempty.toList Decal.allDecals)
                        |> List.map
                            (\maybeDecal ->
                                MyUi.button
                                    (decalHtmlId maybeDecal)
                                    [ Ui.paddingXY 4 4
                                    , Ui.background
                                        (if maybeDecal == currentPlayerData.decal then
                                            Ui.rgb 153 179 255

                                         else
                                            Ui.rgb 204 204 204
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
                                            |> Ui.text
                                    }
                            )
                        |> Ui.row [ Ui.spacing 8 ]
                    ]
                ]
            ]
        , Ui.column
            [ Ui.spacing 16, Ui.height Ui.fill ]
            [ Ui.column
                [ Ui.width Ui.shrink, Ui.spacing 8, Ui.alignTop, Ui.Font.size 16 ]
                [ Ui.text "Participants:"
                , Ui.column
                    [ Ui.width Ui.shrink ]
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
                                ++ (case SeqDict.get userId places of
                                        Just place ->
                                            " (" ++ placeToText place ++ ")"

                                        Nothing ->
                                            ""
                                   )
                                |> Ui.text
                        )
                        users
                        ++ (case Match.botCount lobby of
                                0 ->
                                    []

                                1 ->
                                    [ Ui.text "(and 1 bot)" ]

                                many ->
                                    [ Ui.text ("(and " ++ String.fromInt many ++ " bots)") ]
                           )
                    )
                ]
            , textChat matchSetupData lobby
            ]
        ]


decalHtmlId : Maybe Decal -> HtmlId
decalHtmlId maybeDecal =
    "selectDecal_"
        ++ (case maybeDecal of
                Just decal ->
                    case decal of
                        Decal.Star ->
                            "Star"

                        Decal.Triangle ->
                            "Triangle"

                        Decal.Plus ->
                            "Plus"

                        Decal.Minus ->
                            "Minus"

                        Decal.Square ->
                            "Square"

                        Decal.HollowSquare ->
                            "HollowSquare"

                Nothing ->
                    "noDecal"
           )
        |> Dom.id


textChat : MatchSetupLocal_ -> Match -> Ui.Element Msg
textChat matchSetupData lobby =
    Ui.column
        [ Ui.scrollable
        , Ui.height Ui.fill
        , Ui.padding 4
        ]
        [ Match.messagesOldestToNewest lobby
            |> List.map
                (\{ userId, message } ->
                    let
                        userName : String
                        userName =
                            Id.toInt userId |> String.fromInt |> (++) "User "
                    in
                    Ui.row
                        [ Ui.width Ui.shrink, Ui.Font.size 16 ]
                        [ (if Match.isOwner userId lobby then
                            userName ++ " (host)" ++ " "

                           else
                            userName ++ " "
                          )
                            |> Ui.text
                            |> Ui.el [ Ui.width Ui.shrink, Ui.Font.bold, Ui.alignTop ]
                        , TextMessage.toString message |> Ui.text |> List.singleton |> Ui.Prose.paragraph [ Ui.width Ui.shrink ]
                        ]
                )
            |> Ui.column
                [ Ui.spacing 4
                , Ui.scrollable
                , Ui.height Ui.fill
                , Ui.paddingXY 0 8
                , Ui.htmlAttribute (Dom.idToAttribute textMessageContainerId)
                ]
        , Ui.Input.text
            (Ui.Font.size 16
                :: Ui.padding 8
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
                                |> Ui.htmlAttribute
                            ]

                        Err _ ->
                            []
                   )
            )
            { onChange = TypedTextMessage
            , text = matchSetupData.message
            , placeholder = Just "Press enter to send"
            , label = Ui.Input.labelHidden "Write message"
            }
        ]


findPixelPerfectSize :
    Size
    -> Quantity Float (Rate WorldPixel Pixels)
    -> { canvasSize : ( Quantity Int Pixels, Quantity Int Pixels ), actualCanvasSize : Size }
findPixelPerfectSize windowSize (Quantity pixelRatio) =
    let
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
            findValue windowSize.width

        ( h, actualH ) =
            findValue windowSize.height
    in
    { canvasSize = ( Pixels.pixels w, Pixels.pixels h )
    , actualCanvasSize = { width = Pixels.pixels actualW, height = Pixels.pixels actualH }
    }


canvasView : Size -> Quantity Float (Rate WorldPixel Pixels) -> (Size -> List WebGL.Entity) -> Ui.Element msg
canvasView windowSize devicePixelRatio entities =
    let
        ( cssWindowWidth, cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize windowSize devicePixelRatio
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True, WebGL.stencil 0, WebGL.depth 1 ]
        [ Html.Attributes.width (Pixels.inPixels actualCanvasSize.width)
        , Html.Attributes.height (Pixels.inPixels actualCanvasSize.height)
        , Html.Attributes.style "width" (String.fromInt (Pixels.inPixels cssWindowWidth) ++ "px")
        , Html.Attributes.style "height" (String.fromInt (Pixels.inPixels cssWindowHeight) ++ "px")
        ]
        (entities actualCanvasSize)
        |> Ui.html


camera : Point2d Meters WorldCoordinate -> Length -> Camera3d Meters WorldCoordinate
camera position viewportHeight2 =
    let
        { x, y } =
            Point2d.toMeters position
    in
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.fromMeters { x = x, y = y, z = 0 }
                , eyePoint = Point3d.fromMeters { x = x, y = y, z = 20 }
                , upDirection = Direction3d.y
                }
        , viewportHeight = viewportHeight2
        }


screenToWorld : Size -> Point2d Meters WorldCoordinate -> Length -> Point2d Pixels ScreenCoordinate -> Point2d Meters WorldCoordinate
screenToWorld windowSize cameraPosition viewportHeight2 screenPosition =
    let
        screenRectangle : Rectangle2d Pixels ScreenCoordinate
        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity windowSize.height))
                (Point2d.xy (Quantity.toFloatQuantity windowSize.width) Quantity.zero)
    in
    Camera3d.ray (camera cameraPosition viewportHeight2) screenRectangle screenPosition
        |> Axis3d.originPoint
        |> (\p -> Point3d.toMeters p |> (\a -> Point2d.meters a.x a.y))


backgroundGrid : Point2d units coordinates -> Float -> Size -> WebGL.Entity
backgroundGrid cameraPosition zoom canvasSize =
    let
        { width, height } =
            canvasSize

        canvasWidth =
            Pixels.inPixels width |> toFloat

        canvasHeight =
            Pixels.inPixels height |> toFloat
    in
    WebGL.entityWith
        [ WebGL.Settings.cullFace WebGL.Settings.back ]
        backgroundVertexShader
        backgroundFragmentShader
        squareMesh
        { view = Geometry.Interop.LinearAlgebra.Point2d.toVec2 cameraPosition
        , viewZoom = canvasHeight * zoom
        , windowSize = Math.Vector2.vec2 canvasWidth canvasHeight
        }


countdown : Array.Array RenderableShape
countdown =
    Array.fromList [ Shape.three, Shape.two, Shape.one, Shape.go ]


drawCountdown : Id FrameId -> Mat4 -> List WebGL.Entity
drawCountdown frameId viewMatrix =
    let
        gameTimeElapsed : Duration
        gameTimeElapsed =
            frameTimeElapsed (Id.fromInt 0) frameId

        elapsedSeconds : Int
        elapsedSeconds =
            Duration.inSeconds gameTimeElapsed |> floor
    in
    case Array.get (elapsedSeconds - 1) countdown of
        Just value ->
            drawShape
                (toFromAndBack
                    (Duration.milliseconds 100)
                    (Duration.seconds 800)
                    (Duration.milliseconds 100)
                    (gameTimeElapsed |> Quantity.minus (Duration.seconds (toFloat elapsedSeconds)))
                    Ease.outBack
                    Ease.inBack
                    0
                    0.01
                )
                Point2d.origin
                viewMatrix
                value

        Nothing ->
            []


drawParticles : Id FrameId -> Mat4 -> List Particle -> List WebGL.Entity
drawParticles frameId viewMatrix particles =
    List.map
        (\particle ->
            let
                timeElapsed =
                    frameTimeElapsed particle.spawnedAt frameId

                { x, y } =
                    particle.position
                        |> Point2d.translateBy (Vector2d.for timeElapsed particle.velocity)
                        |> Point2d.toMeters

                t : Float
                t =
                    Quantity.ratio
                        timeElapsed
                        particle.lifetime

                size : Float
                size =
                    Length.inMeters particle.size * (1 - t)
            in
            WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                vertexShader
                fragmentShader
                particleOutlineMesh
                { ucolor = Vec3.vec3 1 1 1
                , view = viewMatrix
                , model =
                    Mat4.makeTranslate3 x y 1 |> Mat4.scale3 size size size
                }
        )
        particles
        ++ List.map
            (\particle ->
                let
                    timeElapsed =
                        frameTimeElapsed particle.spawnedAt frameId

                    { x, y } =
                        particle.position
                            |> Point2d.translateBy (Vector2d.for timeElapsed particle.velocity)
                            |> Point2d.toMeters

                    t : Float
                    t =
                        Quantity.ratio
                            timeElapsed
                            particle.lifetime

                    size : Float
                    size =
                        Length.inMeters particle.size * (1 - t)
                in
                WebGL.entityWith
                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                    vertexShader
                    fragmentShader
                    particleMesh
                    { ucolor = Vec3.vec3 1 1 1
                    , view = viewMatrix
                    , model =
                        Mat4.makeTranslate3 x y 1 |> Mat4.scale3 size size size
                    }
            )
            particles


canvasViewHelper : Config a -> Model -> Size -> List WebGL.Entity
canvasViewHelper model matchSetup canvasSize =
    case ( Match.matchActive (getLocalState matchSetup), matchSetup.matchData ) of
        ( Just match, MatchActiveLocal matchData ) ->
            case matchData.timelineCache of
                Ok cache ->
                    let
                        frameId =
                            timeToFrameId model match
                    in
                    case Timeline.getStateAt gameUpdate frameId cache match.timeline of
                        Ok ( _, state ) ->
                            let
                                zoom : Float
                                zoom =
                                    1 / Length.inMeters viewportHeight

                                --zoomFactor
                                --    * toFloat (max canvasWidth canvasHeight)
                                --    / (toFloat canvasHeight * 2000)
                                viewMatrix : Mat4
                                viewMatrix =
                                    WebGL.Matrices.viewProjectionMatrix
                                        (camera Point2d.origin (Length.meters (1 / zoom)))
                                        { nearClipDepth = Length.meters 0.1
                                        , farClipDepth = Length.meters 30
                                        , aspectRatio =
                                            Quantity.ratio
                                                (Quantity.toFloatQuantity canvasSize.width)
                                                (Quantity.toFloatQuantity canvasSize.height)
                                        }
                            in
                            drawCountdown frameId viewMatrix
                                ++ [ WebGL.entityWith
                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                        vertexShader
                                        fragmentShader
                                        matchData.wallMesh
                                        { ucolor = Vec3.vec3 1 1 1
                                        , view = viewMatrix
                                        , model = Mat4.identity
                                        }
                                   ]
                                ++ List.concatMap
                                    (\( userId, player ) ->
                                        drawPlayer
                                            (timeToFrameId model match)
                                            userId
                                            matchData
                                            viewMatrix
                                            player
                                    )
                                    (SeqDict.toList state.players)
                                ++ List.concatMap
                                    (\snowball ->
                                        let
                                            { x, y, z } =
                                                Point3d.toMeters snowball.position

                                            snowballRadius_ =
                                                Length.inMeters snowballRadius
                                        in
                                        [ WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                            vertexShader
                                            fragmentShader
                                            snowballShadowMesh
                                            { ucolor = Vec3.vec3 1 1 1
                                            , view = viewMatrix
                                            , model =
                                                Mat4.makeTranslate3 x y -0.1
                                                    |> Mat4.scale3 snowballRadius_ snowballRadius_ snowballRadius_
                                            }
                                        , WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                            vertexShader
                                            fragmentShader
                                            snowballMesh
                                            { ucolor = Vec3.vec3 1 1 1
                                            , view = viewMatrix
                                            , model =
                                                Mat4.makeTranslate3 x (y + z) z
                                                    |> Mat4.scale3 snowballRadius_ snowballRadius_ snowballRadius_
                                            }
                                        ]
                                    )
                                    state.snowballs
                                ++ drawParticles frameId viewMatrix state.particles
                                ++ (case SeqDict.get model.userId state.players of
                                        Just player ->
                                            case player.clickStart of
                                                Just clickStart ->
                                                    let
                                                        currentFrameId =
                                                            timeToFrameId model match

                                                        elapsed =
                                                            frameTimeElapsed clickStart.time currentFrameId
                                                    in
                                                    if elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay then
                                                        case Direction2d.from player.position clickStart.position of
                                                            Just direction ->
                                                                let
                                                                    targetPosition =
                                                                        Point2d.translateBy
                                                                            (Vector2d.withLength (throwDistance elapsed) direction)
                                                                            player.position

                                                                    reticleScale =
                                                                        0.2 + 0.15 * throwCharge elapsed
                                                                in
                                                                [ WebGL.entityWith
                                                                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                                                    vertexShader
                                                                    fragmentShader
                                                                    aimingReticle
                                                                    { ucolor = Vec3.vec3 1 1 1
                                                                    , view = viewMatrix
                                                                    , model =
                                                                        point2ToMatrix targetPosition
                                                                            |> Mat4.scale3 reticleScale reticleScale reticleScale
                                                                    }
                                                                ]

                                                            Nothing ->
                                                                []

                                                    else
                                                        []

                                                Nothing ->
                                                    []

                                        Nothing ->
                                            []
                                   )
                                ++ (case SeqDict.get model.userId state.players of
                                        Just player ->
                                            case ( player.finishTime, player.targetPosition ) of
                                                ( DidNotFinish, Just targetPos ) ->
                                                    [ WebGL.entityWith
                                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                                        vertexShader
                                                        fragmentShader
                                                        moveArrow
                                                        { ucolor = Vec3.vec3 1 1 1
                                                        , view = viewMatrix
                                                        , model =
                                                            point2ToMatrix targetPos
                                                                |> Mat4.scale3 0.3 0.3 0.3
                                                        }
                                                    ]

                                                _ ->
                                                    []

                                        _ ->
                                            []
                                   )

                        Err _ ->
                            []

                Err _ ->
                    []

        _ ->
            []


handPosition : Bool -> Id FrameId -> Player -> Point3d Meters WorldCoordinate
handPosition leftHand frameId player =
    let
        speed : Float
        speed =
            Vector2d.length player.velocity |> Quantity.unwrap

        swingPhase : Float
        swingPhase =
            if leftHand then
                0

            else
                pi

        timeElapsed : Duration
        timeElapsed =
            frameTimeElapsed (Id.fromInt 0) frameId

        isMoving : Bool
        isMoving =
            speed > 0.01

        swingAmount : Length
        swingAmount =
            if isMoving then
                sin (Duration.inSeconds timeElapsed * 10 + swingPhase) * 0.15 |> Length.meters

            else
                Quantity.zero

        -- Check if right hand is charging a throw
        chargeOffset : Length
        chargeOffset =
            if not leftHand then
                case player.clickStart of
                    Just clickStart ->
                        let
                            elapsed =
                                frameTimeElapsed clickStart.time frameId
                        in
                        if elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay then
                            -- Move backwards based on charge amount
                            Length.meters (-0.3 * throwCharge elapsed)

                        else
                            Quantity.zero

                    Nothing ->
                        Quantity.zero

            else
                Quantity.zero

        upOffset : Length
        upOffset =
            if leftHand then
                Length.meters 0.3

            else
                case player.clickStart of
                    Just clickStart ->
                        let
                            elapsed =
                                frameTimeElapsed clickStart.time frameId
                        in
                        if elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay then
                            0.3 * (Length.inMeters snowballStartHeight - 0.3) * throwCharge elapsed |> Length.meters

                        else
                            Length.meters 0.3

                    Nothing ->
                        Length.meters 0.3
    in
    Point2d.translateIn
        (if leftHand then
            Direction2d.rotateCounterclockwise player.rotation

         else
            Direction2d.rotateClockwise player.rotation
        )
        (Length.meters 0.5)
        player.position
        |> Point2d.translateIn player.rotation (Quantity.plus swingAmount chargeOffset)
        |> point2To3 upOffset


drawHand : Bool -> Id FrameId -> Player -> Mat4 -> Mat4 -> WebGL.Entity
drawHand leftHand frameId player viewMatrix modelMatrix =
    WebGL.entityWith
        [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
        vertexShader
        fragmentShader
        playerHand
        { ucolor =
            case player.team of
                BlueTeam ->
                    Vec3.vec3 0 0 0.8

                RedTeam ->
                    Vec3.vec3 0.8 0 0
        , view = viewMatrix
        , model = point3ToMatrix (handPosition leftHand frameId player) |> matMul modelMatrix
        }


matMul : Mat4 -> Mat4 -> Mat4
matMul a b =
    Mat4.mul b a


drawPlayer : Id FrameId -> Id UserId -> MatchActiveLocal_ -> Mat4 -> Player -> List WebGL.Entity
drawPlayer frameId userId matchData viewMatrix player =
    case SeqDict.get userId matchData.userIds of
        Just mesh ->
            let
                rotation : Float
                rotation =
                    Direction2d.toAngle player.rotation |> Angle.inRadians

                playerRadius_ : Float
                playerRadius_ =
                    Length.inMeters playerRadius

                modelMatrix : Mat4
                modelMatrix =
                    Mat4.makeScale3 playerRadius_ playerRadius_ playerRadius_
                        |> Mat4.rotate -0.4 (Vec3.vec3 1 0 0)
                        |> Mat4.rotate rotation (Vec3.vec3 0 0 1)
                        |> (\mat ->
                                case player.isDead of
                                    Just death ->
                                        let
                                            framesSinceDeath : Duration
                                            framesSinceDeath =
                                                frameTimeElapsed death.time frameId

                                            fallProgress : Float
                                            fallProgress =
                                                Quantity.ratio framesSinceDeath (Duration.seconds 0.5) |> min 1

                                            ( dx, dy ) =
                                                Direction2d.rotateClockwise death.fallDirection |> Direction2d.components
                                        in
                                        Mat4.rotate (fallProgress * (pi / 2)) (Vec3.vec3 dx dy 0) mat

                                    Nothing ->
                                        mat
                           )
            in
            [ WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerHead
                { ucolor = Vec3.vec3 1 0.8 0.5
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            , drawHand True frameId player viewMatrix modelMatrix
            , drawHand False frameId player viewMatrix modelMatrix
            , WebGL.entityWith
                [ WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                (case player.isDead of
                    Just _ ->
                        deadPlayerEyes

                    Nothing ->
                        playerEyes
                )
                { ucolor = Vec3.vec3 1 1 1
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            , WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerBody
                { ucolor =
                    case player.team of
                        BlueTeam ->
                            Vec3.vec3 0 0 1

                        RedTeam ->
                            Vec3.vec3 1 0 0
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            ]
                ++ (case player.lastEmote of
                        Just lastEmote ->
                            let
                                timeElapsed : Duration
                                timeElapsed =
                                    Quantity.multiplyBy
                                        (Id.toInt frameId - Id.toInt lastEmote.time |> toFloat)
                                        Match.frameDuration

                                emojiSize : Float
                                emojiSize =
                                    toFromAndBack
                                        (Duration.milliseconds 200)
                                        (Duration.seconds 2)
                                        (Duration.milliseconds 200)
                                        timeElapsed
                                        Ease.outBack
                                        Ease.inBack
                                        0
                                        0.002
                            in
                            drawShape
                                emojiSize
                                (Point2d.translateBy (Vector2d.meters 0.4 0.3) player.position)
                                viewMatrix
                                (case lastEmote.emote of
                                    SurpriseEmote ->
                                        Shape.surprise

                                    ImpEmote ->
                                        Shape.imp
                                )

                        Nothing ->
                            []
                   )

        Nothing ->
            []


drawShape : Float -> Point2d units coordinates -> Mat4 -> RenderableShape -> List WebGL.Entity
drawShape scale position viewMatrix shape =
    List.concatMap
        (\layer ->
            FontRender.drawLayer
                layer.color
                layer.mesh
                (point2ToMatrix position |> Mat4.scale3 scale scale 0.01)
                viewMatrix
        )
        shape.layers


toFrom : Duration -> Duration -> Easing -> Float -> Float -> Float
toFrom duration timeElapsed easingFunction startValue endValue =
    let
        t =
            Quantity.ratio timeElapsed duration |> clamp 0 1
    in
    easingFunction t * (endValue - startValue) + startValue


toFromAndBack : Duration -> Duration -> Duration -> Duration -> Easing -> Easing -> Float -> Float -> Float
toFromAndBack startDuration holdDuration endDuration timeElapsed easingIn easingOut startValue endValue =
    if timeElapsed |> Quantity.lessThan startDuration then
        toFrom startDuration timeElapsed easingIn startValue endValue

    else if timeElapsed |> Quantity.lessThan (Quantity.plus startDuration holdDuration) then
        endValue

    else
        toFrom
            endDuration
            (timeElapsed |> Quantity.minus (Quantity.plus startDuration holdDuration))
            easingOut
            endValue
            startValue


wall : Polygon2d Meters WorldCoordinate
wall =
    Polygon2d.withHoles
        []
        [ Point2d.meters -14 -12
        , Point2d.meters 14 -12
        , Point2d.meters 14 12
        , Point2d.meters -14 12
        ]


playerStart : Point2d Meters WorldCoordinate
playerStart =
    Point2d.fromMeters { x = 0, y = 0 }


wallSegments : List (LineSegment2d Meters WorldCoordinate)
wallSegments =
    Geometry.pointsToLineSegments (Polygon2d.outerLoop wall)
        ++ List.concatMap Geometry.pointsToLineSegments (Polygon2d.innerLoops wall)


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


wallLookUp : Dict ( Int, Int ) (SeqSet (LineSegment2d Meters WorldCoordinate))
wallLookUp =
    List.foldl
        (\segment dict ->
            let
                ( start, end ) =
                    LineSegment2d.endpoints segment

                addPoint x y =
                    Dict.update ( x, y ) (Maybe.withDefault SeqSet.empty >> SeqSet.insert segment >> Just)
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
        Dict.empty
        wallSegments


getCollisionCandidates : Point2d Meters coordinates -> SeqSet (LineSegment2d Meters WorldCoordinate)
getCollisionCandidates point =
    Dict.get (pointToGrid point |> (\{ x, y } -> ( x, y ))) wallLookUp |> Maybe.withDefault SeqSet.empty


lineSegmentMesh : Vec3 -> List (LineSegment2d Meters WorldCoordinate) -> Mesh Vertex
lineSegmentMesh color lines =
    List.concatMap (lineMesh (Length.meters 0.1) color) lines |> WebGL.triangles


lineMesh : Quantity Float Meters -> Vec3 -> LineSegment2d Meters WorldCoordinate -> List ( Vertex, Vertex, Vertex )
lineMesh thickness color line =
    let
        ( p0, p1 ) =
            LineSegment2d.endpoints line

        perpendicular : Vector2d units coordinates
        perpendicular =
            Vector2d.from p0 p1
                |> Vector2d.perpendicularTo
                |> Vector2d.normalize
                |> Vector2d.scaleBy (Length.inMeters thickness)
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


pointToVec : Point2d units coordinate -> Vec3
pointToVec point2d =
    let
        { x, y } =
            Point2d.unwrap point2d
    in
    Vec3.vec3 x y 0


clickMoveMaxDelay : Duration
clickMoveMaxDelay =
    Duration.seconds 0.2


chargeMaxDelay : Duration
chargeMaxDelay =
    Duration.seconds 1


clickTotalDelay : Duration
clickTotalDelay =
    Quantity.plus chargeMaxDelay clickMoveMaxDelay


snowballPlayerCollision : Snowball -> Id UserId -> Player -> Bool
snowballPlayerCollision snowball userId player =
    let
        { x, y, z } =
            Point3d.toMeters snowball.position
    in
    (userId /= snowball.thrownBy)
        && (Length.meters z |> Quantity.lessThan playerHeight)
        && (player.isDead == Nothing)
        && (Point2d.distanceFrom (Point2d.meters x y) player.position
                |> Quantity.lessThan (Quantity.plus playerRadius snowballRadius)
           )


playerHeight : Length
playerHeight =
    Length.meters 1.5


gameUpdate : Id FrameId -> List TimelineEvent -> MatchState -> MatchState
gameUpdate frameId inputs model =
    let
        inputs2 : SeqDict (Id UserId) Input
        inputs2 =
            List.map (\input -> ( input.userId, input.input )) inputs |> SeqDict.fromList

        model3 : MatchState
        model3 =
            SeqDict.foldl
                (\userId player model2 ->
                    let
                        input : Input
                        input =
                            if Id.toInt userId < 0 then
                                getBotInput frameId model2 userId player

                            else
                                SeqDict.get userId inputs2 |> Maybe.withDefault noInput

                        ( hitBySnowball, snowballs ) =
                            List.foldl
                                (\snowball ( hitSnowball, snowballs2 ) ->
                                    if hitSnowball == Nothing && snowballPlayerCollision snowball userId player then
                                        ( Just snowball, snowballs2 )

                                    else
                                        ( hitSnowball, snowball :: snowballs2 )
                                )
                                ( Nothing, [] )
                                model2.snowballs
                                |> Tuple.mapSecond List.reverse
                    in
                    { model2
                        | players =
                            SeqDict.insert userId
                                { player
                                    | targetPosition =
                                        case player.isDead of
                                            Just _ ->
                                                Nothing

                                            Nothing ->
                                                case ( player.clickStart, input.action ) of
                                                    ( Just clickStart, ClickRelease point ) ->
                                                        if
                                                            frameTimeElapsed clickStart.time frameId
                                                                |> Quantity.lessThan clickMoveMaxDelay
                                                        then
                                                            Just point

                                                        else
                                                            player.targetPosition

                                                    ( Just clickStart, _ ) ->
                                                        if
                                                            frameTimeElapsed clickStart.time frameId
                                                                |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay
                                                        then
                                                            Nothing

                                                        else
                                                            player.targetPosition

                                                    _ ->
                                                        player.targetPosition
                                    , rotation =
                                        case player.targetPosition of
                                            Just targetPosition ->
                                                case Direction2d.from player.position targetPosition of
                                                    Just direction ->
                                                        let
                                                            angleDifference : Angle
                                                            angleDifference =
                                                                Direction2d.angleFrom player.rotation direction
                                                        in
                                                        if Quantity.abs angleDifference |> Quantity.lessThan (Angle.degrees 5) then
                                                            direction

                                                        else
                                                            Direction2d.rotateBy
                                                                (Angle.degrees (5 * Quantity.sign angleDifference))
                                                                player.rotation

                                                    Nothing ->
                                                        player.rotation

                                            Nothing ->
                                                case player.clickStart of
                                                    Just clickStart ->
                                                        case Direction2d.from player.position clickStart.position of
                                                            Just direction ->
                                                                let
                                                                    angleDifference : Angle
                                                                    angleDifference =
                                                                        Direction2d.angleFrom player.rotation direction
                                                                in
                                                                if Quantity.abs angleDifference |> Quantity.lessThan (Angle.degrees 5) then
                                                                    direction

                                                                else
                                                                    Direction2d.rotateBy
                                                                        (Angle.degrees (5 * Quantity.sign angleDifference))
                                                                        player.rotation

                                                            Nothing ->
                                                                player.rotation

                                                    Nothing ->
                                                        player.rotation
                                    , lastEmote =
                                        case input.emote of
                                            Just emote ->
                                                Just { time = frameId, emote = emote }

                                            Nothing ->
                                                player.lastEmote
                                    , clickStart =
                                        case player.isDead of
                                            Just _ ->
                                                Nothing

                                            Nothing ->
                                                case input.action of
                                                    ClickStart point ->
                                                        Just { position = point, time = frameId }

                                                    ClickRelease _ ->
                                                        Nothing

                                                    NoAction ->
                                                        case player.clickStart of
                                                            Just clickStart ->
                                                                if
                                                                    frameTimeElapsed clickStart.time frameId
                                                                        |> Quantity.greaterThanOrEqualTo clickTotalDelay
                                                                then
                                                                    Nothing

                                                                else
                                                                    player.clickStart

                                                            Nothing ->
                                                                player.clickStart
                                    , isDead =
                                        case hitBySnowball of
                                            Just snowball ->
                                                { time = frameId
                                                , fallDirection =
                                                    vector3To2 snowball.velocity
                                                        |> Vector2d.direction
                                                        |> Maybe.withDefault Direction2d.x
                                                }
                                                    |> Just

                                            Nothing ->
                                                player.isDead
                                }
                                model2.players
                        , snowballs =
                            case ( player.clickStart, input.action ) of
                                ( Just clickStart, ClickRelease point ) ->
                                    let
                                        elapsed : Duration
                                        elapsed =
                                            frameTimeElapsed clickStart.time frameId
                                    in
                                    if
                                        (elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay)
                                            && (elapsed |> Quantity.lessThan clickTotalDelay)
                                    then
                                        let
                                            direction : Direction2d WorldCoordinate
                                            direction =
                                                Direction2d.from player.position clickStart.position
                                                    |> Maybe.withDefault Direction2d.x

                                            { x, y } =
                                                Point2d.toMeters player.position
                                        in
                                        { thrownBy = userId
                                        , thrownAt = frameId
                                        , velocity = throwVelocity direction (throwDistance elapsed)
                                        , position = Point3d.meters x y (Length.inMeters snowballStartHeight)
                                        }
                                            :: snowballs

                                    else
                                        snowballs

                                _ ->
                                    snowballs
                        , particles =
                            case hitBySnowball of
                                Just snowball ->
                                    snowballParticles frameId snowball.thrownAt snowball.position ++ model2.particles

                                Nothing ->
                                    model2.particles
                    }
                )
                model
                model.players

        updatedVelocities_ : SeqDict (Id UserId) Player
        updatedVelocities_ =
            updateVelocities frameId model3.players
    in
    let
        ( survivingSnowballs, newParticles ) =
            List.foldl
                (\snowball ( snowballs2, particles2 ) ->
                    let
                        position : Point3d Meters WorldCoordinate
                        position =
                            Point3d.translateBy (Vector3d.for Match.frameDuration snowball.velocity) snowball.position
                    in
                    if Point3d.zCoordinate position |> Quantity.greaterThanZero then
                        ( { position = position
                          , velocity = Vector3d.plus (Vector3d.for Match.frameDuration gravityVector) snowball.velocity
                          , thrownBy = snowball.thrownBy
                          , thrownAt = snowball.thrownAt
                          }
                            :: snowballs2
                        , particles2
                        )

                    else
                        ( snowballs2
                        , snowballParticles frameId snowball.thrownAt position ++ particles2
                        )
                )
                ( [], [] )
                model3.snowballs
    in
    { players =
        SeqDict.map
            (\id player ->
                SeqDict.remove id updatedVelocities_
                    |> SeqDict.values
                    |> List.foldl (\a b -> handleCollision frameId b a |> Tuple.first) player
            )
            updatedVelocities_
    , snowballs = List.reverse survivingSnowballs
    , particles =
        List.filter
            (\particle -> frameTimeElapsed particle.spawnedAt frameId |> Quantity.lessThan particle.lifetime)
            model3.particles
            ++ newParticles
    }


vector3To2 : Vector3d u c -> Vector2d u c
vector3To2 v =
    let
        { x, y } =
            Vector3d.unwrap v
    in
    Vector2d.unsafe { x = x, y = y }


point2To3 : Quantity Float u -> Point2d u c -> Point3d u c
point2To3 z p =
    Point3d.xyz (Point2d.xCoordinate p) (Point2d.yCoordinate p) z


snowballParticles : Id FrameId -> Id FrameId -> Point3d Meters WorldCoordinate -> List Particle
snowballParticles frameId thrownAt impactPosition =
    let
        { x, y } =
            Point3d.toMeters impactPosition

        count : number
        count =
            8

        particleGenerator : Int -> Random.Generator Particle
        particleGenerator index =
            Random.map3
                (\size lifetime speed ->
                    { position = Point2d.meters x y
                    , velocity =
                        Vector2d.withLength
                            (Speed.metersPerSecond speed)
                            ((toFloat index / count) |> Angle.turns |> Direction2d.fromAngle)
                    , size = Length.meters size
                    , spawnedAt = frameId
                    , lifetime = Duration.seconds lifetime
                    }
                )
                (Random.float 0.1 0.3)
                (Random.float 0.1 0.2)
                (Random.float 2 4)
    in
    Random.step
        (List.range 0 (count - 1) |> List.map particleGenerator |> Random.Extra.sequence)
        (Random.initialSeed (Id.toInt frameId * 1000 + Id.toInt thrownAt))
        |> Tuple.first


sphere : Vec3 -> Vec3 -> Vec3 -> TriangularMesh Vertex
sphere position scaleBy color =
    let
        p =
            Vec3.toRecord position

        s =
            Vec3.toRecord scaleBy

        uDetail =
            18

        vDetail =
            10
    in
    TriangularMesh.indexedBall
        uDetail
        vDetail
        (\u v ->
            let
                longitude =
                    2 * pi * toFloat u / toFloat uDetail

                latitude =
                    pi * toFloat v / toFloat vDetail
            in
            { position =
                Vec3.vec3
                    (sin longitude * sin latitude * s.x + p.x)
                    (cos longitude * sin latitude * s.y + p.y)
                    (cos latitude * s.z + p.z)
            , color = color
            }
        )


sphereMesh : Vec3 -> Vec3 -> Vec3 -> Mesh Vertex
sphereMesh position scaleBy color =
    let
        sphere2 =
            sphere position scaleBy color
    in
    TriangularMesh.faceVertices sphere2 |> WebGL.triangles


playerHead : Mesh Vertex
playerHead =
    sphereMesh (Vec3.vec3 0 0 1.5) (Vec3.vec3 0.9 0.9 0.9) (Vec3.vec3 1 1 1)


playerBody : Mesh Vertex
playerBody =
    sphereMesh (Vec3.vec3 0 0 0.7) (Vec3.vec3 1 1 1.5) (Vec3.vec3 1 1 1)


playerHand : Mesh Vertex
playerHand =
    sphereMesh (Vec3.vec3 0 0 0.7) (Vec3.vec3 0.3 0.3 0.3) (Vec3.vec3 1 1 1)


playerEyes : Mesh Vertex
playerEyes =
    let
        leftEye =
            sphere (Vec3.vec3 0.8 0.4 1.6) (Vec3.vec3 0.12 0.12 0.12) (Vec3.vec3 0 0 0) |> TriangularMesh.faceVertices

        rightEye =
            sphere (Vec3.vec3 0.8 -0.4 1.6) (Vec3.vec3 0.12 0.12 0.12) (Vec3.vec3 0 0 0) |> TriangularMesh.faceVertices
    in
    leftEye ++ rightEye |> WebGL.triangles


deadPlayerEyes : Mesh Vertex
deadPlayerEyes =
    let
        size : Float
        size =
            0.2

        z : Float
        z =
            1.6

        xLine : Float -> List ( Vertex, Vertex, Vertex )
        xLine y =
            let
                thickness =
                    0.1

                color =
                    Vec3.vec3 0 0 0

                line1 =
                    [ ( { position = Vec3.vec3 0.9 (y - size - thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size + thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size + thickness) (z + size), color = color }
                      )
                    , ( { position = Vec3.vec3 0.9 (y + size + thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size - thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size - thickness) (z - size), color = color }
                      )
                    ]

                line2 =
                    [ ( { position = Vec3.vec3 0.9 (y - size - thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size + thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size + thickness) (z - size), color = color }
                      )
                    , ( { position = Vec3.vec3 0.9 (y + size + thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size - thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size - thickness) (z + size), color = color }
                      )
                    ]
            in
            line1 ++ line2
    in
    (xLine 0.4 ++ xLine -0.4) |> WebGL.triangles


getBotInput : Id FrameId -> MatchState -> Id UserId -> Player -> Input
getBotInput frameId _ userId player =
    case player.isDead of
        Just _ ->
            { action = NoAction, emote = Nothing }

        Nothing ->
            let
                seed =
                    Random.initialSeed (Id.toInt frameId + Id.toInt userId * 1000)

                shouldPickNewTarget =
                    player.targetPosition == Nothing || modBy 60 (Id.toInt frameId + Id.toInt userId * 7) == 0

                ( randomX, seed2 ) =
                    Random.step (Random.float -2 8) seed

                ( randomY, _ ) =
                    Random.step (Random.float -2 8) seed2

                newTargetPosition =
                    Point2d.meters randomX randomY
            in
            case player.clickStart of
                Nothing ->
                    if shouldPickNewTarget then
                        { action = ClickStart newTargetPosition, emote = Nothing }

                    else
                        noInput

                Just _ ->
                    { action = ClickRelease newTargetPosition, emote = Nothing }


gravity : Acceleration
gravity =
    Acceleration.metersPerSecondSquared -4


gravityVector : Vector3d MetersPerSecondSquared WorldCoordinate
gravityVector =
    Vector3d.xyz Quantity.zero Quantity.zero gravity


snowballStartHeight : Length
snowballStartHeight =
    Length.meters 1


maxThrowDistance : Length
maxThrowDistance =
    Length.meters 15


throwVelocity : Direction2d WorldCoordinate -> Length -> Vector3d MetersPerSecond WorldCoordinate
throwVelocity direction distance =
    let
        -- For projectile motion starting at height h above ground:
        -- The ball lands when y = 0, giving flight time from quadratic formula
        -- Range at 45°: R_45 = v*(v + sqrt(v² + 4gh)) / (2g)
        -- Solving R_45 = R_max gives: v² = g*R_max² / (R_max + h)
        g =
            Acceleration.inMetersPerSecondSquared gravity |> abs

        h =
            Length.inMeters snowballStartHeight

        rMax =
            Length.inMeters maxThrowDistance

        -- Constant throw speed calibrated so 45° gives maxThrowDistance
        vSquared =
            g * rMax * rMax / (rMax + h)

        v =
            sqrt vSquared

        d =
            Length.inMeters distance |> clamp 0.001 rMax

        -- Find launch angle using the trajectory equation:
        -- g*d²*tan²(θ) - 2*v²*d*tan(θ) + (g*d² - 2*v²*h) = 0
        -- Discriminant: D = v⁴ + 2*g*v²*h - g²*d²
        -- tan(θ) = (v² ± sqrt(D)) / (g*d)
        -- Use - solution to get lower/downward angles for short distances
        discriminant =
            vSquared * vSquared + 2 * g * vSquared * h - g * g * d * d

        tanTheta =
            (vSquared - sqrt (max 0 discriminant)) / (g * d)

        launchAngle =
            atan tanTheta

        -- Velocity components
        horizontalSpeed =
            v * cos launchAngle

        verticalSpeed =
            v * sin launchAngle

        -- Get the 2D direction components
        ( dirX, dirY ) =
            Direction2d.components direction
    in
    Vector3d.metersPerSecond
        (dirX * horizontalSpeed)
        (dirY * horizontalSpeed)
        verticalSpeed


throwCharge : Duration -> Float
throwCharge clickStartElapsed =
    let
        t : Float
        t =
            Quantity.ratio (clickStartElapsed |> Quantity.minus clickMoveMaxDelay) chargeMaxDelay

        offset =
            0.1
    in
    if t < 0.5 then
        (t * 2) * (1 - offset) + offset

    else
        (2 - t * 2) * (1 - offset) + offset


throwDistance : Duration -> Length
throwDistance clickStartElapsed =
    Quantity.multiplyBy (throwCharge clickStartElapsed) maxThrowDistance


updateVelocities : Id FrameId -> SeqDict (Id UserId) Player -> SeqDict (Id UserId) Player
updateVelocities frameId players =
    let
        elapsed : Duration
        elapsed =
            Quantity.multiplyBy (Id.toInt frameId |> toFloat) Match.frameDuration
    in
    SeqDict.map
        (\_ a ->
            let
                nearestCollision :
                    Maybe
                        { collisionVelocity : Vector2d Meters WorldCoordinate
                        , collisionPosition : Point2d Meters WorldCoordinate
                        }
                nearestCollision =
                    getCollisionCandidates a.position
                        |> SeqSet.toList
                        |> List.filterMap
                            (\line ->
                                let
                                    lineCollision =
                                        Geometry.circleLine playerRadius a.position a.velocity line
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
                                        case Geometry.circlePoint playerRadius a.position a.velocity point of
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
                    (case ( a.finishTime, elapsed |> Quantity.lessThan countdownDelay, a.targetPosition ) of
                        ( DidNotFinish, False, Just targetPos ) ->
                            let
                                distance =
                                    Vector2d.from a.position targetPos
                            in
                            distance
                                |> Vector2d.normalize
                                |> Vector2d.scaleBy 0.0015
                                |> Vector2d.unwrap
                                |> Vector2d.unsafe

                        _ ->
                            Vector2d.zero
                    )
                        |> Vector2d.plus a.velocity
                        |> Vector2d.scaleBy 0.95

                newTargetPosition : Maybe (Point2d Meters WorldCoordinate)
                newTargetPosition =
                    case a.targetPosition of
                        Just targetPos ->
                            if Point2d.distanceFrom a.position targetPos |> Quantity.lessThan stopAtDistance then
                                Nothing

                            else
                                a.targetPosition

                        Nothing ->
                            Nothing
            in
            case nearestCollision of
                Just { collisionVelocity, collisionPosition } ->
                    { position = collisionPosition
                    , targetPosition = newTargetPosition
                    , velocity = collisionVelocity
                    , rotation = a.rotation
                    , finishTime = a.finishTime
                    , lastCollision = Just frameId
                    , lastEmote = a.lastEmote
                    , clickStart = a.clickStart
                    , isDead = a.isDead
                    , team = a.team
                    }

                Nothing ->
                    { position = Point2d.translateBy a.velocity a.position
                    , targetPosition = newTargetPosition
                    , velocity = newVelocity
                    , rotation = a.rotation
                    , finishTime = a.finishTime
                    , lastCollision = a.lastCollision
                    , lastEmote = a.lastEmote
                    , clickStart = a.clickStart
                    , isDead = a.isDead
                    , team = a.team
                    }
        )
        players


stopAtDistance : Length
stopAtDistance =
    Length.meters 0.3


playerRadius : Length
playerRadius =
    Length.meters 0.5


arrow : Vec3 -> Mesh Vertex
arrow color =
    [ { v0 = ( -1, 1 ), v1 = ( 0, 0 ), v2 = ( 1, 1 ) }
    , { v0 = ( -0.5, 1 ), v1 = ( 0.5, 1 ), v2 = ( 0.5, 2 ) }
    , { v0 = ( -0.5, 2 ), v1 = ( -0.5, 1 ), v2 = ( 0.5, 2 ) }
    ]
        |> List.map
            (\{ v0, v1, v2 } ->
                ( { position = Vec3.vec3 (Tuple.first v0) (Tuple.second v0) 0
                  , color = color
                  }
                , { position = Vec3.vec3 (Tuple.first v1) (Tuple.second v1) 0
                  , color = color
                  }
                , { position = Vec3.vec3 (Tuple.first v2) (Tuple.second v2) 0
                  , color = color
                  }
                )
            )
        |> WebGL.triangles


moveArrow : WebGL.Mesh Vertex
moveArrow =
    arrow (Vec3.vec3 1 0.8 0.1)


aimingReticle : WebGL.Mesh Vertex
aimingReticle =
    let
        color =
            Vec3.vec3 0.3 0.7 1

        segments =
            32

        innerRadius =
            0.7

        outerRadius =
            1.0

        lineThickness =
            0.08

        toVertex ( x, y ) =
            { position = Vec3.vec3 x y 0, color = color }

        -- Ring segments
        ringTriangles =
            List.range 0 (segments - 1)
                |> List.concatMap
                    (\i ->
                        let
                            angle1 =
                                2 * pi * toFloat i / toFloat segments

                            angle2 =
                                2 * pi * toFloat (i + 1) / toFloat segments

                            inner1 =
                                ( innerRadius * cos angle1, innerRadius * sin angle1 )

                            outer1 =
                                ( outerRadius * cos angle1, outerRadius * sin angle1 )

                            inner2 =
                                ( innerRadius * cos angle2, innerRadius * sin angle2 )

                            outer2 =
                                ( outerRadius * cos angle2, outerRadius * sin angle2 )
                        in
                        [ ( toVertex inner1, toVertex outer1, toVertex outer2 )
                        , ( toVertex inner1, toVertex outer2, toVertex inner2 )
                        ]
                    )

        -- Horizontal crosshair
        horizontalCrosshair =
            [ ( toVertex ( -outerRadius, -lineThickness )
              , toVertex ( outerRadius, -lineThickness )
              , toVertex ( outerRadius, lineThickness )
              )
            , ( toVertex ( -outerRadius, -lineThickness )
              , toVertex ( outerRadius, lineThickness )
              , toVertex ( -outerRadius, lineThickness )
              )
            ]

        -- Vertical crosshair
        verticalCrosshair =
            [ ( toVertex ( -lineThickness, -outerRadius )
              , toVertex ( lineThickness, -outerRadius )
              , toVertex ( lineThickness, outerRadius )
              )
            , ( toVertex ( -lineThickness, -outerRadius )
              , toVertex ( lineThickness, outerRadius )
              , toVertex ( -lineThickness, outerRadius )
              )
            ]
    in
    (ringTriangles ++ horizontalCrosshair ++ verticalCrosshair)
        |> WebGL.triangles


handleCollision : Id FrameId -> Player -> Player -> ( Player, Player )
handleCollision frameId playerA playerB =
    case Geometry.circleCircle playerRadius playerA.position playerA.velocity playerB.position playerB.velocity of
        Just ( v1, v2 ) ->
            ( { playerA | velocity = v1, lastCollision = Just frameId }, { playerB | velocity = v2 } )

        Nothing ->
            ( playerA, playerB )


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
    circleMesh 1 (Vec3.vec3 0 0 0)
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
                ( { position = Vec3.vec3 (cos t0 * size) (sin t0 * size) 0, color = color }
                , { position = Vec3.vec3 (cos t1 * size) (sin t1 * size) 0, color = color }
                , { position = Vec3.vec3 (cos t2 * size) (sin t2 * size) 0, color = color }
                )
            )


snowballMesh : WebGL.Mesh Vertex
snowballMesh =
    circleMesh 1 (Vec3.vec3 0 0 0)
        ++ circleMesh 0.85 (Vec3.vec3 1 1 1)
        |> WebGL.triangles


snowballShadowMesh : WebGL.Mesh Vertex
snowballShadowMesh =
    circleMesh 1 (Vec3.vec3 0.2 0.2 0.2)
        |> WebGL.triangles


particleMesh : WebGL.Mesh Vertex
particleMesh =
    circleMesh 0.9 (Vec3.vec3 1 1 1)
        |> WebGL.triangles


particleOutlineMesh : WebGL.Mesh Vertex
particleOutlineMesh =
    circleMesh 1 (Vec3.vec3 0 0 0)
        |> WebGL.triangles


snowballRadius : Quantity Float Meters
snowballRadius =
    Length.meters 0.2


type alias PlayerUniforms =
    { ucolor : Vec3, view : Mat4, model : Mat4 }


vertexShader : Shader Vertex PlayerUniforms { vcolor : Vec4 }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 color;
varying vec4 vcolor;
uniform vec3 ucolor;
uniform mat4 view;
uniform mat4 model;


void main () {
    gl_Position = view * model * vec4(position, 1.0);

    vcolor = vec4(color.xyz * ucolor, 1.0);
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
            float primaryThickness = 5.0;
            float secondaryThickness = 1.0;
            int x0 = modI(worldCoordinate.x + primaryThickness * 0.5, 8.0) <= primaryThickness ? 1 : 0;
            int y0 = modI(worldCoordinate.y + primaryThickness * 0.5, 8.0) <= primaryThickness ? 1 : 0;
            float value = x0 + y0 >= 1 ? 0.96 : 0.98;
            gl_FragColor = vec4(value, value, value, 1.0);
        }
    |]


initMatchData : ServerTime -> Nonempty ( Id UserId, PlayerData ) -> Maybe ( Id FrameId, MatchState ) -> MatchActiveLocal_
initMatchData serverTime newUserIds maybeTimelineCache =
    { timelineCache =
        case maybeTimelineCache of
            Just timelineCache ->
                { cache = List.Nonempty.singleton timelineCache } |> Ok

            Nothing ->
                initMatch serverTime newUserIds |> Timeline.init |> Ok
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
            |> SeqDict.fromList
    , wallMesh = lineSegmentMesh (Vec3.vec3 1 0 0) wallSegments
    , touchPosition = Nothing
    , previousTouchPosition = Nothing
    , primaryDown = Nothing
    , previousPrimaryDown = Nothing
    , desyncedAtFrame = Nothing
    }


updateMatchData :
    Match.Msg
    -> NetworkModel { userId : Id UserId, msg : Match.Msg } Match
    -> NetworkModel { userId : Id UserId, msg : Match.Msg } Match
    -> MatchLocalOnly
    -> MatchLocalOnly
updateMatchData newMsg newNetworkModel oldNetworkModel oldMatchData =
    let
        newMatchState : Match
        newMatchState =
            NetworkModel.localState Match.matchSetupUpdate newNetworkModel

        oldMatchState : Match
        oldMatchState =
            NetworkModel.localState Match.matchSetupUpdate oldNetworkModel
    in
    case ( Match.matchActive newMatchState, Match.matchActive oldMatchState ) of
        ( Just newMatch, Just _ ) ->
            case oldMatchData of
                MatchActiveLocal matchData ->
                    case ( matchData.timelineCache, newMsg ) of
                        ( Ok timelineCache, Match.MatchInputRequest serverTime _ ) ->
                            { matchData
                                | timelineCache =
                                    Timeline.addInput
                                        (Match.serverTimeToFrameId serverTime newMatch)
                                        timelineCache
                            }
                                |> MatchActiveLocal

                        _ ->
                            MatchActiveLocal matchData

                MatchSetupLocal _ ->
                    initMatchData newMatch.startTime (Match.allUsersAndBots newMatchState) Nothing |> MatchActiveLocal

                MatchError ->
                    MatchError

        ( Just newMatch, Nothing ) ->
            initMatchData newMatch.startTime (Match.allUsersAndBots newMatchState) Nothing |> MatchActiveLocal

        ( Nothing, Just _ ) ->
            initMatchSetupData newMatchState |> MatchSetupLocal

        ( Nothing, Nothing ) ->
            oldMatchData


actualTime : Config a -> Time.Posix
actualTime { time, debugTimeOffset } =
    Duration.addTo time debugTimeOffset


initMatch : ServerTime -> Nonempty ( Id UserId, PlayerData ) -> MatchState
initMatch startTime users =
    let
        playerIds =
            List.Nonempty.toList users
                |> List.filterMap
                    (\( userId, playerData ) ->
                        case playerData.mode of
                            PlayerMode ->
                                Just userId

                            SpectatorMode ->
                                Nothing
                    )

        ( shuffledPlayers, _ ) =
            Random.step
                (Random.shuffle playerIds)
                (Match.unwrapServerTime startTime |> Time.posixToMillis |> Random.initialSeed)

        -- Split players into two teams
        halfCount =
            (List.length shuffledPlayers + 1) // 2

        redTeamPlayers =
            List.take halfCount shuffledPlayers

        blueTeamPlayers =
            List.drop halfCount shuffledPlayers

        -- Team spawn positions (opposite corners)
        -- Red team: bottom-left corner
        redTeamStart =
            Point2d.meters -12 -10

        -- Blue team: top-right corner
        blueTeamStart =
            Point2d.meters 12 10

        spacing =
            Length.inMeters playerRadius * 2.1

        playersPerRow =
            4

        initTeamPlayers team startPos playerList =
            playerList
                |> List.indexedMap
                    (\index userId ->
                        let
                            x =
                                modBy playersPerRow index

                            y =
                                index // playersPerRow

                            -- For blue team, offset in negative direction (towards center)
                            ( xDir, yDir ) =
                                case team of
                                    RedTeam ->
                                        ( 1, 1 )

                                    BlueTeam ->
                                        ( -1, -1 )

                            position =
                                Point2d.translateBy
                                    (Vector2d.fromMeters
                                        { x = toFloat x * spacing * xDir
                                        , y = toFloat y * spacing * yDir
                                        }
                                    )
                                    startPos
                        in
                        ( userId, initPlayer team position )
                    )
    in
    { players =
        (initTeamPlayers RedTeam redTeamStart redTeamPlayers
            ++ initTeamPlayers BlueTeam blueTeamStart blueTeamPlayers
        )
            |> SeqDict.fromList
    , snowballs = []
    , particles = []
    }


initPlayer : Team -> Point2d Meters WorldCoordinate -> Player
initPlayer team position =
    let
        -- Teams face each other: Red faces northeast (towards blue), Blue faces southwest (towards red)
        facingDirection =
            case team of
                RedTeam ->
                    Direction2d.fromAngle (Angle.degrees 45)

                BlueTeam ->
                    Direction2d.fromAngle (Angle.degrees 225)
    in
    { position = position
    , targetPosition = Nothing
    , velocity = Vector2d.zero
    , rotation = facingDirection
    , finishTime = DidNotFinish
    , lastCollision = Nothing
    , lastEmote = Nothing
    , clickStart = Nothing
    , isDead = Nothing
    , team = team
    }


point2ToMatrix : Point2d units coordinates -> Mat4
point2ToMatrix point =
    let
        { x, y } =
            Point2d.unwrap point
    in
    Mat4.makeTranslate3 x y 0


point3ToMatrix : Point3d units coordinates -> Mat4
point3ToMatrix point =
    let
        { x, y, z } =
            Point3d.unwrap point
    in
    Mat4.makeTranslate3 x y z


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


countdownDelay : Duration
countdownDelay =
    Duration.seconds 4


timeToFrameId : Config a -> MatchActive -> Id FrameId
timeToFrameId model match =
    timeToServerTime model
        |> Match.unwrapServerTime
        |> Duration.from (Match.unwrapServerTime match.startTime)
        |> (\a -> Quantity.ratio a Match.frameDuration)
        |> round
        |> Id.fromInt


timeToServerTime : Config a -> ServerTime
timeToServerTime model =
    pingOffset model |> Duration.addTo (actualTime model) |> ServerTime


pingOffset : { a | pingData : Maybe PingData } -> Duration
pingOffset model =
    case model.pingData of
        Just pingData ->
            Quantity.plus pingData.lowEstimate pingData.highEstimate
                |> Quantity.divideBy 2
                |> Quantity.negate

        Nothing ->
            Quantity.zero


initMatchSetupData : Match -> MatchSetupLocal_
initMatchSetupData lobby =
    let
        preview : LobbyPreview
        preview =
            Match.preview lobby
    in
    { matchName = MatchName.toString preview.name
    , message = ""
    , maxPlayers = String.fromInt preview.maxUserCount
    , botCount = Match.botCount lobby |> String.fromInt
    }


scrollToBottom : Command FrontendOnly toMsg Msg
scrollToBottom =
    Dom.setViewportOf textMessageContainerId 0 99999
        |> Task.attempt (\_ -> ScrolledToBottom)


desyncWarning : Maybe (Id FrameId) -> Ui.Element msg
desyncWarning maybeDesyncFrame =
    case maybeDesyncFrame of
        Just _ ->
            Ui.column
                [ Ui.width Ui.shrink
                , Ui.alignTop
                , Ui.centerX
                , Ui.padding 16
                , Ui.spacing 8
                , Ui.background (Ui.rgba 240 0 0 0.9)
                , Ui.rounded 8
                , Ui.move { x = 0, y = 60, z = 0 }
                , noPointerEvents
                ]
                [ Ui.el
                    [ Ui.width Ui.shrink
                    , Ui.Font.size 20
                    , Ui.Font.bold
                    , Ui.Font.color (Ui.rgb 255 255 255)
                    , Ui.centerX
                    ]
                    (Ui.text "Desync Detected!")
                , Ui.el
                    [ Ui.width Ui.shrink
                    , Ui.Font.size 14
                    , Ui.Font.color (Ui.rgb 255 255 255)
                    , Ui.centerX
                    ]
                    (Ui.text "One or more players have desynced")
                ]

        Nothing ->
            Ui.none


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
    Ui.htmlAttribute (Html.Attributes.style "pointer-events" "none")


colorSelector : (ColorIndex -> msg) -> ColorIndex -> Ui.Element msg
colorSelector onSelect currentColor =
    List.Nonempty.toList ColorIndex.allColors
        |> List.map
            (\colorIndex ->
                MyUi.button
                    (colorSelectorHtmlId currentColor)
                    [ Ui.width (Ui.px 36)
                    , Ui.height (Ui.px 36)
                    , Ui.border
                        (if currentColor == colorIndex then
                            3

                         else
                            0
                        )
                    , Ui.borderColor (Ui.rgb 255 255 255)
                    , ColorIndex.toElColor colorIndex |> Ui.background
                    ]
                    { onPress = onSelect colorIndex
                    , label = Ui.none
                    }
            )
        |> Ui.row [ Ui.width Ui.shrink, Ui.wrap ]


colorSelectorHtmlId : ColorIndex -> HtmlId
colorSelectorHtmlId colorIndex =
    "matchPageColorSelector_"
        ++ (case colorIndex of
                Red ->
                    "Red"

                Green ->
                    "Green"

                Blue ->
                    "Blue"

                Orange ->
                    "Orange"

                Brown ->
                    "Brown"

                Purple ->
                    "Purple"

                Pink ->
                    "Pink"

                Yellow ->
                    "Yellow"
           )
        |> Dom.id


viewportHeight : Length
viewportHeight =
    Length.meters 25


getInput : Config a -> MatchActiveLocal_ -> Input
getInput config model =
    { action =
        case ( model.primaryDown, model.previousPrimaryDown ) of
            ( Just _, Nothing ) ->
                case model.touchPosition of
                    Just position ->
                        screenToWorld config.windowSize Point2d.origin viewportHeight position |> ClickStart

                    _ ->
                        NoAction

            ( Nothing, Just _ ) ->
                case model.touchPosition of
                    Just position ->
                        screenToWorld config.windowSize Point2d.origin viewportHeight position |> ClickRelease

                    _ ->
                        NoAction

            _ ->
                NoAction
    , emote =
        if Keyboard.keyPressed config (Keyboard.Character "1") then
            Just SurpriseEmote

        else if Keyboard.keyPressed config (Keyboard.Character "2") then
            Just ImpEmote

        else
            Nothing
    }


noInput : Input
noInput =
    { action = NoAction, emote = Nothing }


animationFrame : Config a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame config model =
    case ( model.matchData, Match.matchActive (getLocalState model) ) of
        ( MatchActiveLocal matchData, Just match ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId config match) cache match.timeline of
                        Ok ( newCache, matchState ) ->
                            let
                                input : Input
                                input =
                                    getInput config matchData

                                model3 : Model
                                model3 =
                                    { model
                                        | matchData =
                                            { matchData
                                                | previousTouchPosition = matchData.touchPosition
                                                , previousPrimaryDown = matchData.primaryDown
                                                , timelineCache = Ok newCache
                                            }
                                                |> MatchActiveLocal
                                    }

                                currentFrameId =
                                    timeToFrameId config match

                                ( oldestFrameId, oldestState ) =
                                    getOldestCachedState newCache

                                playerPositionsCmd : Command FrontendOnly ToBackend msg
                                playerPositionsCmd =
                                    if modBy 2 (Id.toInt oldestFrameId) == 0 && Env.isProduction then
                                        DesyncCheckRequest
                                            model.lobbyId
                                            oldestFrameId
                                            (SeqDict.map (\_ player -> player.position) oldestState.players)
                                            |> Effect.Lamdera.sendToBackend

                                    else
                                        Command.none
                            in
                            (if noInput == input then
                                ( model3, playerPositionsCmd )

                             else
                                matchSetupUpdate
                                    config.userId
                                    (Match.MatchInputRequest (timeToServerTime config) input)
                                    model3
                                    |> Tuple.mapSecond (\cmd -> Command.batch [ cmd, playerPositionsCmd ])
                            )
                                |> (\( matchSetupPage2, cmd ) ->
                                        case
                                            ( matchTimeLeft currentFrameId matchState
                                            , matchTimeLeft (Id.decrement currentFrameId) matchState
                                            )
                                        of
                                            ( Just timeLeft, Just previousTimeLeft ) ->
                                                if Quantity.lessThanZero timeLeft && not (Quantity.lessThanZero previousTimeLeft) then
                                                    matchSetupUpdate
                                                        config.userId
                                                        (Match.MatchFinished
                                                            (SeqDict.map
                                                                (\_ player -> player.finishTime)
                                                                matchState.players
                                                            )
                                                        )
                                                        matchSetupPage2
                                                        |> Tuple.mapSecond (\cmd2 -> Command.batch [ cmd, cmd2, scrollToBottom ])

                                                else
                                                    ( matchSetupPage2, cmd )

                                            _ ->
                                                ( matchSetupPage2, cmd )
                                   )

                        Err _ ->
                            ( model, Command.none )

                Err _ ->
                    ( model, Command.none )

        _ ->
            ( model, Command.none )


frameTimeElapsed : Id FrameId -> Id FrameId -> Duration
frameTimeElapsed start end =
    Quantity.multiplyBy (toFloat (Id.toInt end - Id.toInt start)) Match.frameDuration


matchTimeLeft : Id FrameId -> MatchState -> Maybe Duration
matchTimeLeft currentFrameId matchState =
    let
        finishes : List Duration
        finishes =
            SeqDict.toList matchState.players
                |> List.filterMap
                    (\( _, player ) ->
                        case player.finishTime of
                            Finished finishTime ->
                                Quantity.multiplyBy
                                    (Id.toInt currentFrameId - Id.toInt finishTime |> toFloat)
                                    Match.frameDuration
                                    |> Just

                            DidNotFinish ->
                                Nothing
                    )

        earliestFinish =
            Quantity.maximum finishes |> Maybe.withDefault Quantity.zero

        latestFinish =
            Quantity.minimum finishes |> Maybe.withDefault Quantity.zero

        allFinished =
            SeqDict.size matchState.players == List.length finishes

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
    Dom.id "textMessageContainer"


getLocalState : Model -> Match
getLocalState matchPage =
    NetworkModel.localState Match.matchSetupUpdate matchPage.networkModel


audio : Config a -> Model -> Audio.Audio
audio loaded matchPage =
    case ( Match.matchActive (getLocalState matchPage), matchPage.matchData ) of
        ( Just match, MatchActiveLocal matchData ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId loaded match) cache match.timeline of
                        Ok ( _, state ) ->
                            SeqDict.values state.players
                                |> List.filterMap .lastCollision
                                |> SeqSet.fromList
                                |> SeqSet.toList
                                |> List.map
                                    (\frameId ->
                                        let
                                            collisionTime : Time.Posix
                                            collisionTime =
                                                Quantity.multiplyBy (Id.toInt frameId |> toFloat) Match.frameDuration
                                                    |> Duration.addTo (Match.unwrapServerTime match.startTime)
                                                    |> (\a -> Duration.subtractFrom a (pingOffset loaded))
                                                    |> (\a -> Duration.subtractFrom a loaded.debugTimeOffset)
                                        in
                                        Audio.audio loaded.sounds.collision collisionTime
                                    )
                                |> Audio.group

                        Err _ ->
                            Audio.silence

                Err _ ->
                    Audio.silence

        _ ->
            Audio.silence
