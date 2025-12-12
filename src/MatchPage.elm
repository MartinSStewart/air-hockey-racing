module MatchPage exposing
    ( MatchId
    , MatchLocalOnly
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
    , unnamedMatchText
    , update
    , updateFromBackend
    , vertexShader
    , view
    )

import Angle exposing (Angle)
import Audio
import Axis2d
import Axis3d
import BoundingBox2d exposing (BoundingBox2d)
import Camera3d exposing (Camera3d)
import ColorIndex exposing (ColorIndex)
import Decal exposing (Decal)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Direction3d
import Duration exposing (Duration)
import Ease exposing (Easing)
import Effect.Browser.Dom exposing (HtmlId)
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Task as Task
import Effect.Time as Time
import Effect.WebGL as WebGL exposing (Mesh, Shader)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Env
import FontRender
import Frame2d
import Geometry
import Geometry.Interop.LinearAlgebra.Point2d
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer
import Html.Events.Extra.Touch
import Id exposing (Id)
import Json.Decode
import Keyboard exposing (Key)
import Keyboard.Arrows
import KeyboardExtra as Keyboard
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Match exposing (Action(..), Emote(..), Input, LobbyPreview, Match, MatchActive, MatchState, Place(..), Player, PlayerData, PlayerMode(..), ServerTime(..), Snowball, TimelineEvent, WorldCoordinate)
import MatchName exposing (MatchName)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import NetworkModel exposing (EventId, NetworkModel)
import PingData exposing (PingData)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..), Rate)
import Random
import Random.List as Random
import RasterShapes
import Rectangle2d exposing (Rectangle2d)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Shape
import Size exposing (Size)
import Sounds exposing (Sounds)
import Speed
import TextMessage exposing (TextMessage)
import Timeline exposing (FrameId, TimelineCache, getOldestCachedState)
import Ui
import User exposing (UserId)
import Vector2d exposing (Vector2d)
import Viewpoint3d
import WebGL.Matrices
import WebGL.Settings


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


type MatchId
    = LobbyId Never


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_


type alias Model =
    { lobbyId : Id MatchId
    , networkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
    , matchData : MatchLocalOnly
    }


init : Id MatchId -> Match -> ( Model, Command FrontendOnly toMsg Msg )
init lobbyId lobby =
    let
        networkModel =
            NetworkModel.init lobby
    in
    ( { lobbyId = lobbyId
      , networkModel = networkModel
      , matchData =
            updateMatchData
                Match.JoinMatchSetup
                networkModel
                networkModel
                (initMatchSetupData lobby |> MatchSetupLocal)
      }
    , scrollToBottom
    )


type alias MatchSetupLocal_ =
    { matchName : String, message : String, maxPlayers : String }


type alias Vertex =
    { position : Vec2, color : Vec3 }


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
    = MatchSetupRequest (Id MatchId) (Id EventId) Match.Msg
    | DesyncCheckRequest (Id MatchId) (Id Timeline.FrameId) (SeqDict (Id UserId) (Point2d Meters WorldCoordinate))


type ToFrontend
    = MatchSetupBroadcast (Id MatchId) (Id UserId) Match.Msg
    | MatchSetupResponse (Id MatchId) (Id UserId) Match.Msg (Id EventId)
    | DesyncBroadcast (Id MatchId) (Id FrameId)


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
              }
            , Command.none
            )

        ScrolledToBottom ->
            ( model, Command.none )

        PressedLeaveMatch ->
            matchSetupUpdate config.userId Match.LeaveMatchSetup model


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
    , MatchSetupRequest matchSetup.lobbyId eventId msg |> Effect.Lamdera.sendToBackend
    )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly toMsg Msg )
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
                }

              else
                matchSetup
            , Command.none
            )


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


view : Config a -> Model -> Element Msg
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
                            Element.el
                                (Element.width Element.fill
                                    :: Element.height Element.fill
                                    :: Element.htmlAttribute (Html.Events.Extra.Pointer.onDown PointerDown)
                                    :: Element.htmlAttribute (Html.Events.Extra.Pointer.onUp PointerUp)
                                    :: Element.htmlAttribute (Html.Events.Extra.Pointer.onLeave PointerLeave)
                                    :: Element.inFront (countdown config match)
                                    :: Element.inFront (desyncWarning matchData.desyncedAtFrame)
                                    :: Element.inFront
                                        (Element.Input.button
                                            []
                                            { onPress = Just PressedLeaveMatch, label = Element.el [ Element.Background.color (Element.rgb 255 255 255) ] (Element.text "Leave match") }
                                        )
                                    :: Element.behindContent
                                        (canvasView
                                            config.windowSize
                                            config.devicePixelRatio
                                            (canvasViewHelper config model)
                                        )
                                    :: (case matchData.touchPosition of
                                            Just _ ->
                                                [ Element.htmlAttribute (Html.Events.Extra.Pointer.onMove PointerMoved) ]

                                            Nothing ->
                                                []
                                       )
                                )
                                (matchEndText match matchState config)

                        Err _ ->
                            Element.text "An error occurred during the match :("

                Err _ ->
                    Element.text "An error occurred during the match :("

        ( Nothing, MatchSetupLocal matchSetupData, Just currentPlayerData ) ->
            matchSetupView config lobby matchSetupData currentPlayerData

        _ ->
            Element.text "Loading..."


matchSetupView : Config a -> Match -> MatchSetupLocal_ -> PlayerData -> Element Msg
matchSetupView config lobby matchSetupData currentPlayerData =
    let
        displayType =
            Ui.displayType config.windowSize

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
    Element.column
        [ Element.spacing 8
        , Element.padding (Ui.ifMobile displayType 8 16)
        , Element.width (Element.maximum 800 Element.fill)
        , Element.height Element.fill
        ]
        [ case SeqDict.get config.userId places of
            Just place ->
                placementText place

            Nothing ->
                Element.none
        , if Match.isOwner config.userId lobby then
            Element.row
                [ Element.spacing 8, Element.width Element.fill ]
                (Element.Input.text
                    [ Element.padding 4, Element.width Element.fill ]
                    { onChange = TypedMatchName
                    , text = matchSetupData.matchName
                    , placeholder = Element.Input.placeholder [] unnamedMatchText |> Just
                    , label = Element.Input.labelHidden "Match name"
                    }
                    :: (if matchSetupData.matchName == matchName then
                            []

                        else
                            Ui.simpleButton PressedResetMatchName (Element.text "Reset")
                                :: (case MatchName.fromString matchSetupData.matchName of
                                        Ok matchName_ ->
                                            [ Ui.simpleButton (PressedSaveMatchName matchName_) (Element.text "Save") ]

                                        _ ->
                                            []
                                   )
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
        , if Match.isOwner config.userId lobby then
            Element.row
                [ Element.spacing 8 ]
                (Element.Input.text
                    [ Element.width (Element.px 50), Element.padding 4, Element.Font.alignRight ]
                    { onChange = TypedMaxPlayers
                    , text = matchSetupData.maxPlayers
                    , placeholder = Nothing
                    , label = Element.Input.labelLeft [] (Element.text "Max players")
                    }
                    :: (if matchSetupData.maxPlayers == String.fromInt preview.maxUserCount then
                            []

                        else
                            Ui.simpleButton PressedResetMaxPlayers (Element.text "Reset")
                                :: (case String.toInt matchSetupData.maxPlayers of
                                        Just maxPlayers ->
                                            [ Ui.simpleButton (PressedSaveMaxPlayers maxPlayers) (Element.text "Save") ]

                                        Nothing ->
                                            []
                                   )
                       )
                )

          else
            Element.none
        , Element.wrappedRow
            [ Element.spacing 8 ]
            [ if Match.isOwner config.userId lobby then
                Ui.simpleButton PressedStartMatchSetup (Element.text "Start match")

              else
                Element.none
            , Ui.simpleButton PressedLeaveMatchSetup (Element.text "Leave")
            , case currentPlayerData.mode of
                PlayerMode ->
                    Ui.simpleButton (PressedPlayerMode SpectatorMode) (Element.text "Switch to spectator")

                SpectatorMode ->
                    Ui.simpleButton (PressedPlayerMode PlayerMode) (Element.text "Switch to player")
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
                                ++ (case SeqDict.get userId places of
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


textChat : MatchSetupLocal_ -> Match -> Element Msg
textChat matchSetupData lobby =
    Element.column
        [ Element.scrollbarY
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 4
        ]
        [ Match.messagesOldestToNewest lobby
            |> List.map
                (\{ userId, message } ->
                    let
                        userName : String
                        userName =
                            Id.toInt userId |> String.fromInt |> (++) "User "
                    in
                    Element.row
                        [ Element.Font.size 16 ]
                        [ (if Match.isOwner userId lobby then
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


canvasView : Size -> Quantity Float (Rate WorldPixel Pixels) -> (Size -> List WebGL.Entity) -> Element msg
canvasView windowSize devicePixelRatio entities =
    let
        ( cssWindowWidth, cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize windowSize devicePixelRatio
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True, WebGL.stencil 0 ]
        [ Html.Attributes.width (Pixels.inPixels actualCanvasSize.width)
        , Html.Attributes.height (Pixels.inPixels actualCanvasSize.height)
        , Html.Attributes.style "width" (String.fromInt (Pixels.inPixels cssWindowWidth) ++ "px")
        , Html.Attributes.style "height" (String.fromInt (Pixels.inPixels cssWindowHeight) ++ "px")
        ]
        (entities actualCanvasSize)
        |> Element.html


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
                , eyePoint = Point3d.fromMeters { x = x, y = y, z = 1 }
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


canvasViewHelper : Config a -> Model -> Size -> List WebGL.Entity
canvasViewHelper model matchSetup canvasSize =
    case ( Match.matchActive (getLocalState matchSetup), matchSetup.matchData ) of
        ( Just match, MatchActiveLocal matchData ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId model match) cache match.timeline of
                        Ok ( _, state ) ->
                            let
                                canvasWidth =
                                    Pixels.inPixels canvasSize.width

                                canvasHeight =
                                    Pixels.inPixels canvasSize.height

                                ( cameraPosition, zoomFactor ) =
                                    case SeqDict.get model.userId state.players of
                                        Just player ->
                                            ( player.position, 1 )

                                        Nothing ->
                                            let
                                                vectorAndDistance =
                                                    SeqDict.values state.players
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
                                            ( List.map
                                                (\{ vector, weight } ->
                                                    Vector2d.scaleBy weight vector
                                                )
                                                vectorAndWeight
                                                |> Vector2d.sum
                                                |> Vector2d.scaleBy (1 / totalWeight)
                                                |> (\v -> Point2d.translateBy v Point2d.origin)
                                            , 0.8
                                            )

                                zoom : Float
                                zoom =
                                    1 / Length.inMeters viewportHeight

                                --zoomFactor
                                --    * toFloat (max canvasWidth canvasHeight)
                                --    / (toFloat canvasHeight * 2000)
                                viewMatrix : Mat4
                                viewMatrix =
                                    WebGL.Matrices.viewProjectionMatrix
                                        (camera cameraPosition (Length.meters (1 / zoom)))
                                        { nearClipDepth = Length.meters 0.1
                                        , farClipDepth = Length.meters 10
                                        , aspectRatio =
                                            Quantity.ratio
                                                (Quantity.toFloatQuantity canvasSize.width)
                                                (Quantity.toFloatQuantity canvasSize.height)
                                        }

                                playerRadius_ : Float
                                playerRadius_ =
                                    Length.inMeters playerRadius
                            in
                            backgroundGrid cameraPosition zoom canvasSize
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
                                :: List.concatMap
                                    (\( userId, player ) ->
                                        drawPlayer
                                            (timeToFrameId model match)
                                            userId
                                            matchData
                                            viewMatrix
                                            player
                                            playerRadius_
                                    )
                                    (SeqDict.toList state.players)
                                ++ List.map
                                    (\snowball ->
                                        let
                                            currentFrameId =
                                                timeToFrameId model match

                                            position =
                                                snowballPosition currentFrameId snowball

                                            snowballRadius_ =
                                                Length.inMeters snowballRadius
                                        in
                                        WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                            vertexShader
                                            fragmentShader
                                            snowballMesh
                                            { view = viewMatrix
                                            , model =
                                                pointToMatrix position
                                                    |> Mat4.scale3 snowballRadius_ snowballRadius_ snowballRadius_
                                            }
                                    )
                                    state.snowballs
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
                                                                    arrowScale =
                                                                        0.2 + 0.4 * throwCharge elapsed

                                                                    angle =
                                                                        Direction2d.toAngle direction
                                                                            |> Angle.inRadians
                                                                            |> (\a -> a + pi / 2)
                                                                in
                                                                [ WebGL.entityWith
                                                                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                                                    vertexShader
                                                                    fragmentShader
                                                                    chargingArrow
                                                                    { view = viewMatrix
                                                                    , model =
                                                                        pointToMatrix player.position
                                                                            |> Mat4.rotate angle (Math.Vector3.vec3 0 0 1)
                                                                            |> Mat4.translate3 0 (-0.5 - arrowScale * 3) 0
                                                                            |> Mat4.scale3 arrowScale arrowScale arrowScale
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
                                                        { view = viewMatrix
                                                        , model =
                                                            pointToMatrix targetPos
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


drawPlayer : Id FrameId -> Id UserId -> MatchActiveLocal_ -> Mat4 -> Player -> Float -> List WebGL.Entity
drawPlayer frameId userId matchData viewMatrix player playerRadius_ =
    case SeqDict.get userId matchData.userIds of
        Just mesh ->
            [ WebGL.entityWith
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
            ]
                ++ (case player.lastEmote of
                        Just lastEmote ->
                            let
                                timeElapsed : Duration
                                timeElapsed =
                                    Quantity.multiplyBy
                                        (Id.toInt frameId - Id.toInt lastEmote.time |> toFloat)
                                        Match.frameDuration

                                emojiSize =
                                    toFromAndBack
                                        (Duration.milliseconds 200)
                                        (Duration.seconds 2)
                                        (Duration.milliseconds 200)
                                        timeElapsed
                                        Ease.outBack
                                        Ease.inBack
                                        0
                                        0.15
                            in
                            (case lastEmote.emote of
                                SurpriseEmote ->
                                    Shape.surprise.layers

                                ImpEmote ->
                                    Shape.imp.layers
                            )
                                |> List.concatMap
                                    (\layer ->
                                        FontRender.drawLayer
                                            layer.color
                                            layer.mesh
                                            (pointToMatrix (Point2d.translateBy (Vector2d.meters 40 30) player.position)
                                                |> Mat4.scale3 emojiSize emojiSize 1
                                            )
                                            viewMatrix
                                    )

                        Nothing ->
                            []
                   )

        Nothing ->
            []


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
            |> List.map (Point2d.scaleAbout Point2d.origin 0.05)
        )
        |> Polygon2d.translateBy (Vector2d.meters 0 -8)


playerStart : Point2d Meters WorldCoordinate
playerStart =
    Point2d.fromMeters { x = 23, y = 0 }



--Point2d.fromMeters { x = 0, y = 0 }


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
    List.concatMap (lineMesh (Length.meters 0.01) color) lines |> WebGL.triangles


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


pointToVec : Point2d units coordinate -> Vec2
pointToVec point2d =
    let
        { x, y } =
            Point2d.unwrap point2d
    in
    Math.Vector2.vec2 x y


clickMoveMaxDelay : Duration
clickMoveMaxDelay =
    Duration.seconds 0.5


chargeMaxDelay : Duration
chargeMaxDelay =
    Duration.seconds 2


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
                            SeqDict.get userId inputs2 |> Maybe.withDefault noInput
                    in
                    { model2
                        | players =
                            SeqDict.insert userId
                                { player
                                    | targetPosition =
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
                                    , lastEmote =
                                        case input.emote of
                                            Just emote ->
                                                Just { time = frameId, emote = emote }

                                            Nothing ->
                                                player.lastEmote
                                    , clickStart =
                                        case input.action of
                                            ClickStart point ->
                                                Just { position = point, time = frameId }

                                            ClickRelease _ ->
                                                Nothing

                                            NoAction ->
                                                player.clickStart
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
                                    if elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay then
                                        let
                                            direction : Direction2d WorldCoordinate
                                            direction =
                                                Direction2d.from player.position clickStart.position
                                                    |> Maybe.withDefault Direction2d.x
                                        in
                                        { thrownBy = userId
                                        , thrownAt = frameId
                                        , startVelocity =
                                            Vector2d.withLength
                                                (Speed.metersPerSecond (throwCharge elapsed * 10))
                                                direction
                                        , startPosition = player.position
                                        }
                                            :: model2.snowballs

                                    else
                                        model2.snowballs

                                _ ->
                                    model2.snowballs
                    }
                )
                model
                model.players

        updatedVelocities_ : SeqDict (Id UserId) Player
        updatedVelocities_ =
            updateVelocities frameId model3.players
    in
    { players =
        SeqDict.map
            (\id player ->
                SeqDict.remove id updatedVelocities_
                    |> SeqDict.values
                    |> List.foldl (\a b -> handleCollision frameId b a |> Tuple.first) player
            )
            updatedVelocities_
    , snowballs =
        List.filter
            (\snowball -> frameTimeElapsed snowball.thrownAt frameId |> Quantity.lessThan (Duration.seconds 10))
            model3.snowballs
    }


throwCharge : Duration -> Float
throwCharge clickStartElapsed =
    Quantity.ratio (clickStartElapsed |> Quantity.minus clickMoveMaxDelay) chargeMaxDelay


updateVelocities : Id FrameId -> SeqDict (Id UserId) Player -> SeqDict (Id UserId) Player
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
                                |> Vector2d.scaleBy 0.003
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
                    , finishTime = checkFinish a
                    , lastCollision = Just frameId
                    , lastEmote = a.lastEmote
                    , clickStart = a.clickStart
                    , isDead = a.isDead
                    }

                Nothing ->
                    { position = Point2d.translateBy a.velocity a.position
                    , targetPosition = newTargetPosition
                    , velocity = newVelocity
                    , rotation = a.rotation
                    , finishTime = checkFinish a
                    , lastCollision = a.lastCollision
                    , lastEmote = a.lastEmote
                    , clickStart = a.clickStart
                    , isDead = a.isDead
                    }
        )
        players


stopAtDistance : Length
stopAtDistance =
    Length.meters 0.3


playerRadius : Length
playerRadius =
    Length.meters 0.5


arrow : Vec3 -> Mesh { position : Vec2, color : Vec3 }
arrow color =
    [ { v0 = ( -1, 1 ), v1 = ( 0, 0 ), v2 = ( 1, 1 ) }
    , { v0 = ( -0.5, 1 ), v1 = ( 0.5, 1 ), v2 = ( 0.5, 2 ) }
    , { v0 = ( -0.5, 2 ), v1 = ( -0.5, 1 ), v2 = ( 0.5, 2 ) }
    ]
        |> List.map
            (\{ v0, v1, v2 } ->
                ( { position = Math.Vector2.vec2 (Tuple.first v0) (Tuple.second v0)
                  , color = color
                  }
                , { position = Math.Vector2.vec2 (Tuple.first v1) (Tuple.second v1)
                  , color = color
                  }
                , { position = Math.Vector2.vec2 (Tuple.first v2) (Tuple.second v2)
                  , color = color
                  }
                )
            )
        |> WebGL.triangles


moveArrow : WebGL.Mesh Vertex
moveArrow =
    arrow (Math.Vector3.vec3 1 0.8 0.1)


chargingArrow : WebGL.Mesh Vertex
chargingArrow =
    arrow (Math.Vector3.vec3 0.3 0.7 1)


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


snowballMesh : WebGL.Mesh Vertex
snowballMesh =
    circleMesh 1 (Math.Vector3.vec3 0 0 0)
        ++ circleMesh 0.9 (Math.Vector3.vec3 1 1 1)
        |> WebGL.triangles


snowballRadius : Quantity Float Meters
snowballRadius =
    Length.meters 0.15


snowballPosition : Id Timeline.FrameId -> Match.Snowball -> Point2d Meters WorldCoordinate
snowballPosition currentFrameId snowball =
    Point2d.translateBy
        (Vector2d.for (frameTimeElapsed snowball.thrownAt currentFrameId) snowball.startVelocity)
        snowball.startPosition


finishLine : BoundingBox2d Meters WorldCoordinate
finishLine =
    BoundingBox2d.from
        (Point2d.fromMeters { x = 5300, y = 800 })
        (Point2d.fromMeters { x = 6000, y = 1200 })


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
            float primaryThickness = 5.0;
            float secondaryThickness = 1.0;
            int x0 = modI(worldCoordinate.x + primaryThickness * 0.5, 8.0) <= primaryThickness ? 1 : 0;
            int y0 = modI(worldCoordinate.y + primaryThickness * 0.5, 8.0) <= primaryThickness ? 1 : 0;
            float value = x0 + y0 >= 1 ? 0.96 : 0.98;
            gl_FragColor = vec4(value, value, value, 1.0);
        }
    |]


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

        newUserIds : Nonempty ( Id UserId, PlayerData )
        newUserIds =
            Match.allUsers newMatchState

        initHelper : ServerTime -> MatchLocalOnly
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
                    |> SeqDict.fromList
            , wallMesh = lineSegmentMesh (Math.Vector3.vec3 1 0 0) wallSegments
            , touchPosition = Nothing
            , previousTouchPosition = Nothing
            , primaryDown = Nothing
            , previousPrimaryDown = Nothing
            , desyncedAtFrame = Nothing
            }
                |> MatchActiveLocal
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
                    initHelper newMatch.startTime

        ( Just newMatch, Nothing ) ->
            initHelper newMatch.startTime

        ( Nothing, Just _ ) ->
            initMatchSetupData newMatchState |> MatchSetupLocal

        _ ->
            oldMatchData


actualTime : Config a -> Time.Posix
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
            (Match.unwrapServerTime startTime |> Time.posixToMillis |> Random.initialSeed)
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

                        position =
                            Point2d.translateBy
                                (Vector2d.fromMeters { x = toFloat x * spacing, y = toFloat y * spacing })
                                playerStart
                    in
                    ( userId, initPlayer position )
                )
            |> SeqDict.fromList
    , snowballs = []
    }


initPlayer : Point2d Meters WorldCoordinate -> Player
initPlayer position =
    { position = position
    , targetPosition = Nothing
    , velocity = Vector2d.zero
    , rotation = Quantity.zero
    , finishTime = DidNotFinish
    , lastCollision = Nothing
    , lastEmote = Nothing
    , clickStart = Nothing
    , isDead = Nothing
    }


pointToMatrix : Point2d units coordinates -> Mat4
pointToMatrix point =
    let
        { x, y } =
            Point2d.unwrap point
    in
    Mat4.makeTranslate3 x y 0


unnamedMatchText : Element msg
unnamedMatchText =
    Element.el
        [ Element.Font.italic, Element.Font.color (Element.rgb 0.6 0.6 0.6) ]
        (Element.text "Unnamed match")


matchEndText : MatchActive -> MatchState -> Config a -> Element msg
matchEndText match matchState model =
    let
        maybeFinish : Maybe { place : Int, userId : Id UserId, finishTime : Id FrameId }
        maybeFinish =
            SeqDict.toList matchState.players
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
                , Quantity.multiplyBy (Id.toInt finish.finishTime |> toFloat) Match.frameDuration
                    |> timestamp_
                    |> Element.text
                    |> Element.el [ Element.centerX, Element.Font.bold, Element.Font.size 24 ]
                , case maybeTimeLeft of
                    Just timeLeft ->
                        Element.paragraph
                            [ Element.Font.center, Element.Font.bold, Element.Font.size 24 ]
                            [ "Match will end in "
                                ++ String.fromInt (round (Duration.inSeconds timeLeft))
                                |> Element.text
                            ]

                    Nothing ->
                        Element.none
                ]

        Nothing ->
            case maybeTimeLeft of
                Just timeLeft ->
                    Element.paragraph
                        [ Element.Font.center, Element.Font.bold, Element.Font.size 24 ]
                        [ "Someone finished! The match will end in "
                            ++ String.fromInt (round (Duration.inSeconds timeLeft))
                            |> Element.text
                        ]

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


countdownDelay : Duration
countdownDelay =
    Duration.seconds 3


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
    }


scrollToBottom : Command FrontendOnly toMsg Msg
scrollToBottom =
    Effect.Browser.Dom.setViewportOf textMessageContainerId 0 99999
        |> Task.attempt (\_ -> ScrolledToBottom)


countdown : Config a -> MatchActive -> Element msg
countdown model match =
    let
        elapsed : Duration
        elapsed =
            Quantity.multiplyBy (timeToFrameId model match |> Id.toInt |> toFloat) Match.frameDuration

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


desyncWarning : Maybe (Id FrameId) -> Element msg
desyncWarning maybeDesyncFrame =
    case maybeDesyncFrame of
        Just _ ->
            Element.column
                [ Element.alignTop
                , Element.centerX
                , Element.padding 16
                , Element.spacing 8
                , Element.Background.color (Element.rgba 0.8 0 0 0.9)
                , Element.Border.rounded 8
                , Element.moveDown 60
                , noPointerEvents
                ]
                [ Element.el
                    [ Element.Font.size 20
                    , Element.Font.bold
                    , Element.Font.color (Element.rgb 1 1 1)
                    , Element.centerX
                    ]
                    (Element.text "Desync Detected!")
                , Element.el
                    [ Element.Font.size 14
                    , Element.Font.color (Element.rgb 1 1 1)
                    , Element.centerX
                    ]
                    (Element.text "One or more players have desynced")
                ]

        Nothing ->
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


viewportHeight : Length
viewportHeight =
    Length.meters 20


getInput : Config a -> MatchState -> MatchActiveLocal_ -> Input
getInput config matchState model =
    { action =
        case ( model.primaryDown, model.previousPrimaryDown ) of
            ( Just _, Nothing ) ->
                case ( model.touchPosition, SeqDict.get config.userId matchState.players ) of
                    ( Just position, Just currentPlayer ) ->
                        screenToWorld config.windowSize currentPlayer.position viewportHeight position |> ClickStart

                    _ ->
                        NoAction

            ( Nothing, Just _ ) ->
                case ( model.touchPosition, SeqDict.get config.userId matchState.players ) of
                    ( Just position, Just currentPlayer ) ->
                        screenToWorld config.windowSize currentPlayer.position viewportHeight position |> ClickRelease

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
                                    getInput config matchState matchData

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
    Effect.Browser.Dom.id "textMessageContainer"


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
