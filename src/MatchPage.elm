module MatchPage exposing
    ( MatchId
    , MatchLocalOnly
    , Model
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
    , canvasViewHelper
    , fragmentShader
    , init
    , lineMesh
    , lineSegmentMesh
    , scrollToBottom
    , squareMesh
    , unnamedMatchText
    , update
    , updateFromBackend
    , vertexShader
    , view
    )

import Angle exposing (Angle)
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Audio
import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import Camera3d exposing (Camera3d)
import Collision
import ColorIndex exposing (ColorIndex)
import Decal exposing (Decal)
import Dict as RegularDict
import Direction2d exposing (Direction2d)
import Direction3d
import Duration exposing (Duration)
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
import Geometry.Interop.LinearAlgebra.Point2d
import Html.Attributes
import Html.Events
import Html.Events.Extra.Touch
import Id exposing (Id)
import Json.Decode
import Keyboard exposing (Key)
import Keyboard.Arrows
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Match exposing (LobbyPreview, Match, MatchActive, MatchState, Place(..), Player, PlayerData, PlayerMode(..), ServerTime(..), TimelineEvent, WorldCoordinate)
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
import Size exposing (Size)
import Sounds exposing (Sounds)
import TextMessage exposing (TextMessage)
import Timeline exposing (FrameId, TimelineCache)
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
    | PointerDown Html.Events.Extra.Touch.Event
    | PointerUp
    | PointerMoved Html.Events.Extra.Touch.Event


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
    , userIds : Dict (Id UserId) (Mesh Vertex)
    , wallMesh : Mesh Vertex
    , touchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    }


type ToBackend
    = MatchSetupRequest (Id MatchId) (Id EventId) Match.Msg


type ToFrontend
    = MatchSetupBroadcast (Id MatchId) (Id UserId) Match.Msg
    | MatchSetupResponse (Id MatchId) (Id UserId) Match.Msg (Id EventId)


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
                            { matchData
                                | touchPosition =
                                    case event.targetTouches ++ event.changedTouches ++ event.touches of
                                        head :: _ ->
                                            Point2d.fromTuple Pixels.pixels head.clientPos |> Just

                                        _ ->
                                            matchData.touchPosition
                            }
                                |> MatchActiveLocal

                        MatchSetupLocal _ ->
                            model.matchData
              }
            , Command.none
            )

        PointerUp ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            MatchActiveLocal { matchData | touchPosition = Nothing }

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
                            { matchData
                                | touchPosition =
                                    case event.targetTouches ++ event.changedTouches ++ event.touches of
                                        head :: _ ->
                                            Point2d.fromTuple Pixels.pixels head.clientPos |> Just

                                        _ ->
                                            matchData.touchPosition
                            }
                                |> MatchActiveLocal

                        MatchSetupLocal _ ->
                            model.matchData
              }
            , Command.none
            )

        ScrolledToBottom ->
            ( model, Command.none )


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
        , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
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
    case ( Match.matchActive lobby, model.matchData, Match.allUsers_ lobby |> Dict.get config.userId ) of
        ( Just match, MatchActiveLocal matchData, _ ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId config match) cache match.timeline of
                        Ok ( _, matchState ) ->
                            Element.el
                                (Element.width Element.fill
                                    :: Element.height Element.fill
                                    :: Element.htmlAttribute (Html.Events.Extra.Touch.onStart PointerDown)
                                    :: Element.htmlAttribute (Html.Events.Extra.Touch.onCancel (\_ -> PointerUp))
                                    :: Element.htmlAttribute (Html.Events.Extra.Touch.onEnd (\_ -> PointerUp))
                                    :: Element.inFront (countdown config match)
                                    :: Element.behindContent
                                        (canvasView
                                            config.windowSize
                                            config.devicePixelRatio
                                            (canvasViewHelper config model)
                                        )
                                    :: (case matchData.touchPosition of
                                            Just _ ->
                                                [ Element.htmlAttribute (Html.Events.Extra.Touch.onMove PointerMoved) ]

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

        places : Dict (Id UserId) Int
        places =
            Match.previousMatchFinishTimes lobby
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

        preview =
            Match.preview lobby
    in
    Element.column
        [ Element.spacing 8
        , Element.padding (Ui.ifMobile displayType 8 16)
        , Element.width (Element.maximum 800 Element.fill)
        , Element.height Element.fill
        ]
        [ case Dict.get config.userId places of
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
camera position viewportHeight =
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
        , viewportHeight = viewportHeight
        }


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
                                    case Dict.get model.userId state.players of
                                        Just player ->
                                            ( player.position, 1 )

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
                                    zoomFactor
                                        * toFloat (max canvasWidth canvasHeight)
                                        / (toFloat canvasHeight * 2000)

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

                                input : Maybe (Direction2d WorldCoordinate)
                                input =
                                    getInputDirection model.windowSize model.currentKeys matchData.touchPosition
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

                Err _ ->
                    []

        _ ->
            []


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
        |> Polygon2d.translateBy (Vector2d.meters 0 -800)


playerStart : Point2d Meters WorldCoordinate
playerStart =
    Point2d.fromMeters { x = 2300, y = 0 }



--Point2d.fromMeters { x = 0, y = 0 }


wallSegments : List (LineSegment2d Meters WorldCoordinate)
wallSegments =
    Collision.pointsToLineSegments (Polygon2d.outerLoop wall)
        ++ List.concatMap Collision.pointsToLineSegments (Polygon2d.innerLoops wall)


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


lineSegmentMesh : Vec3 -> List (LineSegment2d Meters WorldCoordinate) -> Mesh Vertex
lineSegmentMesh color lines =
    List.concatMap (lineMesh (Length.meters 10) color) lines |> WebGL.triangles


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
            Quantity.multiplyBy (Id.toInt frameId |> toFloat) Match.frameDuration
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
                ( { position = Math.Vector2.vec2 (Tuple.first v0) (Tuple.second v0)
                  , color = Math.Vector3.vec3 1 0.8 0.1
                  }
                , { position = Math.Vector2.vec2 (Tuple.first v1) (Tuple.second v1)
                  , color = Math.Vector3.vec3 1 0.8 0.1
                  }
                , { position = Math.Vector2.vec2 (Tuple.first v2) (Tuple.second v2)
                  , color = Math.Vector3.vec3 1 0.8 0.1
                  }
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
                    |> Dict.fromList
            , wallMesh = lineSegmentMesh (Math.Vector3.vec3 1 0 0) wallSegments
            , touchPosition = Nothing
            , previousTouchPosition = Nothing
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


getInputDirection : Size -> List Key -> Maybe (Point2d Pixels ScreenCoordinate) -> Maybe (Direction2d WorldCoordinate)
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


animationFrame : Config a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame model matchSetupPage =
    case ( matchSetupPage.matchData, Match.matchActive (getLocalState matchSetupPage) ) of
        ( MatchActiveLocal matchData, Just match ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId model match) cache match.timeline of
                        Ok ( newCache, matchState ) ->
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

                                model3 : Model
                                model3 =
                                    { matchSetupPage
                                        | matchData =
                                            { matchData
                                                | previousTouchPosition = matchData.touchPosition
                                                , timelineCache = Ok newCache
                                            }
                                                |> MatchActiveLocal
                                    }

                                currentFrameId =
                                    timeToFrameId model match
                            in
                            (if inputUnchanged then
                                ( model3, Command.none )

                             else
                                matchSetupUpdate model.userId (Match.MatchInputRequest (timeToServerTime model) input) model3
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
                                                        model.userId
                                                        (Match.MatchFinished
                                                            (Dict.map
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
                            ( matchSetupPage, Command.none )

                Err _ ->
                    ( matchSetupPage, Command.none )

        _ ->
            ( matchSetupPage, Command.none )


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
                            Dict.values state.players
                                |> List.filterMap .lastCollision
                                |> Set.fromList
                                |> Set.toList
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
