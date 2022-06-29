module EditorPage exposing
    ( Config
    , Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , animationFrame
    , init
    , update
    , updateFromBackend
    , view
    )

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Axis2d
import Axis3d
import Camera3d exposing (Camera3d)
import CubicSpline2d
import Direction2d
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.WebGL as WebGL exposing (Entity, Mesh)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Lazy
import FontRender exposing (FontVertex)
import Geometry
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Geometry.Types exposing (Rectangle2d(..))
import Html.Events.Extra.Mouse exposing (Event)
import Html.Events.Extra.Wheel
import Id exposing (Id)
import Keyboard
import KeyboardExtra as Keyboard
import Length exposing (Length, Meters)
import LineSegment2d
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Match exposing (WorldCoordinate)
import MatchPage exposing (ScreenCoordinate, Vertex, WorldPixel)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2
import Math.Vector3
import Math.Vector4
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Ports
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity(..), Rate)
import Rectangle2d
import Serialize exposing (Codec)
import Shape exposing (Layer, LayerId, PathSegment)
import Size exposing (Size)
import Ui
import Vector2d exposing (Vector2d)
import WebGL.Matrices
import WebGL.Settings


type Msg
    = NoOp
    | MouseDown Event
    | MouseUp Event
    | MouseMoved Event
    | MouseLeft Event
    | MouseWheel Html.Events.Extra.Wheel.Event
    | PressedLayer (Id LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Id LayerId)
    | TypedColor { red : Int, green : Int, blue : Int }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Id LayerId)
    | PressedMoveLayerDown (Id LayerId)
    | PressedMirrorX


type alias Model =
    { mousePosition : Maybe (Point2d Pixels ScreenCoordinate)
    , mousePositionPrevious : Maybe (Point2d Pixels ScreenCoordinate)
    , mouseDownAt : Maybe (Point2d Meters WorldCoordinate)
    , wheelDownAt : Maybe (Point2d Meters WorldCoordinate)
    , cameraPosition : Point2d Meters WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length
    , meshCache : Dict (Id LayerId) { pathMesh : Mesh Vertex, pathFillMesh : Mesh FontVertex }
    , placingPoint : Maybe { index : Int, position : Point2d Meters WorldCoordinate }
    }


type alias EditorState =
    { layers : Dict (Id LayerId) Layer
    , currentLayer : Id LayerId
    , selectedNodes : Set Int
    }


initLayer : Layer
initLayer =
    { path = []
    , red = 255
    , green = 0
    , blue = 100
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend


type alias Config a =
    { a
        | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
        , windowSize : Size
        , currentKeys : List Keyboard.Key
        , previousKeys : List Keyboard.Key
    }


init : Model
init =
    { mousePosition = Nothing
    , mousePositionPrevious = Nothing
    , mouseDownAt = Nothing
    , wheelDownAt = Nothing
    , cameraPosition = Point2d.origin
    , undoHistory = []
    , editorState = initEditorState
    , redoHistory = []
    , viewportHeight = Length.meters 2000
    , meshCache = Dict.empty
    , placingPoint = Nothing
    }


initEditorState =
    { layers = Dict.fromList [ ( Id.fromInt 0, initLayer ) ]
    , currentLayer = Id.fromInt 0
    , selectedNodes = Set.empty
    }


getLayer : EditorState -> ( Id LayerId, Layer )
getLayer editorState =
    case Dict.get editorState.currentLayer editorState.layers of
        Just layer ->
            ( editorState.currentLayer, layer )

        Nothing ->
            Dict.toList editorState.layers |> List.head |> Maybe.withDefault ( Id.fromInt 0, initLayer )


setLayer : Layer -> EditorState -> EditorState
setLayer layer editorState =
    if Dict.member editorState.currentLayer editorState.layers then
        { editorState | layers = Dict.update editorState.currentLayer (\_ -> Just layer) editorState.layers }

    else
        case Dict.toList editorState.layers of
            ( id, _ ) :: rest ->
                { editorState | layers = ( id, layer ) :: rest |> Dict.fromList }

            [] ->
                { editorState | layers = [ ( Id.fromInt 0, layer ) ] |> Dict.fromList }


nearestPathIndex : Config a -> Point2d Meters WorldCoordinate -> Bool -> Set Int -> Layer -> Int
nearestPathIndex config point isSelectedLayer selection layer =
    pathToQuadraticSplines config Nothing isSelectedLayer selection layer.path
        |> List.indexedMap
            (\index2 splines ->
                List.map
                    (\spline ->
                        ( index2, Geometry.findNearestPoint point spline |> .dist )
                    )
                    splines
            )
        |> List.concat
        |> Quantity.minimumBy Tuple.second
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


startPathSegment : Config a -> Point2d Meters WorldCoordinate -> Model -> Model
startPathSegment config point model =
    let
        ( _, layer ) =
            getLayer model.editorState

        index : Int
        index =
            nearestPathIndex config point True model.editorState.selectedNodes layer
    in
    { model | placingPoint = Just { index = index + 1, position = point } }


fullPath : Config a -> Layer -> Bool -> Model -> List PathSegment
fullPath config layer isCurrentLayer model =
    case ( model.placingPoint, isCurrentLayer ) of
        ( Just { index, position }, True ) ->
            let
                mouseWorld : Vector2d Meters WorldCoordinate
                mouseWorld =
                    case ( model.mouseDownAt, model.mousePosition ) of
                        ( Just mouseDownAt, Just mousePosition ) ->
                            Vector2d.from (screenToWorld config model mousePosition) mouseDownAt

                        _ ->
                            Vector2d.zero
            in
            List.take index layer.path
                ++ [ { position = position
                     , handleNext = mouseWorld
                     , handlePrevious = Vector2d.reverse mouseWorld
                     }
                   ]
                ++ List.drop index layer.path

        _ ->
            layer.path


finishPathSegment : Point2d Meters WorldCoordinate -> Model -> Model
finishPathSegment point model =
    let
        ( _, layer ) =
            getLayer model.editorState
    in
    case model.placingPoint of
        Nothing ->
            model

        Just { index, position } ->
            addEditorState
                (setLayer
                    { layer
                        | path =
                            List.take index layer.path
                                ++ [ { position = position
                                     , handlePrevious = Vector2d.from position point
                                     , handleNext = Vector2d.from point position
                                     }
                                   ]
                                ++ List.drop index layer.path
                    }
                    model.editorState
                )
                { model | placingPoint = Nothing }


addEditorState : EditorState -> Model -> Model
addEditorState newEditorState model =
    { model
        | editorState = newEditorState
        , undoHistory = model.editorState :: model.undoHistory |> List.take 50
        , redoHistory = []
    }


replaceEditorState : EditorState -> Model -> Model
replaceEditorState newEditorState model =
    { model | editorState = newEditorState }


uiScale : { a | viewportHeight : Quantity Float Meters } -> Float
uiScale model =
    Quantity.ratio model.viewportHeight (Length.meters 800)


updateMesh : Config a -> Model -> Model -> Model
updateMesh config previousModel model =
    let
        uiScale_ =
            uiScale model
    in
    { model
        | meshCache =
            Dict.map
                (\layerId layer ->
                    let
                        fullPath_ =
                            fullPath config layer isCurrentLayer model

                        isCurrentLayer =
                            layerId == model.editorState.currentLayer

                        maybeDragging =
                            if isCurrentLayer then
                                isDragging config model

                            else
                                Nothing

                        maybeMouseWorldPosition =
                            case model.mousePosition of
                                Just mousePosition ->
                                    screenToWorld config model mousePosition |> Just

                                Nothing ->
                                    Nothing
                    in
                    case
                        ( Dict.get layerId model.meshCache
                        , (model.editorState == previousModel.editorState)
                            && not
                                (Keyboard.keyPressed config Keyboard.Control
                                    || Keyboard.keyPressed config Keyboard.Meta
                                    || Keyboard.keyReleased config Keyboard.Control
                                    || Keyboard.keyReleased config Keyboard.Meta
                                )
                            && (model.mousePosition == previousModel.mousePosition || (maybeDragging == Nothing && model.placingPoint == Nothing && not isCurrentLayer))
                            && (model.viewportHeight == previousModel.viewportHeight || not isCurrentLayer)
                        )
                    of
                        ( Just cache, True ) ->
                            cache

                        _ ->
                            let
                                _ =
                                    Debug.log "a" layerId

                                splines : List (QuadraticSpline2d Meters WorldCoordinate)
                                splines =
                                    pathToQuadraticSplines
                                        config
                                        maybeDragging
                                        isCurrentLayer
                                        model.editorState.selectedNodes
                                        fullPath_
                                        |> List.concat
                            in
                            { pathFillMesh =
                                List.map
                                    (\spline ->
                                        { position = QuadraticSpline2d.startPoint spline
                                        , controlPoint = QuadraticSpline2d.secondControlPoint spline
                                        }
                                    )
                                    splines
                                    |> Shape.shapeToMesh_
                            , pathMesh =
                                List.indexedMap
                                    (\index segment ->
                                        let
                                            color =
                                                if
                                                    isCurrentLayer
                                                        && Set.member index model.editorState.selectedNodes
                                                then
                                                    Math.Vector3.vec3 0 0.8 0.1

                                                else
                                                    Math.Vector3.vec3 0 0 0

                                            segment2 =
                                                dragSegment
                                                    config
                                                    index
                                                    maybeDragging
                                                    isCurrentLayer
                                                    model.editorState.selectedNodes
                                                    segment

                                            handlePrevious =
                                                Point2d.translateBy segment2.handlePrevious segment2.position

                                            handleNext =
                                                Point2d.translateBy segment2.handleNext segment2.position

                                            size =
                                                3 * uiScale_

                                            drawSquare p =
                                                let
                                                    { x, y } =
                                                        Point2d.toMeters p
                                                in
                                                [ ( { position = Math.Vector2.vec2 (x - size) (y - size)
                                                    , color = color
                                                    }
                                                  , { position = Math.Vector2.vec2 (x + size) (y - size)
                                                    , color = color
                                                    }
                                                  , { position = Math.Vector2.vec2 (x + size) (y + size)
                                                    , color = color
                                                    }
                                                  )
                                                , ( { position = Math.Vector2.vec2 (x - size) (y - size)
                                                    , color = color
                                                    }
                                                  , { position = Math.Vector2.vec2 (x + size) (y + size)
                                                    , color = color
                                                    }
                                                  , { position = Math.Vector2.vec2 (x - size) (y + size)
                                                    , color = color
                                                    }
                                                  )
                                                ]
                                        in
                                        drawSquare segment2.position
                                            ++ drawSquare handlePrevious
                                            ++ drawSquare handleNext
                                            ++ (case ( maybeMouseWorldPosition, maybeDragging, ( isCurrentLayer, model.placingPoint ) ) of
                                                    ( Just mouseWorldPosition, Nothing, ( True, Nothing ) ) ->
                                                        case
                                                            List.map
                                                                (Geometry.findNearestPoint mouseWorldPosition)
                                                                splines
                                                                |> Quantity.minimumBy .dist
                                                        of
                                                            Just { pos } ->
                                                                drawSquare pos

                                                            Nothing ->
                                                                []

                                                    _ ->
                                                        []
                                               )
                                            ++ MatchPage.lineMesh
                                                (Length.meters uiScale_)
                                                color
                                                (LineSegment2d.from
                                                    segment2.position
                                                    (Point2d.translateBy segment2.handleNext segment2.position)
                                                )
                                            ++ MatchPage.lineMesh
                                                (Length.meters uiScale_)
                                                color
                                                (LineSegment2d.from
                                                    segment2.position
                                                    (Point2d.translateBy segment2.handlePrevious segment2.position)
                                                )
                                    )
                                    fullPath_
                                    |> List.concat
                                    |> WebGL.triangles
                            }
                )
                model.editorState.layers
    }


dragSegment : Config a -> Int -> Maybe Dragging -> Bool -> Set Int -> PathSegment -> PathSegment
dragSegment config index maybeDragging isCurrentLayer selection pathSegment =
    let
        ctrlDown =
            Keyboard.keyDown config Keyboard.Control || Keyboard.keyDown config Keyboard.Meta
    in
    case maybeDragging of
        Just dragging ->
            if isCurrentLayer && Set.member index selection then
                case dragging.dragType of
                    CenterPoint ->
                        { pathSegment
                            | position = Point2d.translateBy dragging.offset pathSegment.position
                        }

                    NextHandle ->
                        let
                            handleNext =
                                Vector2d.plus dragging.offset pathSegment.handleNext
                        in
                        { pathSegment
                            | handleNext = handleNext
                            , handlePrevious =
                                if ctrlDown then
                                    Vector2d.reverse handleNext

                                else
                                    pathSegment.handlePrevious
                        }

                    PreviousHandle ->
                        let
                            handlePrevious =
                                Vector2d.plus dragging.offset pathSegment.handlePrevious
                        in
                        { pathSegment
                            | handlePrevious = handlePrevious
                            , handleNext =
                                if ctrlDown then
                                    Vector2d.reverse handlePrevious

                                else
                                    pathSegment.handleNext
                        }

            else
                pathSegment

        Nothing ->
            pathSegment


pathToQuadraticSplines :
    Config a
    -> Maybe Dragging
    -> Bool
    -> Set Int
    -> List PathSegment
    -> List (List (QuadraticSpline2d Meters WorldCoordinate))
pathToQuadraticSplines config maybeDragging isSelectedLayer selection path =
    case path of
        first :: rest ->
            let
                pathLength =
                    List.length path
            in
            List.foldl
                (\segment state ->
                    let
                        segment2 =
                            dragSegment config (modBy pathLength state.index) maybeDragging isSelectedLayer selection segment
                    in
                    { index = state.index + 1
                    , previousPoint = segment2
                    , curves =
                        state.curves
                            ++ [ Geometry.cubicSplineToQuadratic
                                    (Length.meters 1)
                                    (CubicSpline2d.fromControlPoints
                                        state.previousPoint.position
                                        (Point2d.translateBy
                                            state.previousPoint.handleNext
                                            state.previousPoint.position
                                        )
                                        (Point2d.translateBy
                                            segment2.handlePrevious
                                            segment2.position
                                        )
                                        segment2.position
                                    )
                                    |> List.Nonempty.toList
                               ]
                    }
                )
                { index = 1
                , previousPoint = dragSegment config 0 maybeDragging isSelectedLayer selection first
                , curves = []
                }
                (rest ++ [ first ])
                |> .curves

        [] ->
            []


animationFrame : Config a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame config model =
    let
        ( _, layer ) =
            getLayer editorState

        editorState =
            model.editorState
    in
    ( if Keyboard.pressedUndo config then
        case model.undoHistory of
            head :: rest ->
                { model
                    | undoHistory = rest
                    , editorState = head
                    , redoHistory = editorState :: model.redoHistory
                }

            [] ->
                model

      else if Keyboard.pressedRedo config then
        case model.redoHistory of
            head :: rest ->
                { model
                    | redoHistory = rest
                    , editorState = head
                    , undoHistory = editorState :: model.undoHistory
                }

            [] ->
                model

      else if Keyboard.keyPressed config Keyboard.Delete then
        if Set.isEmpty editorState.selectedNodes then
            model

        else
            case model.placingPoint of
                Just _ ->
                    { model | placingPoint = Nothing }

                Nothing ->
                    addEditorState
                        (setLayer
                            { layer
                                | path =
                                    List.indexedMap
                                        Tuple.pair
                                        layer.path
                                        |> List.filterMap
                                            (\( index, point ) ->
                                                if Set.member index editorState.selectedNodes then
                                                    Nothing

                                                else
                                                    Just point
                                            )
                            }
                            { editorState | selectedNodes = Set.empty }
                        )
                        model

      else
        model
    , Command.none
    )
        |> Tuple.mapFirst (updateMesh config model)


newPoint : Quantity Float units -> Quantity Float units -> Point2d units coordinates
newPoint (Quantity x) (Quantity y) =
    Point2d.unsafe { x = x, y = y }


screenToWorld : Config a -> Model -> Point2d Pixels ScreenCoordinate -> Point2d Meters WorldCoordinate
screenToWorld config model screenPosition =
    let
        camera : Camera3d Meters WorldCoordinate
        camera =
            MatchPage.camera model.cameraPosition model.viewportHeight

        screenRectangle : Rectangle2d Pixels ScreenCoordinate
        screenRectangle =
            Rectangle2d.from
                (newPoint Quantity.zero (Quantity.toFloatQuantity config.windowSize.height))
                (newPoint (Quantity.toFloatQuantity config.windowSize.width) Quantity.zero)
    in
    Camera3d.ray camera screenRectangle screenPosition
        |> Axis3d.originPoint
        |> (\p -> Point3d.toMeters p |> (\a -> Point2d.meters a.x a.y))


type DragType
    = CenterPoint
    | NextHandle
    | PreviousHandle


type alias Dragging =
    { offset : Vector2d Meters WorldCoordinate, dragType : DragType, index : Int }


isDragging : Config a -> Model -> Maybe Dragging
isDragging config model =
    case ( model.mouseDownAt, model.mousePosition ) of
        ( Just mouseDownAt, Just mousePosition ) ->
            let
                ( _, layer ) =
                    getLayer model.editorState

                uiScale_ =
                    uiScale model
            in
            List.indexedMap
                (\index segment ->
                    let
                        centerDistance =
                            Vector2d.from mouseDownAt segment.position |> Vector2d.length

                        nextDistance =
                            Vector2d.from
                                mouseDownAt
                                (Point2d.translateBy segment.handleNext segment.position)
                                |> Vector2d.length

                        previousDistance =
                            Vector2d.from
                                mouseDownAt
                                (Point2d.translateBy segment.handlePrevious segment.position)
                                |> Vector2d.length

                        offset =
                            Vector2d.from mouseDownAt (screenToWorld config model mousePosition)
                    in
                    [ if centerDistance |> Quantity.lessThan (Length.meters (7 * uiScale_)) then
                        { distance = centerDistance
                        , offset = offset
                        , dragType = CenterPoint
                        , index = index
                        }
                            |> Just

                      else
                        Nothing
                    , if nextDistance |> Quantity.lessThan (Length.meters (10 * uiScale_)) then
                        { distance = nextDistance
                        , offset = offset
                        , dragType = NextHandle
                        , index = index
                        }
                            |> Just

                      else
                        Nothing
                    , if previousDistance |> Quantity.lessThan (Length.meters (10 * uiScale_)) then
                        { distance = previousDistance
                        , offset = offset
                        , dragType = PreviousHandle
                        , index = index
                        }
                            |> Just

                      else
                        Nothing
                    ]
                        |> List.filterMap identity
                )
                layer.path
                |> List.concat
                |> Quantity.minimumBy .distance
                |> Maybe.map (\a -> { offset = a.offset, dragType = a.dragType, index = a.index })

        _ ->
            Nothing


handleMouseDown config model event =
    let
        ( x, y ) =
            event.pagePos

        screenPosition : Point2d Pixels ScreenCoordinate
        screenPosition =
            Point2d.pixels x y

        worldPosition =
            screenToWorld config model screenPosition
    in
    case event.button of
        Html.Events.Extra.Mouse.MainButton ->
            if x < layersViewWidth then
                model

            else
                { model
                    | mousePosition = Just screenPosition
                    , mouseDownAt = Just worldPosition
                }
                    |> (\model2 ->
                            case isDragging config model2 of
                                Just dragging ->
                                    let
                                        editorState =
                                            model2.editorState
                                    in
                                    replaceEditorState
                                        { editorState
                                            | selectedNodes =
                                                if Keyboard.keyDown config Keyboard.Shift then
                                                    if Set.member dragging.index editorState.selectedNodes then
                                                        Set.remove dragging.index editorState.selectedNodes

                                                    else
                                                        Set.insert dragging.index editorState.selectedNodes

                                                else
                                                    Set.singleton dragging.index
                                        }
                                        model2

                                Nothing ->
                                    startPathSegment config worldPosition model2
                       )

        Html.Events.Extra.Mouse.MiddleButton ->
            { model | mousePosition = Just screenPosition, wheelDownAt = Just worldPosition }

        _ ->
            model


update : Config a -> Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update config msg model =
    (case msg of
        NoOp ->
            ( model, Command.none )

        MouseDown event ->
            ( handleMouseDown config model event, Command.none )

        MouseUp event ->
            let
                ( x, y ) =
                    event.pagePos

                screenPosition : Point2d Pixels ScreenCoordinate
                screenPosition =
                    Point2d.pixels x y

                model2 =
                    { model
                        | mousePosition = Point2d.pixels x y |> Just
                        , mouseDownAt = Nothing
                    }
            in
            ( case event.button of
                Html.Events.Extra.Mouse.MainButton ->
                    case isDragging config model of
                        Just dragging ->
                            let
                                ( _, layer ) =
                                    getLayer model.editorState
                            in
                            addEditorState
                                (setLayer
                                    { layer
                                        | path =
                                            List.indexedMap
                                                (\index segment ->
                                                    dragSegment
                                                        config
                                                        index
                                                        (Just dragging)
                                                        True
                                                        model.editorState.selectedNodes
                                                        segment
                                                )
                                                layer.path
                                    }
                                    model.editorState
                                )
                                model2

                        Nothing ->
                            finishPathSegment (screenToWorld config model screenPosition) model2

                Html.Events.Extra.Mouse.MiddleButton ->
                    { model
                        | mousePosition = Point2d.pixels x y |> Just
                        , wheelDownAt = Nothing
                    }

                _ ->
                    model
            , Command.none
            )

        MouseMoved event ->
            if event.button == Html.Events.Extra.Mouse.MainButton then
                let
                    ( x, y ) =
                        event.pagePos
                in
                ( { model
                    | mousePosition = Point2d.pixels x y |> Just
                    , mousePositionPrevious = model.mousePosition
                    , cameraPosition =
                        case ( model.mousePosition, model.mousePositionPrevious, model.wheelDownAt ) of
                            ( Just mousePosition, Just mousePositionPrevious, Just _ ) ->
                                Point2d.translateBy
                                    (Vector2d.from
                                        (screenToWorld config model mousePosition)
                                        (screenToWorld config model mousePositionPrevious)
                                    )
                                    model.cameraPosition

                            _ ->
                                model.cameraPosition
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        MouseWheel event ->
            ( { model
                | viewportHeight =
                    Quantity.multiplyBy
                        (if event.deltaY > 0 then
                            1.2

                         else
                            1 / 1.2
                        )
                        model.viewportHeight
              }
            , Command.none
            )

        PressedLayer layerId ->
            let
                editorState =
                    model.editorState
            in
            ( replaceEditorState
                { editorState | currentLayer = layerId, selectedNodes = Set.empty }
                model
            , Command.none
            )

        PressedAddLayer ->
            ( addLayer initLayer model, Command.none )

        PressedRemoveLayer layerId ->
            let
                editorState =
                    model.editorState
            in
            ( addEditorState { editorState | layers = Dict.remove layerId editorState.layers } model
            , Command.none
            )

        TypedColor { red, green, blue } ->
            let
                ( _, layer ) =
                    getLayer model.editorState
            in
            ( addEditorState
                (setLayer { layer | red = red, green = green, blue = blue } model.editorState)
                model
            , Command.none
            )

        PressedSave ->
            ( model
            , Serialize.encodeToString Shape.codec { layers = model.editorState.layers }
                |> Ports.writeToClipboard
            )

        PressedMoveLayerUp layerId ->
            ( moveLayers True layerId model, Command.none )

        PressedMoveLayerDown layerId ->
            ( moveLayers False layerId model, Command.none )

        PressedDuplicate ->
            ( addLayer (getLayer model.editorState |> Tuple.second) model, Command.none )

        MouseLeft _ ->
            ( { model | mousePosition = Nothing }, Command.none )

        TypedLoadFromClipboard text ->
            ( case Serialize.decodeFromString Shape.codec text of
                Ok ok ->
                    addEditorState { initEditorState | layers = ok.layers } model

                Err _ ->
                    model
            , Command.none
            )

        PressedMirrorX ->
            let
                ( _, layer ) =
                    getLayer model.editorState

                selectedPoints =
                    List.indexedMap Tuple.pair layer.path
                        |> List.filterMap
                            (\( index, segment ) ->
                                if Set.member index model.editorState.selectedNodes then
                                    Vector2d.from Point2d.origin segment.position |> Just

                                else
                                    Nothing
                            )

                centerPoint : Point2d Meters WorldCoordinate
                centerPoint =
                    selectedPoints
                        |> Vector2d.sum
                        |> Vector2d.scaleBy (1 / toFloat (List.length selectedPoints))
                        |> (\v -> Point2d.translateBy v Point2d.origin)
            in
            ( addEditorState
                (setLayer
                    { layer
                        | path =
                            List.indexedMap
                                (\index segment ->
                                    if Set.member index model.editorState.selectedNodes then
                                        let
                                            axis =
                                                Axis2d.withDirection Direction2d.y centerPoint
                                        in
                                        { position = Point2d.mirrorAcross axis segment.position
                                        , handlePrevious = Vector2d.mirrorAcross axis segment.handlePrevious
                                        , handleNext = Vector2d.mirrorAcross axis segment.handleNext
                                        }

                                    else
                                        segment
                                )
                                layer.path
                    }
                    model.editorState
                )
                model
            , Command.none
            )
    )
        |> Tuple.mapFirst (updateMesh config model)


addLayer : Layer -> Model -> Model
addLayer layer model =
    let
        editorState =
            model.editorState

        layerId =
            Dict.keys model.editorState.layers
                |> List.maximumBy Id.toInt
                |> Maybe.withDefault (Id.fromInt 0)
                |> Id.increment
    in
    addEditorState
        { editorState
            | layers = Dict.insert layerId layer editorState.layers
            , currentLayer = layerId
            , selectedNodes = Set.empty
        }
        model


moveLayers : Bool -> Id LayerId -> Model -> Model
moveLayers moveUp layerId model =
    let
        editorState =
            model.editorState
    in
    case Dict.toList editorState.layers |> List.findIndex (Tuple.first >> (==) layerId) of
        Just index ->
            addEditorState
                { editorState
                    | layers =
                        Dict.toList editorState.layers
                            |> List.swapAt
                                index
                                (if moveUp then
                                    index - 1

                                 else
                                    index + 1
                                )
                            |> List.reverse
                            |> Dict.fromList
                }
                model

        Nothing ->
            model


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Command.none )


view : Config a -> Model -> Element Msg
view config model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , MatchPage.canvasView
            config.windowSize
            config.devicePixelRatio
            (canvasView model)
            |> Element.behindContent
        , Element.htmlAttribute
            (Html.Events.Extra.Mouse.onWithOptions
                "mousedown"
                { stopPropagation = False
                , preventDefault = False
                }
                MouseDown
            )
        , Element.htmlAttribute (Html.Events.Extra.Mouse.onUp MouseUp)
        , Element.htmlAttribute (Html.Events.Extra.Mouse.onMove MouseMoved)
        , Element.htmlAttribute (Html.Events.Extra.Mouse.onLeave MouseLeft)
        , Element.htmlAttribute (Html.Events.Extra.Wheel.onWheel MouseWheel)
        , toolView config model |> Element.inFront
        ]
        Element.none


toolView : Config a -> Model -> Element Msg
toolView config model =
    let
        ( _, layer ) =
            getLayer model.editorState
    in
    Element.column
        [ Element.width (Element.px layersViewWidth)
        , Element.height Element.fill
        , Element.Background.color (Element.rgb 1 1 1)
        , Element.Border.width 1
        , Element.spacing 4
        ]
        [ layersView model.editorState.currentLayer model.editorState.layers
        , Element.column
            [ Element.spacing 4, Element.padding 4 ]
            [ Element.Input.text
                [ Element.padding 4 ]
                { onChange =
                    \text ->
                        TypedColor
                            { red = String.toInt text |> Maybe.withDefault layer.red
                            , green = layer.green
                            , blue = layer.blue
                            }
                , text = String.fromInt layer.red
                , placeholder = Nothing
                , label = Element.Input.labelLeft [] (Element.text "R")
                }
            , Element.Input.text
                [ Element.padding 4 ]
                { onChange =
                    \text ->
                        TypedColor
                            { red = layer.red
                            , green = String.toInt text |> Maybe.withDefault layer.green
                            , blue = layer.blue
                            }
                , text = String.fromInt layer.green
                , placeholder = Nothing
                , label = Element.Input.labelLeft [] (Element.text "G")
                }
            , Element.Input.text
                [ Element.padding 4 ]
                { onChange =
                    \text ->
                        TypedColor
                            { red = layer.red
                            , green = layer.green
                            , blue = String.toInt text |> Maybe.withDefault layer.blue
                            }
                , text = String.fromInt layer.blue
                , placeholder = Nothing
                , label = Element.Input.labelLeft [] (Element.text "B")
                }
            ]
        , Ui.button buttonAttributes { onPress = PressedMirrorX, label = Element.text "Mirror X" }
        , Ui.button buttonAttributes { onPress = PressedSave, label = Element.text "Save to clipboard" }
        , Element.Input.text
            [ Element.padding 4 ]
            { onChange = TypedLoadFromClipboard
            , text = ""
            , placeholder = Element.Input.placeholder [] (Element.text "Load from clipboard") |> Just
            , label = Element.Input.labelHidden "Load from clipboard"
            }
        , case model.mousePosition of
            Just mousePosition ->
                let
                    { x, y } =
                        screenToWorld config model mousePosition |> Point2d.toMeters
                in
                String.fromInt (round x)
                    ++ ","
                    ++ String.fromInt (round y)
                    |> Element.text
                    |> Element.el [ Element.alignBottom ]

            Nothing ->
                Element.none
        ]


buttonAttributes =
    [ Element.padding 8
    , Element.width Element.fill
    , Element.Border.width 1
    ]


layersView : Id LayerId -> Dict (Id LayerId) Layer -> Element Msg
layersView currentLayer layers =
    List.map
        (\( layerId, _ ) ->
            Element.row
                [ Element.width Element.fill ]
                [ Ui.button
                    ((if currentLayer == layerId then
                        Element.Font.bold

                      else
                        Element.Font.regular
                     )
                        :: buttonAttributes
                    )
                    { onPress = PressedLayer layerId
                    , label = "Layer " ++ String.fromInt (Id.toInt layerId) |> Element.text
                    }
                , Ui.button
                    [ Element.padding 4
                    , Element.height Element.fill
                    , Element.Border.width 1
                    , Element.Background.color (Element.rgb 0.8 0.8 0.8)
                    ]
                    { onPress = PressedMoveLayerUp layerId
                    , label = Element.text "🡹"
                    }
                , Ui.button
                    [ Element.padding 4
                    , Element.height Element.fill
                    , Element.Border.width 1
                    , Element.Background.color (Element.rgb 0.8 0.8 0.8)
                    ]
                    { onPress = PressedMoveLayerDown layerId
                    , label = Element.text "🡻"
                    }
                , Ui.button
                    [ Element.padding 4
                    , Element.height Element.fill
                    , Element.Border.width 1
                    , Element.Background.color (Element.rgb 0.8 0.8 0.8)
                    , Element.Font.bold
                    ]
                    { onPress = PressedRemoveLayer layerId
                    , label = Element.text "X"
                    }
                ]
        )
        (Dict.toList layers)
        ++ [ Ui.button
                buttonAttributes
                { onPress = PressedAddLayer
                , label = Element.text "Add layer"
                }
           , Ui.button
                buttonAttributes
                { onPress = PressedDuplicate
                , label = Element.text "Duplicate"
                }
           ]
        |> Element.column
            [ Element.width Element.fill
            , Element.Border.width 1
            ]


layersViewWidth : number
layersViewWidth =
    190


canvasView : Model -> Size -> List Entity
canvasView model canvasSize =
    let
        viewMatrix : Mat4
        viewMatrix =
            WebGL.Matrices.viewProjectionMatrix
                (MatchPage.camera model.cameraPosition model.viewportHeight)
                { nearClipDepth = Length.meters 0.1
                , farClipDepth = Length.meters 10
                , aspectRatio =
                    Quantity.ratio
                        (Quantity.toFloatQuantity canvasSize.width)
                        (Quantity.toFloatQuantity canvasSize.height)
                }
    in
    [ MatchPage.backgroundGrid model.cameraPosition (1 / Length.inMeters model.viewportHeight) canvasSize ]
        ++ List.concatMap
            (\( layerId, layer ) ->
                case Dict.get layerId model.meshCache of
                    Just cache ->
                        FontRender.drawLayer
                            (Math.Vector3.vec3
                                (toFloat layer.red / 255)
                                (toFloat layer.green / 255)
                                (toFloat layer.blue / 255)
                            )
                            cache.pathFillMesh
                            Mat4.identity
                            viewMatrix
                            ++ (if layerId == model.editorState.currentLayer then
                                    [ WebGL.entityWith
                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                        MatchPage.vertexShader
                                        MatchPage.fragmentShader
                                        cache.pathMesh
                                        { view = viewMatrix
                                        , model = Mat4.identity
                                        }
                                    ]

                                else
                                    []
                               )

                    Nothing ->
                        []
            )
            (Dict.toList model.editorState.layers)



--++ List.concatMap
--    (\segment ->
--        let
--            position =
--                Point2d.toMeters segment.position
--
--            handlePrevious =
--                Point2d.translateBy segment.handlePrevious segment.position
--                    |> Point2d.toMeters
--
--            handleNext =
--                Point2d.translateBy segment.handleNext segment.position
--                    |> Point2d.toMeters
--
--            size =
--                6
--        in
--        [ WebGL.entityWith
--            [ WebGL.Settings.cullFace WebGL.Settings.back ]
--            MatchPage.vertexShader
--            MatchPage.fragmentShader
--            squareMesh
--            { view = viewMatrix
--            , model = Mat4.makeTranslate3 position.x position.y 0 |> Mat4.scale3 size size 1
--            }
--        , WebGL.entityWith
--            [ WebGL.Settings.cullFace WebGL.Settings.back ]
--            MatchPage.vertexShader
--            MatchPage.fragmentShader
--            squareMesh
--            { view = viewMatrix
--            , model =
--                Mat4.makeTranslate3 handlePrevious.x handlePrevious.y 0
--                    |> Mat4.scale3 size size 1
--            }
--        , WebGL.entityWith
--            [ WebGL.Settings.cullFace WebGL.Settings.back ]
--            MatchPage.vertexShader
--            MatchPage.fragmentShader
--            squareMesh
--            { view = viewMatrix
--            , model = Mat4.makeTranslate3 handleNext.x handleNext.y 0 |> Mat4.scale3 size size 1
--            }
--        , WebGL.entityWith
--            [ WebGL.Settings.cullFace WebGL.Settings.back ]
--            MatchPage.vertexShader
--            MatchPage.fragmentShader
--            (MatchPage.lineMesh
--                (Length.meters 3)
--                (Math.Vector3.vec3 0 0 0)
--                (LineSegment2d.from
--                    segment.position
--                    (Point2d.translateBy segment.handlePrevious segment.position)
--                )
--                |> WebGL.triangles
--            )
--            { view = viewMatrix
--            , model = Mat4.identity
--            }
--        , WebGL.entityWith
--            [ WebGL.Settings.cullFace WebGL.Settings.back ]
--            MatchPage.vertexShader
--            MatchPage.fragmentShader
--            (MatchPage.lineMesh
--                (Length.meters 3)
--                (Math.Vector3.vec3 0 0 0)
--                (LineSegment2d.from
--                    segment.position
--                    (Point2d.translateBy segment.handleNext segment.position)
--                )
--                |> WebGL.triangles
--            )
--            { view = viewMatrix
--            , model = Mat4.identity
--            }
--        ]
--    )
--    model.path


squareMesh : WebGL.Mesh Vertex
squareMesh =
    WebGL.triangleFan
        [ { position = Math.Vector2.vec2 -1 -1, color = Math.Vector3.vec3 0 0 0 }
        , { position = Math.Vector2.vec2 1 -1, color = Math.Vector3.vec3 0 0 0 }
        , { position = Math.Vector2.vec2 1 1, color = Math.Vector3.vec3 0 0 0 }
        , { position = Math.Vector2.vec2 -1 1, color = Math.Vector3.vec3 0 0 0 }
        ]



--++ FontRender.drawLayer (Math.Vector4.vec4 1 0 0.5 1) mesh viewMatrix
--++ [ WebGL.entityWith
--        []
--        FontRender.vertexShaderFont
--        FontRender.fragmentShaderFont
--        mesh
--        { viewMatrix = viewMatrix
--        , color = Math.Vector4.vec4 1 0 0.5 1
--        }
--   ]
