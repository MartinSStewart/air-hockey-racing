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
import Axis3d
import Camera3d exposing (Camera3d)
import Collision
import CubicSpline2d
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.WebGL as WebGL exposing (Entity, Mesh)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import FontRender exposing (FontVertex)
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Geometry.Types exposing (Rectangle2d(..))
import Html.Events.Extra.Mouse exposing (Event)
import Html.Events.Extra.Wheel
import Id exposing (Id)
import Keyboard
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
import QuadraticSpline2d
import Quantity exposing (Quantity(..), Rate)
import Rectangle2d
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
    | MouseWheel Html.Events.Extra.Wheel.Event
    | PressedLayer (Id LayerId)
    | PressedAddLayer
    | PressedRemoveLayer (Id LayerId)


type alias Model =
    { mousePosition : Maybe (Point2d Pixels ScreenCoordinate)
    , mousePositionPrevious : Maybe (Point2d Pixels ScreenCoordinate)
    , mouseDownAt : Maybe (Point2d Meters WorldCoordinate)
    , wheelDownAt : Maybe (Point2d Meters WorldCoordinate)
    , cameraPosition : Point2d Meters WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , pathMesh : Mesh Vertex
    , pathFillMesh : Mesh FontVertex
    , viewportHeight : Length
    }


type LayerId
    = LayerId Never


type alias EditorState =
    { layers : Dict (Id LayerId) Layer
    , currentLayer : Id LayerId
    }


type alias Layer =
    { path : List PathSegment
    , nextPathSegment : NextPathSegment
    , red : Int
    , green : Int
    , blue : Int
    }


initLayer : Layer
initLayer =
    { path = []
    , nextPathSegment = NoPathSegment
    , red = 255
    , green = 0
    , blue = 100
    }


type NextPathSegment
    = NoPathSegment
    | PlacingHandles (Point2d Meters WorldCoordinate)


type alias PathSegment =
    { position : Point2d Meters WorldCoordinate
    , handlePrevious : Vector2d Meters WorldCoordinate
    , handleNext : Vector2d Meters WorldCoordinate
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
    , editorState =
        { layers = Dict.fromList [ ( Id.fromInt 0, initLayer ) ]
        , currentLayer = Id.fromInt 0
        }
    , redoHistory = []
    , pathMesh = WebGL.triangles []
    , pathFillMesh = shapeToMesh_ []
    , viewportHeight = Length.meters 2000
    }


setNonempty : Int -> a -> Nonempty a -> Nonempty a
setNonempty index value nonempty =
    List.Nonempty.toList nonempty
        |> List.setAt index value
        |> List.Nonempty.fromList
        |> Maybe.withDefault nonempty


getLayer : EditorState -> Layer
getLayer editorState =
    case Dict.get editorState.currentLayer editorState.layers of
        Just layer ->
            layer

        Nothing ->
            Dict.values editorState.layers |> List.head |> Maybe.withDefault initLayer


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


startPathSegment : Point2d Meters WorldCoordinate -> Model -> Model
startPathSegment point model =
    let
        layer =
            getLayer model.editorState
    in
    case layer.nextPathSegment of
        NoPathSegment ->
            addEditorState
                (setLayer { layer | nextPathSegment = PlacingHandles point } model.editorState)
                model

        PlacingHandles _ ->
            model


fullPath : Config a -> Layer -> Bool -> Model -> List PathSegment
fullPath config layer isCurrentLayer model =
    (case layer.nextPathSegment of
        NoPathSegment ->
            []

        PlacingHandles position ->
            let
                mouseWorld =
                    case ( isCurrentLayer, model.mouseDownAt, model.mousePosition ) of
                        ( True, Just mouseDownAt, Just mousePosition ) ->
                            Vector2d.from (screenToWorld config model mousePosition) mouseDownAt

                        _ ->
                            Vector2d.zero
            in
            [ { position = position
              , handleNext = mouseWorld
              , handlePrevious = Vector2d.reverse mouseWorld
              }
            ]
    )
        ++ layer.path


finishPathSegment : Config a -> Point2d Meters WorldCoordinate -> Model -> Model
finishPathSegment config point model =
    let
        layer =
            getLayer model.editorState
    in
    case layer.nextPathSegment of
        NoPathSegment ->
            model

        PlacingHandles position ->
            replaceEditorState
                (setLayer
                    { layer
                        | path =
                            { position = position
                            , handlePrevious = Vector2d.from position point
                            , handleNext = Vector2d.from point position
                            }
                                :: layer.path
                        , nextPathSegment = NoPathSegment
                    }
                    model.editorState
                )
                model


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


updateMesh : Config a -> Model -> Model -> Model
updateMesh config previousModel model =
    let
        layer =
            getLayer model.editorState

        fullPath_ =
            fullPath config layer True model

        maybeDragging =
            isDragging config model

        uiScale =
            Quantity.ratio model.viewportHeight (Length.meters 800)
    in
    if
        (model.editorState == previousModel.editorState)
            && (model.mousePosition == previousModel.mousePosition)
            && (model.viewportHeight == previousModel.viewportHeight)
    then
        model

    else
        { model
            | pathFillMesh = pathToFillMesh maybeDragging fullPath_
            , pathMesh =
                List.indexedMap
                    (\index segment ->
                        let
                            segment2 =
                                dragSegment index maybeDragging segment

                            handlePrevious =
                                Point2d.translateBy segment2.handlePrevious segment2.position

                            handleNext =
                                Point2d.translateBy segment2.handleNext segment2.position

                            size =
                                3 * uiScale

                            drawSquare p =
                                let
                                    { x, y } =
                                        Point2d.toMeters p
                                in
                                [ ( { position = Math.Vector2.vec2 (x - size) (y - size)
                                    , color = Math.Vector3.vec3 0 0 0
                                    }
                                  , { position = Math.Vector2.vec2 (x + size) (y - size)
                                    , color = Math.Vector3.vec3 0 0 0
                                    }
                                  , { position = Math.Vector2.vec2 (x + size) (y + size)
                                    , color = Math.Vector3.vec3 0 0 0
                                    }
                                  )
                                , ( { position = Math.Vector2.vec2 (x - size) (y - size)
                                    , color = Math.Vector3.vec3 0 0 0
                                    }
                                  , { position = Math.Vector2.vec2 (x + size) (y + size)
                                    , color = Math.Vector3.vec3 0 0 0
                                    }
                                  , { position = Math.Vector2.vec2 (x - size) (y + size)
                                    , color = Math.Vector3.vec3 0 0 0
                                    }
                                  )
                                ]
                        in
                        drawSquare segment2.position
                            ++ drawSquare handlePrevious
                            ++ drawSquare handleNext
                            ++ MatchPage.lineMesh
                                (Length.meters uiScale)
                                (Math.Vector3.vec3 0 0 0)
                                (LineSegment2d.from
                                    segment2.position
                                    (Point2d.translateBy segment2.handleNext segment2.position)
                                )
                            ++ MatchPage.lineMesh
                                (Length.meters uiScale)
                                (Math.Vector3.vec3 0 0 0)
                                (LineSegment2d.from
                                    segment2.position
                                    (Point2d.translateBy segment2.handlePrevious segment2.position)
                                )
                    )
                    fullPath_
                    |> List.concat
                    |> WebGL.triangles
        }


dragSegment : Int -> Maybe Dragging -> PathSegment -> PathSegment
dragSegment index maybeDragging pathSegment =
    case maybeDragging of
        Just dragging ->
            if dragging.index == index then
                case dragging.dragType of
                    CenterPoint ->
                        { pathSegment
                            | position = Point2d.translateBy dragging.offset pathSegment.position
                        }

                    NextHandle ->
                        { pathSegment
                            | handleNext = Vector2d.plus dragging.offset pathSegment.handleNext
                        }

                    PreviousHandle ->
                        { pathSegment
                            | handlePrevious = Vector2d.plus dragging.offset pathSegment.handlePrevious
                        }

            else
                pathSegment

        Nothing ->
            pathSegment


pathToFillMesh : Maybe Dragging -> List PathSegment -> Mesh FontVertex
pathToFillMesh maybeDragging path =
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
                            dragSegment (modBy pathLength state.index) maybeDragging segment
                    in
                    { index = state.index + 1
                    , previousPoint = segment2
                    , curves =
                        state.curves
                            ++ (Collision.cubicSplineToQuadratic
                                    (Length.meters 2)
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
                               )
                    }
                )
                { index = 1, previousPoint = dragSegment 0 maybeDragging first, curves = [] }
                (rest ++ [ first ])
                |> .curves
                |> List.map
                    (\spline ->
                        { position = QuadraticSpline2d.startPoint spline
                        , controlPoint = QuadraticSpline2d.secondControlPoint spline
                        }
                    )
                |> shapeToMesh_

        [] ->
            shapeToMesh_ []


animationFrame : Config a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame config model =
    ( if pressedUndo config then
        case model.undoHistory of
            head :: rest ->
                { model
                    | undoHistory = rest
                    , editorState = head
                    , redoHistory = model.editorState :: model.redoHistory
                }

            [] ->
                model

      else if pressedRedo config then
        case model.redoHistory of
            head :: rest ->
                { model
                    | redoHistory = rest
                    , editorState = head
                    , undoHistory = model.editorState :: model.undoHistory
                }

            [] ->
                model

      else
        model
    , Command.none
    )
        |> Tuple.mapFirst (updateMesh config model)


pressedUndo : Config a -> Bool
pressedUndo config =
    keyPressed config (Keyboard.Character "Z")
        && not (keyDown config Keyboard.Shift)
        && (keyDown config Keyboard.Control || keyDown config Keyboard.Meta)


pressedRedo : Config a -> Bool
pressedRedo config =
    (keyPressed config (Keyboard.Character "Z")
        && keyDown config Keyboard.Shift
        && (keyDown config Keyboard.Control || keyDown config Keyboard.Meta)
    )
        || (keyPressed config (Keyboard.Character "Y")
                && (keyDown config Keyboard.Control || keyDown config Keyboard.Meta)
           )


keyPressed : Config a -> Keyboard.Key -> Bool
keyPressed config key =
    List.any ((==) key) config.currentKeys && not (List.any ((==) key) config.previousKeys)


keyDown : Config a -> Keyboard.Key -> Bool
keyDown config key =
    List.any ((==) key) config.currentKeys


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
                layer =
                    getLayer model.editorState
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
                    [ if centerDistance |> Quantity.lessThan (Length.meters 20) then
                        { distance = centerDistance
                        , offset = offset
                        , dragType = CenterPoint
                        , index = index
                        }
                            |> Just

                      else
                        Nothing
                    , if nextDistance |> Quantity.lessThan (Length.meters 30) then
                        { distance = nextDistance
                        , offset = offset
                        , dragType = NextHandle
                        , index = index
                        }
                            |> Just

                      else
                        Nothing
                    , if previousDistance |> Quantity.lessThan (Length.meters 30) then
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


update : Config a -> Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update config msg model =
    (case msg of
        NoOp ->
            ( model, Command.none )

        MouseDown event ->
            let
                ( x, y ) =
                    event.pagePos

                screenPosition : Point2d Pixels ScreenCoordinate
                screenPosition =
                    Point2d.pixels x y

                worldPosition =
                    screenToWorld config model screenPosition
            in
            ( case event.button of
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
                                        Just _ ->
                                            model2

                                        Nothing ->
                                            startPathSegment worldPosition model2
                               )

                Html.Events.Extra.Mouse.MiddleButton ->
                    { model | mousePosition = Just screenPosition, wheelDownAt = Just worldPosition }

                _ ->
                    model
            , Command.none
            )

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
                                layer =
                                    getLayer model.editorState
                            in
                            addEditorState
                                (setLayer
                                    { layer
                                        | path =
                                            List.updateAt
                                                dragging.index
                                                (\segment -> dragSegment dragging.index (Just dragging) segment)
                                                layer.path
                                    }
                                    model.editorState
                                )
                                model2

                        Nothing ->
                            finishPathSegment config (screenToWorld config model screenPosition) model2

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
            ( replaceEditorState { editorState | currentLayer = layerId } model
            , Command.none
            )

        PressedAddLayer ->
            let
                editorState =
                    model.editorState

                layerId =
                    Dict.keys model.editorState.layers |> List.maximumBy Id.toInt |> Maybe.withDefault (Id.fromInt 0) |> Id.increment
            in
            ( addEditorState
                { editorState
                    | layers = Dict.insert layerId initLayer editorState.layers
                    , currentLayer = layerId
                }
                model
            , Command.none
            )

        PressedRemoveLayer layerId ->
            let
                editorState =
                    model.editorState
            in
            ( addEditorState { editorState | layers = Dict.remove layerId editorState.layers } model
            , Command.none
            )
    )
        |> Tuple.mapFirst (updateMesh config model)


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
        , Element.htmlAttribute (Html.Events.Extra.Mouse.onDown MouseDown)
        , Element.htmlAttribute (Html.Events.Extra.Mouse.onUp MouseUp)
        , Element.htmlAttribute (Html.Events.Extra.Mouse.onMove MouseMoved)
        , Element.htmlAttribute (Html.Events.Extra.Wheel.onWheel MouseWheel)
        , layersView model.editorState.currentLayer model.editorState.layers |> Element.inFront
        ]
        Element.none


layersView : Id LayerId -> Dict (Id LayerId) Layer -> Element Msg
layersView currentLayer layers =
    List.map
        (\( layerId, layer ) ->
            Ui.button
                [ Element.padding 8
                , Element.width Element.fill
                , Element.Border.width 1
                , if currentLayer == layerId then
                    Element.Font.bold

                  else
                    Element.Font.regular
                ]
                { onPress = PressedLayer layerId
                , label = "Layer " ++ String.fromInt (Id.toInt layerId) |> Element.text
                }
        )
        (Dict.toList layers)
        ++ [ Ui.button
                [ Element.padding 8
                , Element.width Element.fill
                , Element.Background.color (Element.rgb 1 1 1)
                , Element.Border.width 1
                ]
                { onPress = PressedAddLayer
                , label = Element.text "Add layer"
                }
           ]
        |> Element.column
            [ Element.width (Element.px layersViewWidth)
            , Element.height Element.fill
            , Element.Background.color (Element.rgb 1 1 1)
            , Element.Border.width 1
            ]


layersViewWidth =
    140


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
        ++ FontRender.drawLayer (Math.Vector4.vec4 1 0 0.5 1) model.pathFillMesh viewMatrix
        ++ [ WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                MatchPage.vertexShader
                MatchPage.fragmentShader
                model.pathMesh
                { view = viewMatrix
                , model = Mat4.identity
                }
           ]



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


mesh : Mesh FontVertex
mesh =
    shapeToMesh_ bezierExample


bezierExample : List { position : Point2d Meters coordinates, controlPoint : Point2d Meters coordinates }
bezierExample =
    [ CubicSpline2d.fromControlPoints
        Point2d.origin
        (Point2d.meters -200 300)
        (Point2d.meters -300 400)
        (Point2d.meters 0 300)
    , CubicSpline2d.fromControlPoints
        (Point2d.meters 0 300)
        (Point2d.meters 200 -300)
        (Point2d.meters 300 800)
        Point2d.origin
    ]
        |> List.concatMap (Collision.cubicSplineToQuadratic (Length.meters 2) >> List.Nonempty.toList)
        |> List.map
            (\spline ->
                { position = QuadraticSpline2d.startPoint spline
                , controlPoint = QuadraticSpline2d.secondControlPoint spline
                }
            )


shapeExample =
    [ { position = Point2d.origin, controlPoint = Point2d.meters -200 300 }
    , { position = Point2d.meters 0 600, controlPoint = Point2d.meters 380 380 }
    , { position = Point2d.meters 600 0, controlPoint = Point2d.meters 300 200 }
    ]


shapeToMesh_ :
    List { position : Point2d units coordinates, controlPoint : Point2d units coordinates }
    -> Mesh FontVertex
shapeToMesh_ path =
    (case List.map (.position >> Point2d.toVec2) path of
        first :: second :: rest ->
            List.foldl
                (\point state ->
                    { first = state.first
                    , previous = point
                    , triangles =
                        ( { position = point, s = 0.2, t = 0.2 }
                        , { position = state.previous, s = 0.2, t = 0.2 }
                        , { position = state.first, s = 0.2, t = 0.2 }
                        )
                            :: state.triangles
                    }
                )
                { first = first, previous = second, triangles = [] }
                rest
                |> .triangles

        _ ->
            []
    )
        ++ (case path of
                first :: rest ->
                    List.foldl
                        (\point state ->
                            { previous = point
                            , triangles =
                                ( { position = Point2d.toVec2 point.position
                                  , s = 0
                                  , t = 0
                                  }
                                , { position = Point2d.toVec2 state.previous.position
                                  , s = 0
                                  , t = 1
                                  }
                                , { position = Point2d.toVec2 state.previous.controlPoint
                                  , s = 1
                                  , t = 0
                                  }
                                )
                                    :: state.triangles
                            }
                        )
                        { previous = first, triangles = [] }
                        (rest ++ [ first ])
                        |> .triangles

                [] ->
                    []
           )
        |> WebGL.triangles
