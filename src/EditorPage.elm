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

import Axis3d
import Camera3d exposing (Camera3d)
import Collision
import CubicSpline2d
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.WebGL as WebGL exposing (Entity, Mesh)
import Element exposing (Element)
import FontRender exposing (FontVertex)
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Geometry.Types exposing (Rectangle2d(..))
import Html.Events.Extra.Mouse exposing (Event)
import Keyboard
import Length exposing (Length, Meters)
import LineSegment2d
import List.Extra as List
import List.Nonempty
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
import Vector2d exposing (Vector2d)
import WebGL.Matrices
import WebGL.Settings


type Msg
    = NoOp
    | MouseDown Event
    | MouseUp Event
    | MouseMoved Event


type alias Model =
    { mousePosition : Maybe (Point2d Pixels ScreenCoordinate)
    , mouseDownAt : Maybe (Point2d Meters WorldCoordinate)
    , cameraPosition : Point2d Meters WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , pathMesh : Mesh Vertex
    , pathFillMesh : Mesh FontVertex
    , viewportHeight : Length
    }


type alias EditorState =
    { path : List PathSegment
    , nextPathSegment : NextPathSegment
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
    , mouseDownAt = Nothing
    , cameraPosition = Point2d.origin
    , undoHistory = []
    , editorState = { path = [], nextPathSegment = NoPathSegment }
    , redoHistory = []
    , pathMesh = WebGL.triangles []
    , pathFillMesh = shapeToMesh_ []
    , viewportHeight = Length.meters 2000
    }


startPathSegment : Config a -> Point2d Meters WorldCoordinate -> Model -> Model
startPathSegment config point model =
    case model.editorState.nextPathSegment of
        NoPathSegment ->
            let
                editorState =
                    model.editorState
            in
            addEditorState { editorState | nextPathSegment = PlacingHandles point } model

        PlacingHandles _ ->
            model


fullPath : Config a -> Model -> List PathSegment
fullPath config model =
    (case model.editorState.nextPathSegment of
        NoPathSegment ->
            []

        PlacingHandles position ->
            let
                mouseWorld =
                    case ( model.mouseDownAt, model.mousePosition ) of
                        ( Just mouseDownAt, Just mousePosition ) ->
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
        ++ model.editorState.path


finishPathSegment : Config a -> Point2d Meters WorldCoordinate -> Model -> Model
finishPathSegment config point model =
    case model.editorState.nextPathSegment of
        NoPathSegment ->
            model

        PlacingHandles position ->
            replaceEditorState
                { path =
                    { position = position
                    , handlePrevious = Vector2d.from position point
                    , handleNext = Vector2d.from point position
                    }
                        :: model.editorState.path
                , nextPathSegment = NoPathSegment
                }
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
        fullPath_ =
            fullPath config model

        maybeDragging =
            isDragging config model
    in
    if model.editorState == previousModel.editorState && model.mousePosition == previousModel.mousePosition then
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
                                6

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
                                (Length.meters 2)
                                (Math.Vector3.vec3 0 0 0)
                                (LineSegment2d.from
                                    segment2.position
                                    (Point2d.translateBy segment2.handleNext segment2.position)
                                )
                            ++ MatchPage.lineMesh
                                (Length.meters 2)
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
                model.editorState.path
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
            ( { model
                | mousePosition = Just screenPosition
                , mouseDownAt = Just worldPosition
              }
                |> (\model2 ->
                        case isDragging config model2 of
                            Just _ ->
                                model2

                            Nothing ->
                                startPathSegment config worldPosition model2
                   )
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
            ( case isDragging config model of
                Just dragging ->
                    let
                        editorState =
                            model.editorState
                    in
                    addEditorState
                        { editorState
                            | path =
                                List.updateAt
                                    dragging.index
                                    (\segment -> dragSegment dragging.index (Just dragging) segment)
                                    model.editorState.path
                        }
                        model2

                Nothing ->
                    finishPathSegment config (screenToWorld config model screenPosition) model2
            , Command.none
            )

        MouseMoved event ->
            let
                ( x, y ) =
                    event.pagePos
            in
            ( { model | mousePosition = Point2d.pixels x y |> Just }
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
        ]
        Element.none


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
