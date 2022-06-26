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

import Axis2d
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
import Length exposing (Length, Meters)
import List.Nonempty
import Match exposing (WorldCoordinate)
import MatchPage exposing (ScreenCoordinate, Vertex, WorldPixel)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
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
    , mouseDownAt : Maybe (Point2d Pixels ScreenCoordinate)
    , cameraPosition : Point2d Meters WorldCoordinate
    , path : List PathSegment
    , nextPathSegment : NextPathSegment
    , pathMesh : Mesh Vertex
    , viewportHeight : Length
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
    }


init : Model
init =
    { mousePosition = Nothing
    , mouseDownAt = Nothing
    , cameraPosition = Point2d.origin
    , path = []
    , nextPathSegment = NoPathSegment
    , pathMesh = MatchPage.lineSegmentMesh (Math.Vector3.vec3 1 0.1 0.1) []
    , viewportHeight = Length.meters 2000
    }


startPathSegment : Point2d Meters WorldCoordinate -> Model -> Model
startPathSegment point model =
    case model.nextPathSegment of
        NoPathSegment ->
            { model
                | nextPathSegment = PlacingHandles point
                , pathMesh = pathToMesh NoPathSegment model.path
            }

        PlacingHandles _ ->
            model


finishPathSegment : Point2d Meters WorldCoordinate -> Model -> Model
finishPathSegment point model =
    case model.nextPathSegment of
        NoPathSegment ->
            model

        PlacingHandles position ->
            let
                newPath =
                    { position = position
                    , handlePrevious = Vector2d.from position point
                    , handleNext = Vector2d.from point position
                    }
                        :: model.path
            in
            { model
                | path = newPath
                , nextPathSegment = NoPathSegment
                , pathMesh = pathToMesh NoPathSegment newPath
            }


pathToMesh : NextPathSegment -> List PathSegment -> WebGL.Mesh Vertex
pathToMesh nextPathSegment pathSegments =
    (case nextPathSegment of
        NoPathSegment ->
            []

        PlacingHandles position ->
            [ position ]
    )
        ++ List.map .position pathSegments
        |> Collision.pointsToLineSegments
        |> MatchPage.lineSegmentMesh (Math.Vector3.vec3 1 0 0)


animationFrame : Config a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame config model =
    ( model, Command.none )


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


update : Config a -> Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update config msg model =
    case msg of
        NoOp ->
            ( model, Command.none )

        MouseDown event ->
            let
                ( x, y ) =
                    event.pagePos

                screenPosition : Point2d Pixels ScreenCoordinate
                screenPosition =
                    Point2d.pixels x y
            in
            ( { model
                | mousePosition = Just screenPosition
                , mouseDownAt = Just screenPosition
              }
                |> startPathSegment (screenToWorld config model screenPosition)
            , Command.none
            )

        MouseUp event ->
            let
                ( x, y ) =
                    event.pagePos

                screenPosition : Point2d Pixels ScreenCoordinate
                screenPosition =
                    Point2d.pixels x y
            in
            ( { model
                | mousePosition = Point2d.pixels x y |> Just
                , mouseDownAt = Nothing
              }
                |> finishPathSegment (screenToWorld config model screenPosition)
            , Command.none
            )

        MouseMoved event ->
            let
                ( x, y ) =
                    event.pagePos
            in
            ( { model | mousePosition = Point2d.pixels x y |> Just }, Command.none )


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
    [ MatchPage.backgroundGrid model.cameraPosition (1 / Length.inMeters model.viewportHeight) canvasSize
    , WebGL.entityWith
        [ WebGL.Settings.cullFace WebGL.Settings.back ]
        MatchPage.vertexShader
        MatchPage.fragmentShader
        model.pathMesh
        { view = viewMatrix
        , model = Mat4.identity
        }
    ]
        ++ FontRender.drawLayer (Math.Vector4.vec4 1 0 0.5 1) mesh viewMatrix



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



--bezierExample : List { position : Point2d Meters coordinates, controlPoint : Point2d Meters coordinates }
--bezierExample =
--    [ CubicSpline2d.fromControlPoints
--        Point2d.origin
--        (Point2d.meters -100 150)
--        (Point2d.meters -150 200)
--        (Point2d.meters 0 300)
--    , CubicSpline2d.fromControlPoints
--        (Point2d.meters 0 300)
--        (Point2d.meters 100 -150)
--        (Point2d.meters 150 400)
--        Point2d.origin
--    ]
--        |> List.concatMap Collision.cubicSplineToQuadratic
--        |> Debug.log ""
--        |> List.map
--            (\spline ->
--                { position = QuadraticSpline2d.startPoint spline
--                , controlPoint = QuadraticSpline2d.secondControlPoint spline
--                }
--            )


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
        |> List.concatMap (Collision.cubicSplineToQuadratic (Length.meters 3) >> List.Nonempty.toList)
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
