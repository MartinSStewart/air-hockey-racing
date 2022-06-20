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

import Collision
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.WebGL as WebGL exposing (Entity, Mesh)
import Element exposing (Element)
import Html.Events.Extra.Mouse exposing (Event)
import Length exposing (Meters)
import Match exposing (WorldCoordinate)
import MatchPage exposing (ScreenCoordinate, Vertex, WorldPixel)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Ui exposing (Size)
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
    , path : List (Point2d Meters WorldCoordinate)
    , pathMesh : Mesh Vertex
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
    , pathMesh = MatchPage.lineSegmentMesh (Math.Vector3.vec3 1 0.1 0.1) []
    }


addPath : Point2d Meters WorldCoordinate -> Model -> Model
addPath point model =
    let
        newPath =
            point :: model.path
    in
    { model
        | path = newPath
        , pathMesh =
            Collision.pointsToLineSegments newPath
                |> MatchPage.lineSegmentMesh (Math.Vector3.vec3 1 0.1 0.1)
    }


animationFrame : a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame config model =
    ( model, Command.none )


update : Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Command.none )

        MouseDown event ->
            ( model, Command.none )

        MouseUp event ->
            ( model, Command.none )

        MouseMoved event ->
            ( model, Command.none )


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
        zoom =
            1 / 2000

        viewMatrix : Mat4
        viewMatrix =
            WebGL.Matrices.viewProjectionMatrix
                (MatchPage.camera model.cameraPosition (Length.meters (1 / zoom)))
                { nearClipDepth = Length.meters 0.1
                , farClipDepth = Length.meters 10
                , aspectRatio =
                    Quantity.ratio
                        (Quantity.toFloatQuantity canvasSize.width)
                        (Quantity.toFloatQuantity canvasSize.height)
                }
    in
    [ MatchPage.backgroundGrid model.cameraPosition zoom canvasSize
    , WebGL.entityWith
        [ WebGL.Settings.cullFace WebGL.Settings.back ]
        MatchPage.vertexShader
        MatchPage.fragmentShader
        model.pathMesh
        { view = viewMatrix
        , model = Mat4.identity
        }
    ]
