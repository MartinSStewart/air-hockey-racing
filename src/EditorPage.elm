module EditorPage exposing
    ( Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , animationFrame
    , update
    , updateFromBackend
    , view
    )

import Effect.Command as Command exposing (Command, FrontendOnly)
import Element exposing (Element)
import Match exposing (WorldCoordinate)
import MatchPage exposing (ScreenCoordinate)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)


type Msg
    = NoOp


type alias Model =
    { mousePosition : Maybe (Point2d Pixels ScreenCoordinate)
    , mouseDownAt : Maybe (Point2d Pixels ScreenCoordinate)
    , cameraPosition : Point2d Pixels WorldCoordinate
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend


animationFrame : a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame config model =
    ( model, Command.none )


update : Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Command.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Command.none )


view : Model -> Element msg
view model =
    Element.none
