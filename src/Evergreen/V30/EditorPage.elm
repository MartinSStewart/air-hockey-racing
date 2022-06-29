module Evergreen.V30.EditorPage exposing (..)

import AssocList
import AssocSet
import Effect.WebGL
import Evergreen.V30.FontRender
import Evergreen.V30.Id
import Evergreen.V30.Match
import Evergreen.V30.MatchPage
import Evergreen.V30.Point2d
import Evergreen.V30.Shape
import Html.Events.Extra.Mouse
import Html.Events.Extra.Wheel
import Length
import Pixels


type Msg
    = MouseDown Html.Events.Extra.Mouse.Event
    | MouseUp Html.Events.Extra.Mouse.Event
    | MouseMoved Html.Events.Extra.Mouse.Event
    | MouseLeft Html.Events.Extra.Mouse.Event
    | MouseWheel Html.Events.Extra.Wheel.Event
    | PressedLayer (Evergreen.V30.Id.Id Evergreen.V30.Shape.LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Evergreen.V30.Id.Id Evergreen.V30.Shape.LayerId)
    | TypedColor
        { red : Int
        , green : Int
        , blue : Int
        }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Evergreen.V30.Id.Id Evergreen.V30.Shape.LayerId)
    | PressedMoveLayerDown (Evergreen.V30.Id.Id Evergreen.V30.Shape.LayerId)
    | PressedMirrorX


type alias EditorState =
    { layers : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.Shape.LayerId) Evergreen.V30.Shape.Layer
    , currentLayer : Evergreen.V30.Id.Id Evergreen.V30.Shape.LayerId
    , selectedNodes : AssocSet.Set Int
    }


type alias Model =
    { mousePosition : Maybe (Evergreen.V30.Point2d.Point2d Pixels.Pixels Evergreen.V30.MatchPage.ScreenCoordinate)
    , mousePositionPrevious : Maybe (Evergreen.V30.Point2d.Point2d Pixels.Pixels Evergreen.V30.MatchPage.ScreenCoordinate)
    , mouseDownAt : Maybe (Evergreen.V30.Point2d.Point2d Length.Meters Evergreen.V30.Match.WorldCoordinate)
    , wheelDownAt : Maybe (Evergreen.V30.Point2d.Point2d Length.Meters Evergreen.V30.Match.WorldCoordinate)
    , cameraPosition : Evergreen.V30.Point2d.Point2d Length.Meters Evergreen.V30.Match.WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length.Length
    , meshCache :
        AssocList.Dict
            (Evergreen.V30.Id.Id Evergreen.V30.Shape.LayerId)
            { pathMesh : Effect.WebGL.Mesh Evergreen.V30.MatchPage.Vertex
            , pathFillMesh : Effect.WebGL.Mesh Evergreen.V30.FontRender.FontVertex
            }
    , placingPoint :
        Maybe
            { index : Int
            , position : Evergreen.V30.Point2d.Point2d Length.Meters Evergreen.V30.Match.WorldCoordinate
            }
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend
