module Evergreen.V30.Shape exposing (..)

import Evergreen.V30.Match
import Evergreen.V30.Point2d
import Evergreen.V30.Vector2d
import Length


type LayerId
    = LayerId Never


type alias PathSegment =
    { position : Evergreen.V30.Point2d.Point2d Length.Meters Evergreen.V30.Match.WorldCoordinate
    , handlePrevious : Evergreen.V30.Vector2d.Vector2d Length.Meters Evergreen.V30.Match.WorldCoordinate
    , handleNext : Evergreen.V30.Vector2d.Vector2d Length.Meters Evergreen.V30.Match.WorldCoordinate
    }


type alias Layer =
    { path : List PathSegment
    , red : Int
    , green : Int
    , blue : Int
    }
