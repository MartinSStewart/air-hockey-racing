module Evergreen.V26.Geometry.Types exposing (..)


type Direction2d coordinates
    = Direction2d
        { x : Float
        , y : Float
        }


type Point2d units coordinates
    = Point2d
        { x : Float
        , y : Float
        }


type Vector2d units coordinates
    = Vector2d
        { x : Float
        , y : Float
        }
