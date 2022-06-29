module Evergreen.V30.FontRender exposing (..)

import Math.Vector2


type alias FontVertex =
    { position : Math.Vector2.Vec2
    , s : Float
    , t : Float
    }
