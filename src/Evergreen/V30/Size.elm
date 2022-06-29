module Evergreen.V30.Size exposing (..)

import Pixels
import Quantity


type alias Size =
    { width : Quantity.Quantity Int Pixels.Pixels
    , height : Quantity.Quantity Int Pixels.Pixels
    }
