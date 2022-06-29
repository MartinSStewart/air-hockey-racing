module Size exposing (Size)

import Pixels exposing (Pixels)
import Quantity exposing (Quantity)


type alias Size =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }
