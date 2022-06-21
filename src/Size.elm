module Size exposing (Size, toPoint)

import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)


type alias Size =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


toPoint : Size -> Point2d Pixels coordinate
toPoint size =
    Point2d.pixels
        (toFloat (Pixels.inPixels size.width))
        (toFloat (Pixels.inPixels size.height))
