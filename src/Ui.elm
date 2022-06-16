module Ui exposing
    ( DisplayType(..)
    , WindowSize
    , button
    , displayType
    , ifMobile
    )

import Element exposing (Element)
import Element.Input
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)


type alias WindowSize =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


button : List (Element.Attribute msg) -> { onPress : msg, label : Element msg } -> Element msg
button attributes { onPress, label } =
    Element.Input.button attributes { onPress = Just onPress, label = label }


type DisplayType
    = Desktop
    | Mobile


displayType : WindowSize -> DisplayType
displayType windowSize =
    if windowSize.width |> Quantity.lessThan (Pixels.pixels 800) then
        Mobile

    else
        Desktop


ifMobile : DisplayType -> a -> a -> a
ifMobile displayType_ a b =
    case displayType_ of
        Mobile ->
            a

        Desktop ->
            b
