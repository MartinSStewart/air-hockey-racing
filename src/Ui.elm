module Ui exposing
    ( DisplayType(..)
    , Size
    , button
    , displayType
    , ifMobile
    , simpleButton
    )

import Element exposing (Element)
import Element.Background
import Element.Input
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)


type alias Size =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


button : List (Element.Attribute msg) -> { onPress : msg, label : Element msg } -> Element msg
button attributes { onPress, label } =
    Element.Input.button attributes { onPress = Just onPress, label = label }


simpleButton : msg -> Element msg -> Element msg
simpleButton onPress label =
    button
        [ Element.Background.color <| Element.rgb 0.9 0.9 0.85
        , Element.padding 4
        ]
        { onPress = onPress
        , label = label
        }


type DisplayType
    = Desktop
    | Mobile


displayType : Size -> DisplayType
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
