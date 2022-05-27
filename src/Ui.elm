module Ui exposing (..)

import Element exposing (Element)
import Element.Input


button : List (Element.Attribute msg) -> { onPress : msg, label : Element msg } -> Element msg
button attributes { onPress, label } =
    Element.Input.button attributes { onPress = Just onPress, label = label }
