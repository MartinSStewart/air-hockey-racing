module MyUi exposing
    ( DisplayType(..)
    , button
    , displayType
    , ifMobile
    , simpleButton
    )

import Pixels
import Quantity
import Size exposing (Size)
import Ui
import Ui.Input


button : List (Ui.Attribute msg) -> { onPress : msg, label : Ui.Element msg } -> Ui.Element msg
button attributes { onPress, label } =
    Ui.el
        (Ui.width Ui.shrink :: Ui.Input.button onPress :: attributes)
        label


simpleButton : msg -> Ui.Element msg -> Ui.Element msg
simpleButton onPress label =
    button
        [ Ui.background <| Ui.rgb 230 230 217
        , Ui.padding 4
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
