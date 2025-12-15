module MyUi exposing
    ( DisplayType(..)
    , button
    , displayType
    , ifMobile
    , simpleButton
    )

import Effect.Browser.Dom as Dom exposing (HtmlId)
import Pixels
import Quantity
import Size exposing (Size)
import Ui
import Ui.Input


button : HtmlId -> List (Ui.Attribute msg) -> { onPress : msg, label : Ui.Element msg } -> Ui.Element msg
button htmlId attributes { onPress, label } =
    Ui.el
        (Ui.id (Dom.idToString htmlId) :: Ui.width Ui.shrink :: Ui.Input.button onPress :: attributes)
        label


simpleButton : HtmlId -> msg -> Ui.Element msg -> Ui.Element msg
simpleButton htmlId onPress label =
    button
        htmlId
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
