module ColorIndex exposing (ColorIndex(..), allColors, toElColor, toVec3)

import Color exposing (Color)
import Element
import List.Nonempty exposing (Nonempty(..))
import Math.Vector3 exposing (Vec3)


type ColorIndex
    = Red
    | Green
    | Blue
    | Orange
    | Brown
    | Purple
    | Pink
    | Yellow


toColor : ColorIndex -> Color
toColor colorIndex =
    case colorIndex of
        Red ->
            Color.red

        Green ->
            Color.green

        Blue ->
            Color.blue

        Orange ->
            Color.orange

        Brown ->
            Color.rgb255 110 71 0

        Purple ->
            Color.purple

        Pink ->
            Color.rgb 1 0.7 0.7

        Yellow ->
            Color.yellow


toElColor : ColorIndex -> Element.Color
toElColor =
    toColor >> colorToElColor


toVec3 : ColorIndex -> Vec3
toVec3 colorIndex =
    let
        { red, green, blue } =
            toColor colorIndex |> Color.toRgba
    in
    Math.Vector3.vec3 red green blue


allColors : Nonempty ColorIndex
allColors =
    Nonempty Red [ Orange, Yellow, Green, Blue, Purple, Pink, Brown ]


colorToElColor : Color -> Element.Color
colorToElColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    Element.rgba red green blue alpha
