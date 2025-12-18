module Decal exposing (Decal(..), allDecals, toString, triangles)

import ColorIndex exposing (ColorIndex)
import List.Nonempty exposing (Nonempty(..))
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)


type Decal
    = Star
    | Triangle
    | Plus
    | Minus
    | Square
    | HollowSquare


allDecals : Nonempty Decal
allDecals =
    Nonempty Star [ Triangle, Plus, Minus, Square, HollowSquare ]


toString : Decal -> String
toString decal =
    case decal of
        Star ->
            "★"

        Triangle ->
            "▲"

        Plus ->
            " + "

        Minus ->
            " - "

        Square ->
            " ■ "

        HollowSquare ->
            " □ "


type alias Vertex =
    { position : Vec3, color : Vec3 }


triangles : ColorIndex -> Decal -> List ( Vertex, Vertex, Vertex )
triangles colorIndex decal =
    let
        color =
            ColorIndex.toVec3 colorIndex
    in
    (case decal of
        Star ->
            let
                size =
                    0.7
            in
            List.range 0 4
                |> List.map
                    (\index ->
                        let
                            t0 =
                                2 * pi * toFloat index / 5

                            t1 =
                                2 * pi * toFloat (index + 2) / 5

                            t2 =
                                pi + 2 * pi * toFloat (index + 1) / 5
                        in
                        ( Vec3.vec3 (cos t0 * size) (sin t0 * size) 0
                        , Vec3.vec3 (cos t1 * size) (sin t1 * size) 0
                        , Vec3.vec3 (cos t2 * size * 0.4) (sin t2 * size * 0.4) 0
                        )
                    )

        Triangle ->
            let
                size =
                    0.7
            in
            [ ( Vec3.vec3 (cos 0 * size) (sin 0 * size) 0
              , Vec3.vec3 (cos (2 * pi / 3) * size) (sin (2 * pi / 3) * size) 0
              , Vec3.vec3 (cos (4 * pi / 3) * size) (sin (4 * pi / 3) * size) 0
              )
            ]

        Plus ->
            [ ( Vec3.vec3 0.6 0.15 0
              , Vec3.vec3 -0.6 0.15 0
              , Vec3.vec3 0.6 -0.15 0
              )
            , ( Vec3.vec3 -0.6 0.15 0
              , Vec3.vec3 -0.6 -0.15 0
              , Vec3.vec3 0.6 -0.15 0
              )
            , ( Vec3.vec3 0.15 0.6 0
              , Vec3.vec3 -0.15 0.6 0
              , Vec3.vec3 0.15 -0.6 0
              )
            , ( Vec3.vec3 0.15 -0.6 0
              , Vec3.vec3 -0.15 0.6 0
              , Vec3.vec3 -0.15 -0.6 0
              )
            ]

        Minus ->
            [ ( Vec3.vec3 0.6 0.15 0
              , Vec3.vec3 -0.6 0.15 0
              , Vec3.vec3 0.6 -0.15 0
              )
            , ( Vec3.vec3 -0.6 0.15 0
              , Vec3.vec3 -0.6 -0.15 0
              , Vec3.vec3 0.6 -0.15 0
              )
            ]

        Square ->
            let
                size =
                    0.5
            in
            [ ( Vec3.vec3 size size 0
              , Vec3.vec3 -size -size 0
              , Vec3.vec3 size -size 0
              )
            , ( Vec3.vec3 size size 0
              , Vec3.vec3 -size size 0
              , Vec3.vec3 -size -size 0
              )
            ]

        HollowSquare ->
            let
                size =
                    0.5

                innerSize =
                    0.3
            in
            [ ( Vec3.vec3 size size 0
              , Vec3.vec3 innerSize innerSize 0
              , Vec3.vec3 innerSize -innerSize 0
              )
            , ( Vec3.vec3 size size 0
              , Vec3.vec3 innerSize -innerSize 0
              , Vec3.vec3 size -size 0
              )
            , ( Vec3.vec3 size size 0
              , Vec3.vec3 -innerSize innerSize 0
              , Vec3.vec3 innerSize innerSize 0
              )
            , ( Vec3.vec3 size size 0
              , Vec3.vec3 -size size 0
              , Vec3.vec3 -innerSize innerSize 0
              )
            , ( Vec3.vec3 -size -size 0
              , Vec3.vec3 -innerSize -innerSize 0
              , Vec3.vec3 -innerSize innerSize 0
              )
            , ( Vec3.vec3 -size -size 0
              , Vec3.vec3 -innerSize innerSize 0
              , Vec3.vec3 -size size 0
              )
            , ( Vec3.vec3 -size -size 0
              , Vec3.vec3 innerSize -innerSize 0
              , Vec3.vec3 -innerSize -innerSize 0
              )
            , ( Vec3.vec3 -size -size 0
              , Vec3.vec3 size -size 0
              , Vec3.vec3 innerSize -innerSize 0
              )
            ]
    )
        |> List.map
            (\( a, b, c ) ->
                ( { position = a, color = color }
                , { position = b, color = color }
                , { position = c, color = color }
                )
            )
