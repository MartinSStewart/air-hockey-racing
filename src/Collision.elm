module Collision exposing (circleCircle, circleLine)

import Axis2d
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


circleLine :
    Quantity Float Meters
    -> Point2d Meters coordinates
    -> Vector2d Meters coordinates
    -> LineSegment2d Meters coordinates
    -> Maybe (Point2d Meters coordinates)
circleLine circleRadius circlePosition circleVelocity line =
    case Vector2d.direction circleVelocity of
        Just direction ->
            case LineSegment2d.intersectionWithAxis (Axis2d.through circlePosition direction) line of
                Just a ->
                    case LineSegment2d.perpendicularDirection line of
                        Just perpendicularDirection ->
                            case
                                LineSegment2d.intersectionWithAxis
                                    (Axis2d.withDirection perpendicularDirection circlePosition)
                                    line
                            of
                                Just p1 ->
                                    let
                                        v1 : Vector2d Meters coordinates
                                        v1 =
                                            Vector2d.normalize circleVelocity |> Vector2d.unwrap |> Vector2d.unsafe

                                        collisionPoint =
                                            Quantity.ratio
                                                (Vector2d.length (Vector2d.from a circlePosition))
                                                (Vector2d.length (Vector2d.from p1 circlePosition))
                                                |> (\value -> Quantity.multiplyBy value circleRadius)
                                                |> (\value -> Vector2d.scaleBy (Length.inMeters value) v1)
                                                |> Vector2d.reverse
                                                |> (\vector -> Point2d.translateBy vector a)
                                    in
                                    if Point2d.distanceFrom circlePosition collisionPoint |> Quantity.lessThan (Vector2d.length circleVelocity) then
                                        Just collisionPoint

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


circleCircle :
    Quantity Float Meters
    -> Point2d Meters coordinate
    -> Vector2d Meters coordinate
    -> Point2d Meters coordinate
    -> Vector2d Meters coordinate
    -> Maybe ( Vector2d Meters coordinate, Vector2d Meters coordinate )
circleCircle radius p1 v1 p2 v2 =
    if Point2d.distanceFrom p1 p2 |> Quantity.lessThan (Quantity.multiplyBy 2 radius) then
        let
            ( circle1Vx, circle1Vy ) =
                Vector2d.toTuple Length.inMeters v1

            ( circle2Vx, circle2Vy ) =
                Vector2d.toTuple Length.inMeters v2

            ( cx1, cy1 ) =
                Point2d.toTuple Length.inMeters p1

            ( cx2, cy2 ) =
                Point2d.toTuple Length.inMeters p2

            d =
                sqrt ((cx1 - cx2) ^ 2 + (cy1 - cy2) ^ 2)

            circle1Mass =
                1

            circle2Mass =
                1

            nx =
                (cx2 - cx1) / d

            ny =
                (cy2 - cy1) / d

            p =
                2
                    * (circle1Vx * nx + circle1Vy * ny - circle2Vx * nx - circle2Vy * ny)
                    / (circle1Mass + circle2Mass)

            vx1 =
                circle1Vx - p * circle1Mass * nx

            vy1 =
                circle1Vy - p * circle1Mass * ny

            vx2 =
                circle2Vx + p * circle2Mass * nx

            vy2 =
                circle2Vy + p * circle2Mass * ny
        in
        Just ( Vector2d.fromMeters { x = vx1, y = vy1 }, Vector2d.fromMeters { x = vx2, y = vy2 } )

    else
        Nothing
