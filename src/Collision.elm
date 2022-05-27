module Collision exposing (circleLine)

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
