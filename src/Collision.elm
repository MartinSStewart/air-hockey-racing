module Collision exposing (circleCircle, circleLine, circlePoint)

import Axis2d exposing (Axis2d)
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


axisAxis : Axis2d Meters coordinates -> Axis2d Meters coordinates -> Maybe (Point2d Meters coordinates)
axisAxis axis1 axis2 =
    let
        a1p1 =
            Axis2d.originPoint axis1

        a1p2 =
            Point2d.translateIn (Axis2d.direction axis1) Length.meter a1p1

        a1 =
            Vector2d.from a1p2 a1p1 |> Vector2d.yComponent |> Quantity.unwrap

        b1 =
            Vector2d.from a1p1 a1p2 |> Vector2d.xComponent |> Quantity.unwrap

        c1 =
            a1 * Quantity.unwrap (Point2d.xCoordinate a1p1) + b1 * Quantity.unwrap (Point2d.yCoordinate a1p1)

        a2p1 =
            Axis2d.originPoint axis2

        a2p2 =
            Point2d.translateIn (Axis2d.direction axis2) Length.meter a2p1

        a2 =
            Vector2d.from a2p2 a2p1 |> Vector2d.yComponent |> Quantity.unwrap

        b2 =
            Vector2d.from a2p1 a2p2 |> Vector2d.xComponent |> Quantity.unwrap

        c2 =
            a2 * Quantity.unwrap (Point2d.xCoordinate a2p1) + b2 * Quantity.unwrap (Point2d.yCoordinate a2p1)

        delta =
            a1 * b2 - a2 * b1

        x =
            (b2 * c1 - b1 * c2) / delta

        y =
            (a1 * c2 - a2 * c1) / delta
    in
    if isNaN x || isInfinite x || isNaN y || isInfinite y then
        Nothing

    else
        Point2d.fromMeters { x = x, y = y } |> Just


lineToAxis : LineSegment2d units coordinates -> Maybe (Axis2d units coordinates)
lineToAxis line =
    Axis2d.throughPoints (LineSegment2d.startPoint line) (LineSegment2d.endPoint line)


circleLine :
    Quantity Float Meters
    -> Point2d Meters coordinates
    -> Vector2d Meters coordinates
    -> LineSegment2d Meters coordinates
    -> Maybe (Point2d Meters coordinates)
circleLine circleRadius circlePosition circleVelocity line =
    case Vector2d.direction circleVelocity of
        Just direction ->
            case Maybe.andThen (axisAxis (Axis2d.through circlePosition direction)) (lineToAxis line) of
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

                                        collisionPoint : Point2d Meters coordinates
                                        collisionPoint =
                                            Quantity.ratio
                                                (Vector2d.length (Vector2d.from a circlePosition))
                                                (Vector2d.length (Vector2d.from p1 circlePosition))
                                                |> (\value -> Quantity.multiplyBy value circleRadius)
                                                |> (\value -> Vector2d.scaleBy (Length.inMeters value) v1)
                                                |> Vector2d.reverse
                                                |> (\vector -> Point2d.translateBy vector a)

                                        _ =
                                            Debug.log "a" collisionPoint
                                    in
                                    if
                                        Point2d.distanceFrom circlePosition collisionPoint
                                            |> Debug.log "distance"
                                            |> Quantity.lessThan (Vector2d.length circleVelocity |> Debug.log "velocity")
                                    then
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


circlePoint :
    Quantity Float Meters
    -> Point2d Meters coordinates
    -> Vector2d Meters coordinates
    -> Point2d Meters coordinates
    -> Maybe (Point2d Meters coordinates)
circlePoint circleRadius circlePosition circleVelocity point =
    let
        a =
            Point2d.translateBy circleVelocity circlePosition
    in
    if Point2d.distanceFrom a point |> Quantity.lessThan circleRadius then
        Just circlePosition

    else
        Nothing



--case Vector2d.direction circleVelocity of
--    Just direction ->
--        let
--            closestDistance : Float
--            closestDistance =
--                Point2d.signedDistanceFrom (Axis2d.through circlePosition direction) point
--                    |> Quantity.abs
--                    |> Quantity.unwrap
--
--            rawCircleRadius : Float
--            rawCircleRadius =
--                Quantity.unwrap circleRadius
--        in
--        if closestDistance <= rawCircleRadius then
--            let
--                collisionPoint =
--                    (rawCircleRadius ^ 2 - closestDistance ^ 2)
--                        |> sqrt
--                        |> Length.meters
--                        |> (\value -> Vector2d.withLength value direction)
--                        |> Vector2d.reverse
--                        |> Vector2d.plus circleVelocity
--                        |> (\vector -> Point2d.translateBy vector circlePosition)
--
--                d =
--                    Vector2d.from circlePosition collisionPoint
--
--                t =
--                    Vector2d.dot circleVelocity d
--                        |> Quantity.unwrap
--            in
--            if t > 0 && (Vector2d.length d |> Quantity.lessThan (Vector2d.length circleVelocity)) then
--                Just collisionPoint
--
--            else
--                Nothing
--
--        else
--            Nothing
--
--    Nothing ->
--        Nothing


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
