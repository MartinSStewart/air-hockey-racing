module Collision exposing (circleCircle, circleLine, circlePoint, cubicSplineToQuadratic, pointsToLineSegments)

import Axis2d exposing (Axis2d)
import CubicSpline2d exposing (CubicSpline2d)
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


axisAxis : Axis2d units coordinates -> Axis2d units coordinates -> Maybe (Point2d units coordinates)
axisAxis axis1 axis2 =
    let
        a1p1 =
            Axis2d.originPoint axis1

        a1p2 =
            Point2d.translateIn (Axis2d.direction axis1) (Quantity.unsafe 1) a1p1

        a1 =
            Vector2d.from a1p2 a1p1 |> Vector2d.yComponent |> Quantity.unwrap

        b1 =
            Vector2d.from a1p1 a1p2 |> Vector2d.xComponent |> Quantity.unwrap

        c1 =
            a1 * Quantity.unwrap (Point2d.xCoordinate a1p1) + b1 * Quantity.unwrap (Point2d.yCoordinate a1p1)

        a2p1 =
            Axis2d.originPoint axis2

        a2p2 =
            Point2d.translateIn (Axis2d.direction axis2) (Quantity.unsafe 1) a2p1

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
        Point2d.unsafe { x = x, y = y } |> Just


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
                                    in
                                    if
                                        Point2d.distanceFrom circlePosition collisionPoint
                                            |> Quantity.lessThan (Vector2d.length circleVelocity)
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


{-| <https://stackoverflow.com/a/14514491>
-}
find_inflection_points : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List Float
find_inflection_points p1x p1y p2x p2y p3x p3y p4x p4y =
    let
        ax =
            -p1x + 3 * p2x - 3 * p3x + p4x

        bx =
            3 * p1x - 6 * p2x + 3 * p3x

        cx =
            -3 * p1x + 3 * p2x

        ay =
            -p1y + 3 * p2y - 3 * p3y + p4y

        by =
            3 * p1y - 6 * p2y + 3 * p3y

        cy =
            -3 * p1y + 3 * p2y

        a =
            3 * (ay * bx - ax * by)

        b =
            3 * (ay * cx - ax * cy)

        c =
            by * cx - bx * cy

        r2 =
            b * b - 4 * a * c
    in
    if r2 >= 0 && a /= 0 then
        let
            r =
                sqrt r2

            firstIfp =
                (-b + r) / (2 * a)

            secondIfp =
                (-b - r) / (2 * a)
        in
        if (firstIfp > 0 && firstIfp < 1) && (secondIfp > 0 && secondIfp < 1) then
            let
                data =
                    if firstIfp > secondIfp then
                        { firstIfp = secondIfp, secondIfp = firstIfp }

                    else
                        { firstIfp = firstIfp, secondIfp = secondIfp }
            in
            if data.secondIfp - data.firstIfp > 0.00001 then
                [ data.firstIfp, data.secondIfp ]

            else
                [ data.firstIfp ]

        else if firstIfp > 0 && firstIfp < 1 then
            [ firstIfp ]

        else if secondIfp > 0 && secondIfp < 1 then
            [ secondIfp ]

        else
            []

    else
        []


get_t_values_of_critical_points :
    Point2d unit coordinate
    -> Point2d unit coordinate
    -> Point2d unit coordinate
    -> Point2d unit coordinate
    -> List Float
get_t_values_of_critical_points point1 point2 point3 point4 =
    let
        p1 =
            Point2d.unwrap point1

        c1 =
            Point2d.unwrap point2

        c2 =
            Point2d.unwrap point3

        p2 =
            Point2d.unwrap point4

        a0 =
            (c2.x - 2 * c1.x + p1.x) - (p2.x - 2 * c2.x + c1.x)

        b0 =
            2 * (c1.x - p1.x) - 2 * (c2.x - c1.x)

        c0 =
            p1.x - c1.x

        t1_0 =
            (-b0 + sqrt (b0 * b0 - 4 * a0 * c0)) / 2 / a0

        t2_0 =
            (-b0 - sqrt (b0 * b0 - 4 * a0 * c0)) / 2 / a0

        tvalues =
            (if t1_0 >= 0 && t1_0 <= 1 then
                [ t1_0 ]

             else
                []
            )
                ++ (if t2_0 >= 0 && t2_0 <= 1 && t1_0 /= t2_0 then
                        [ t2_0 ]

                    else
                        []
                   )
                ++ (if t1 >= 0 && t1 <= 1 && t1 /= t1_0 && t1 /= t2_0 then
                        [ t1 ]

                    else
                        []
                   )
                ++ (if t2 >= 0 && t2 <= 1 && t2 /= t1_0 && t2 /= t2_0 && t2 /= t1 then
                        [ t2 ]

                    else
                        []
                   )

        a =
            (c2.y - 2 * c1.y + p1.y) - (p2.y - 2 * c2.y + c1.y)

        b =
            2 * (c1.y - p1.y) - 2 * (c2.y - c1.y)

        c =
            p1.y - c1.y

        t1 =
            (-b + sqrt (b * b - 4 * a * c)) / 2 / a

        t2 =
            (-b - sqrt (b * b - 4 * a * c)) / 2 / a

        inflectionpoints =
            find_inflection_points p1.x p1.y c1.x c1.y c2.x c2.y p2.x p2.y
    in
    tvalues ++ inflectionpoints |> List.sort


cubicSplineToQuadratic :
    Quantity Float unit
    -> CubicSpline2d unit coordinate
    -> Result () (Nonempty (QuadraticSpline2d unit coordinate))
cubicSplineToQuadratic tolerance cubicSpline =
    cubicSplineToQuadraticHelper 1 tolerance cubicSpline


cubicSplineToQuadraticHelper :
    Int
    -> Quantity Float unit
    -> CubicSpline2d unit coordinate
    -> Result () (Nonempty (QuadraticSpline2d unit coordinate))
cubicSplineToQuadraticHelper iterations tolerance cubicSpline =
    --[ s0, s1, s2, s3 ]
    --    |> List.map simpleCubicSplineToQuadratic
    --splitRecursively 2 cubicSpline
    if iterations > 100 then
        Err ()

    else
        case splitIntoSegments iterations tolerance cubicSpline of
            Just nonempty ->
                Ok nonempty

            Nothing ->
                cubicSplineToQuadraticHelper (iterations + 1) tolerance cubicSpline


splitRecursively : Int -> CubicSpline2d units coordinates -> List (CubicSpline2d units coordinates)
splitRecursively n cubicSpline =
    if n > 0 then
        let
            ( s0, s1 ) =
                CubicSpline2d.splitAt 0.5 cubicSpline
        in
        splitRecursively (n - 1) s0 ++ splitRecursively (n - 1) s1

    else
        [ cubicSpline ]


simpleCubicSplineToQuadratic : CubicSpline2d unit coordinate -> QuadraticSpline2d unit coordinate
simpleCubicSplineToQuadratic cubicSpline =
    let
        p0 =
            CubicSpline2d.firstControlPoint cubicSpline

        v0 =
            Vector2d.zero

        v1 =
            Vector2d.from p0 (CubicSpline2d.secondControlPoint cubicSpline)

        v2 =
            Vector2d.from p0 (CubicSpline2d.thirdControlPoint cubicSpline)

        v3 =
            Vector2d.from p0 (CubicSpline2d.fourthControlPoint cubicSpline)
    in
    QuadraticSpline2d.fromControlPoints
        p0
        (Point2d.translateBy
            (Vector2d.sum
                [ Vector2d.scaleBy (-1 / 4) v0
                , Vector2d.scaleBy (3 / 4) v1
                , Vector2d.scaleBy (3 / 4) v2
                , Vector2d.scaleBy (-1 / 4) v3
                ]
            )
            p0
        )
        (CubicSpline2d.fourthControlPoint cubicSpline)


splitIntoSegments :
    Int
    -> Quantity Float unit
    -> CubicSpline2d unit coordinate
    -> Maybe (Nonempty (QuadraticSpline2d unit coordinate))
splitIntoSegments segments tolerance cubicSpline =
    List.range 1 (segments - 1)
        |> List.map (\index -> toFloat index / toFloat segments)
        |> List.foldl
            (\criticalPoint maybeState ->
                case maybeState of
                    Just state ->
                        let
                            ( s0, s1 ) =
                                CubicSpline2d.splitAt
                                    ((criticalPoint - state.lastPoint) / (1 - state.lastPoint))
                                    state.remainingSpline

                            quadraticSpline : QuadraticSpline2d units coordinates
                            quadraticSpline =
                                simpleCubicSplineToQuadratic s0
                        in
                        if checkTolerance tolerance quadraticSpline cubicSpline then
                            { remainingSpline = s1
                            , splineSegments = quadraticSpline :: state.splineSegments
                            , lastPoint = criticalPoint
                            }
                                |> Just

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
            (Just { remainingSpline = cubicSpline, splineSegments = [], lastPoint = 0 })
        |> Maybe.andThen
            (\a ->
                let
                    quadraticSpline : QuadraticSpline2d unit coordinate
                    quadraticSpline =
                        simpleCubicSplineToQuadratic a.remainingSpline
                in
                if checkTolerance tolerance quadraticSpline cubicSpline then
                    Nonempty quadraticSpline a.splineSegments |> List.Nonempty.reverse |> Just

                else
                    Nothing
            )


checkTolerance : Quantity Float units -> QuadraticSpline2d units coordinates -> CubicSpline2d units coordinates -> Bool
checkTolerance tolerance quadraticSpline cubicSpline =
    List.range 1 9
        |> List.all
            (\index ->
                let
                    t =
                        toFloat index / 10
                in
                Point2d.distanceFrom
                    (QuadraticSpline2d.pointOn quadraticSpline t)
                    (CubicSpline2d.pointOn cubicSpline t)
                    |> Quantity.lessThan tolerance
            )



--let
--    criticalPoints : List Float
--    criticalPoints =
--        get_t_values_of_critical_points
--            (CubicSpline2d.firstControlPoint cubicSpline)
--            (CubicSpline2d.secondControlPoint cubicSpline)
--            (CubicSpline2d.thirdControlPoint cubicSpline)
--            (CubicSpline2d.fourthControlPoint cubicSpline)
--in
--List.foldl
--    (\criticalPoint state ->
--        let
--            ( s0, s1 ) =
--                CubicSpline2d.splitAt
--                    ((criticalPoint - state.lastPoint) / (1 - state.lastPoint))
--                    state.remainingSpline
--        in
--        { remainingSpline = s1
--        , splineSegments = s0 :: state.splineSegments
--        , lastPoint = criticalPoint
--        }
--    )
--    { remainingSpline = cubicSpline, splineSegments = [], lastPoint = 0 }
--    criticalPoints
--    |> .splineSegments
--    |> List.map
--        (\cubic ->
--            QuadraticSpline2d.fromControlPoints
--                (CubicSpline2d.firstControlPoint cubic)
--                (axisAxis
--                    (Axis2d.throughPoints
--                        (CubicSpline2d.firstControlPoint cubic)
--                        (CubicSpline2d.secondControlPoint cubic)
--                        |> Maybe.withDefault Axis2d.x
--                    )
--                    (Axis2d.throughPoints
--                        (CubicSpline2d.thirdControlPoint cubic)
--                        (CubicSpline2d.fourthControlPoint cubic)
--                        |> Maybe.withDefault Axis2d.x
--                    )
--                    |> Maybe.withDefault (CubicSpline2d.firstControlPoint cubic)
--                )
--                (CubicSpline2d.fourthControlPoint cubic)
--        )


pointsToLineSegments : List (Point2d units coordinates) -> List (LineSegment2d units coordinates)
pointsToLineSegments points =
    case ( List.head points, List.reverse points |> List.head ) of
        ( Just head, Just last ) ->
            points
                |> List.groupsOfWithStep 2 1
                |> (::) [ last, head ]
                |> List.filterMap
                    (\list ->
                        case list of
                            [ first, second ] ->
                                LineSegment2d.from first second |> Just

                            _ ->
                                Nothing
                    )

        _ ->
            []
