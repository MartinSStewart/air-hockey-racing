module Geometry exposing (circleCircle, circleLine, circlePoint, cubicSplineToQuadratic, findNearestPoint, pointsToLineSegments)

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


cubicSplineToQuadratic :
    Quantity Float unit
    -> CubicSpline2d unit coordinate
    -> Nonempty (QuadraticSpline2d unit coordinate)
cubicSplineToQuadratic tolerance cubicSpline =
    cubicSplineToQuadraticHelper 1 tolerance cubicSpline


cubicSplineToQuadraticHelper :
    Int
    -> Quantity Float unit
    -> CubicSpline2d unit coordinate
    -> Nonempty (QuadraticSpline2d unit coordinate)
cubicSplineToQuadraticHelper iterations tolerance cubicSpline =
    if iterations > 100 then
        simpleCubicSplineToQuadratic cubicSpline |> List.Nonempty.fromElement

    else
        case splitIntoSegments iterations tolerance cubicSpline of
            Just nonempty ->
                nonempty

            Nothing ->
                cubicSplineToQuadraticHelper (iterations + 1) tolerance cubicSpline


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



--
--cubicBeziersToPath : CubicSpline2d unit coordinate -> List (CubicSpline2d unit coordinate)
--cubicBeziersToPath cubicSpline =
--    let
--        c0 =
--            CubicSpline2d.firstControlPoint cubicSpline
--
--        c1 =
--            CubicSpline2d.firstControlPoint cubicSpline
--
--        c2 =
--            CubicSpline2d.firstControlPoint cubicSpline
--
--        c3 =
--            CubicSpline2d.firstControlPoint cubicSpline
--
--        list =
--            [ c0, c1, c2, c3 ]
--                |> List.foldl
--                    (\point state ->
--                        case state.previousPoint of
--                            Nothing state
--                    )
--                    { previousPoint = Nothing, offsetPath = [] }
--    in
--    []


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

                            quadraticSpline : QuadraticSpline2d unit coordinate
                            quadraticSpline =
                                simpleCubicSplineToQuadratic s0
                        in
                        if isValidConversion tolerance quadraticSpline s0 then
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
                if isValidConversion tolerance quadraticSpline a.remainingSpline then
                    Nonempty quadraticSpline a.splineSegments |> List.Nonempty.reverse |> Just

                else
                    Nothing
            )


isValidConversion : Quantity Float units -> QuadraticSpline2d units coordinates -> CubicSpline2d units coordinates -> Bool
isValidConversion tolerance quadraticSpline cubicSpline =
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



--
--{-| <https://www.pouet.net/topic.php?which=9119&page=1#c429023>
---}
--solveCubic a b c =
--    let
--        p =
--            b - a * a / 3
--
--        q =
--            a * (2 * a * a - 9 * b) / 27 + c
--
--        p3 =
--            p * p * p
--
--        d =
--            q * q + 4 * p3 / 27
--
--        offset =
--            -a / 3
--    in
--    if d >= 0 then
--        let
--            z =
--                sqrt d
--
--            u =
--                (-q + z) / 2 |> cuberoot
--
--            v =
--                (-q - z) / 2 |> cuberoot
--        in
--        [ offset + u + v ]
--
--    else
--        let
--            u =
--                sqrt (-p / 3)
--
--            v =
--                acos (-(sqrt (-27 / p3)) * q / 2) / 3
--
--            m =
--                cos v
--
--            n =
--                sin v * 1.732050808
--        in
--        [ offset + u * (m + m)
--        , offset - u * (n + m)
--        , offset + u * (n - m)
--        ]
--
--
--cuberoot v =
--    pow v (1 / 3)
--
--
--signedDistanceSquared : Point2d unit coordinate -> QuadraticSpline2d unit coordinate -> Quantity Float unit
--signedDistanceSquared p spline =
--    let
--        aCap : Vector2d unit coordinate
--        aCap =
--            Vector2d.from
--                (QuadraticSpline2d.firstControlPoint spline)
--                (QuadraticSpline2d.secondControlPoint spline)
--
--        bCap : Vector2d unit coordinate
--        bCap =
--            Vector2d.from
--                (QuadraticSpline2d.secondControlPoint spline)
--                (QuadraticSpline2d.thirdControlPoint spline)
--                |> Vector2d.minus aCap
--
--        a : Float
--        a =
--            squareVector bCap
--
--        b : Float
--        b =
--            Vector2d.scaleBy 3 aCap |> Vector2d.dot bCap |> Quantity.unwrap
--
--        c =
--            Vector2d.scaleBy 2 aCap |> squareVector
--    in
--    Quantity.zero
--
--
--squareVector v =
--    let
--        { x, y } =
--            Vector2d.unwrap v
--    in
--    x * x + y * y
--signedDistanceSquared p s =
--    let
--        minDis = 1e20
--
--
--        dCap = s.p0 - p
--
--        a = s.A
--        b = s.B
--        c = s.C + dot(D, s.c)
--        d = dot(dCap, s.d)
--
--        res = solveCubic (b*a) (c*a) (d*a)
--
--        for(int j=0 j<n j++) {
--            float t = Clamp(res[j])
--            point2D d = s.p0 + (s.b + s.c*t)*t - p
--            minDis = min(minDis, dot(d,d))
--        }
--    in
--    return minDis
--}


getDist x1 y1 x2 y2 =
    sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))


findNearestPoint :
    Point2d units coordinates
    -> QuadraticSpline2d units coordinates
    -> { t : Float, pos : Point2d units coordinates, dist : Quantity Float units }
findNearestPoint point spline =
    let
        { x, y } =
            Point2d.unwrap point

        p0 =
            QuadraticSpline2d.firstControlPoint spline |> Point2d.unwrap

        p1 =
            QuadraticSpline2d.secondControlPoint spline |> Point2d.unwrap

        p2 =
            QuadraticSpline2d.thirdControlPoint spline |> Point2d.unwrap

        aCap =
            { x = p1.x - p0.x
            , y = p1.y - p0.y
            }

        bCap =
            { x = p0.x - 2 * p1.x + p2.x
            , y = p0.y - 2 * p1.y + p2.y
            }

        pos =
            { x = p0.x - x, y = p0.y - y }

        a =
            bCap.x * bCap.x + bCap.y * bCap.y

        b =
            3 * (aCap.x * bCap.x + aCap.y * bCap.y)

        c =
            2 * (aCap.x * aCap.x + aCap.y * aCap.y) + pos.x * bCap.x + pos.y * bCap.y

        d =
            pos.x * aCap.x + pos.y * aCap.y

        sol =
            thirdDegreeEquation a b c d

        d0 =
            getDist x y p0.x p0.y

        d2 =
            getDist x y p2.x p2.y

        maybeResult =
            List.filterMap
                (\t ->
                    if t >= 0 && t <= 1 then
                        let
                            pos2 =
                                getPos p0 p1 p2 t

                            dist =
                                getDist x y pos2.x pos2.y
                        in
                        if dist < d0 && dist < d2 then
                            { tMin = t
                            , distMin = dist
                            , posMin = pos2
                            }
                                |> Just

                        else
                            Nothing

                    else
                        Nothing
                )
                sol
                |> List.minimumBy .distMin
    in
    case maybeResult of
        Just { tMin, distMin, posMin } ->
            { t = tMin
            , pos = Point2d.unsafe posMin
            , dist = Quantity.unsafe distMin
            }

        Nothing ->
            if d0 < d2 then
                { t = 0
                , pos = Point2d.unsafe { x = p0.x, y = p0.y }
                , dist = Quantity.unsafe d0
                }

            else
                { t = 1
                , pos = Point2d.unsafe { x = p2.x, y = p2.y }
                , dist = Quantity.unsafe d2
                }


getPos p0 p1 p2 t =
    let
        a =
            (1 - t) * (1 - t)

        b =
            2 * t * (1 - t)

        c =
            t * t
    in
    { x = a * p0.x + b * p1.x + c * p2.x
    , y = a * p0.y + b * p1.y + c * p2.y
    }


zeroMax =
    0.0000001


thirdDegreeEquation : Float -> Float -> Float -> Float -> List Float
thirdDegreeEquation a b c d =
    if abs a > zeroMax then
        let
            a2 =
                b / a

            b2 =
                c / a

            c2 =
                d / a

            p =
                b2 - a2 * a2 / 3

            q =
                a2 * (2 * a2 * a2 - 9 * b2) / 27 + c2

            p3 =
                p * p * p

            dCap =
                q * q + 4 * p3 / 27

            offset =
                -a2 / 3
        in
        if dCap > zeroMax then
            let
                z =
                    sqrt dCap

                u =
                    (-q + z) / 2

                v =
                    (-q - z) / 2

                u2 =
                    if u >= 0 then
                        u ^ (1 / 3)

                    else
                        -(-u ^ (1 / 3))

                v2 =
                    if v >= 0 then
                        v ^ (1 / 3)

                    else
                        -(-v ^ (1 / 3))
            in
            [ u2 + v2 + offset ]

        else if dCap < -zeroMax then
            let
                u =
                    2 * sqrt (-p / 3)

                v =
                    acos -(sqrt (-27 / p3) * q / 2) / 3
            in
            [ u * cos v + offset
            , u * cos (v + 2 * pi / 3) + offset
            , u * cos (v + 4 * pi / 3) + offset
            ]

        else
            let
                u =
                    if q < 0 then
                        (-q / 2) ^ (1 / 3)

                    else
                        -((q / 2) ^ (1 / 3))
            in
            [ 2 * u + offset, -u + offset ]

    else
        let
            a2 =
                b

            b2 =
                c

            c2 =
                d
        in
        if abs a2 <= zeroMax then
            if abs b2 <= zeroMax then
                []

            else
                [ -c2 / b2 ]

        else
            let
                dCap =
                    b2 * b2 - 4 * a2 * c2
            in
            if dCap <= -zeroMax then
                []

            else if dCap > zeroMax then
                let
                    dCap2 =
                        sqrt dCap
                in
                [ (-b2 - dCap2) / (2 * a2)
                , (-b2 + dCap2) / (2 * a2)
                ]

            else if dCap < -zeroMax then
                []

            else
                [ -b2 / (2 * a2) ]
