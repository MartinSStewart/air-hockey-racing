module Tests exposing (..)

import Collision
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Length
import LineSegment2d
import Point2d
import Test exposing (..)
import Vector2d


suite : Test
suite =
    describe "Intersections"
        [ describe "circleLine"
            [ test "Wall collision 0" <|
                \_ ->
                    Collision.circleLine
                        Length.meter
                        (Point2d.fromMeters { x = -1, y = 0 })
                        (Vector2d.fromMeters { x = 2, y = 0 })
                        (LineSegment2d.from (Point2d.fromMeters { x = 1, y = -1 }) (Point2d.fromMeters { x = 1, y = 1 }))
                        |> Expect.equal (Just (Point2d.fromMeters { x = 0, y = 0 }))
            , test "Wall collision 1" <|
                \_ ->
                    Collision.circleLine
                        Length.meter
                        (Point2d.fromMeters { x = 4, y = 0 })
                        (Vector2d.fromMeters { x = -3, y = 0 })
                        (LineSegment2d.from (Point2d.fromMeters { x = 1, y = -1 }) (Point2d.fromMeters { x = 1, y = 1 }))
                        |> Expect.equal (Just (Point2d.fromMeters { x = 2, y = 0 }))
            , test "Wall collision 2" <|
                \_ ->
                    Collision.circleLine
                        (Length.meters 2)
                        (Point2d.fromMeters { x = 4, y = 0 })
                        (Vector2d.fromMeters { x = -3, y = 0 })
                        (LineSegment2d.from (Point2d.fromMeters { x = 1, y = -1 }) (Point2d.fromMeters { x = 1, y = 1 }))
                        |> Expect.equal (Just (Point2d.fromMeters { x = 3, y = 0 }))
            , test "Wall collision 3" <|
                \_ ->
                    Collision.circleLine
                        Length.meter
                        (Point2d.fromMeters { x = -1, y = -1 })
                        (Vector2d.fromMeters { x = 2, y = 2 })
                        (LineSegment2d.from (Point2d.fromMeters { x = 1, y = -1 }) (Point2d.fromMeters { x = 1, y = 1 }))
                        |> Expect.equal (Just (Point2d.fromMeters { x = 0, y = 0 }))
            , test "Wall collision 4" <|
                \_ ->
                    Collision.circleLine
                        Length.meter
                        (Point2d.fromMeters { x = -1, y = -1 })
                        (Vector2d.fromMeters { x = 0.5, y = 0.5 })
                        (LineSegment2d.from (Point2d.fromMeters { x = 1, y = -1 }) (Point2d.fromMeters { x = 1, y = 1 }))
                        |> Expect.equal Nothing
            , test "Wall collision 5" <|
                \_ ->
                    Collision.circleLine
                        Length.meter
                        (Point2d.fromMeters { x = -0.5, y = 0 })
                        (Vector2d.fromMeters { x = -2, y = 0 })
                        (LineSegment2d.from (Point2d.fromMeters { x = 1, y = -1 }) (Point2d.fromMeters { x = 1, y = 1 }))
                        |> Expect.equal Nothing
            , test "Wall collision 6" <|
                \_ ->
                    Collision.circleLine
                        Length.meter
                        (Point2d.fromMeters { x = -0.01, y = 0 })
                        (Vector2d.fromMeters { x = 0.2, y = 1 })
                        (LineSegment2d.from (Point2d.fromMeters { x = 1, y = -1 }) (Point2d.fromMeters { x = 1, y = 1 }))
                        |> Expect.equal (Just (Point2d.fromMeters { x = -2.220446049250313e-16, y = 0.04999999999999982 }))
            ]

        --, describe "circlePoint"
        --    [ test "Collision 0" <|
        --        \_ ->
        --            Collision.circlePoint
        --                Length.meter
        --                (Point2d.fromMeters { x = 0, y = 0 })
        --                (Vector2d.fromMeters { x = 2, y = 0 })
        --                (Point2d.fromMeters { x = 2, y = 0 })
        --                |> Expect.equal (Just (Point2d.fromMeters { x = 1, y = 0 }))
        --    , test "Collision 1" <|
        --        \_ ->
        --            Collision.circlePoint
        --                Length.meter
        --                (Point2d.fromMeters { x = 1, y = 0 })
        --                (Vector2d.fromMeters { x = 2, y = 0 })
        --                (Point2d.fromMeters { x = 3, y = 0 })
        --                |> Expect.equal (Just (Point2d.fromMeters { x = 2, y = 0 }))
        --    , test "Collision 2" <|
        --        \_ ->
        --            Collision.circlePoint
        --                Length.meter
        --                (Point2d.fromMeters { x = 1, y = 1 })
        --                (Vector2d.fromMeters { x = 2, y = 0 })
        --                (Point2d.fromMeters { x = 2, y = 1.5 })
        --                |> Expect.equal (Just (Point2d.fromMeters { x = 2.1339745962155616, y = 1 }))
        --    , test "Collision 3" <|
        --        \_ ->
        --            Collision.circlePoint
        --                Length.meter
        --                (Point2d.fromMeters { x = 0, y = 0 })
        --                (Vector2d.fromMeters { x = 2, y = 2 })
        --                (Point2d.fromMeters { x = 2, y = 2 })
        --                |> Expect.equal (Just (Point2d.fromMeters { x = 1.2928932188134525, y = 1.2928932188134525 }))
        --    , test "Collision 4" <|
        --        \_ ->
        --            Collision.circlePoint
        --                (Length.meters 50)
        --                (Point2d.fromMeters { x = 2723.8881168093935, y = 73.08895104607079 })
        --                (Vector2d.fromMeters { x = 0.006066149098940187, y = 0.6673689702902375 })
        --                (Point2d.fromMeters { x = 3155, y = 705 })
        --                |> Expect.equal Nothing
        --    , test "Collision 5" <|
        --        \_ ->
        --            Collision.circlePoint
        --                (Length.meters 50)
        --                (Point2d.fromMeters { x = 2688.274844814608, y = -264.8824352979353 })
        --                (Vector2d.fromMeters { x = 2.386164841421291, y = -4.186907582583755 })
        --                (Point2d.fromMeters { x = 2190, y = 705 })
        --                |> Expect.equal Nothing
        --    , test "Collision 6" <|
        --        \_ ->
        --            Collision.circlePoint
        --                (Length.meters 50)
        --                (Point2d.fromMeters { x = 5095.590271983716, y = 1045.8563861875934 })
        --                (Vector2d.fromMeters { x = 15.771927614810986, y = 10.07865360826776 })
        --                (Point2d.fromMeters { x = 5935, y = 1640 })
        --                |> Expect.equal Nothing
        --    ]
        ]



-- { p = Point2d { x = 5095.590271983716, y = 1045.8563861875934 }, p2 = Point2d { x = 5935, y = 1640 }, r = Quantity 50, v = Vector2d { x = 15.771927614810986, y = 10.07865360826776 } }
