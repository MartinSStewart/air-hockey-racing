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
        ]
