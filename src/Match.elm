module Match exposing (Match, entities, init, move, step, tankLength, tankRadius, users)

import Block3d exposing (Block3d)
import Color
import Cylinder3d
import Direction3d
import Duration
import Frame3d
import Id exposing (Id)
import IdDict exposing (IdDict)
import Keyboard.Arrows
import Length exposing (Length)
import Physics.Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World
import Point3d
import Quantity
import Scene3d.Entity
import Scene3d.Material
import User exposing (UserId)


type Match
    = Match { world : Physics.World.World (Maybe (Id UserId)), users : IdDict UserId UserData }


type alias UserData =
    { move : Keyboard.Arrows.Direction }


init : IdDict UserId () -> Match
init users_ =
    Match
        { world =
            IdDict.toList users_
                |> List.map (Tuple.first >> tank)
                |> List.foldl Physics.World.add Physics.World.empty
                |> Physics.World.add (Physics.Body.block floor Nothing)
        , users = IdDict.map (\_ _ -> { move = Keyboard.Arrows.NoDirection }) users_
        }


users : Match -> IdDict UserId ()
users (Match match) =
    match.users |> IdDict.map (\_ _ -> ())


move : Id UserId -> Keyboard.Arrows.Direction -> Match -> Match
move userId direction (Match match) =
    { match
        | users =
            IdDict.update
                userId
                (Maybe.map (\userData -> { userData | move = direction }))
                match.users
    }
        |> Match


step : Match -> Match
step (Match match) =
    { match
        | world = Physics.World.simulate (Quantity.multiplyBy (1 / 60) Duration.second) match.world
    }
        |> Match


floorWidth =
    Length.meters 8


floor : Block3d Length.Meters coordinates
floor =
    Block3d.centeredOn
        (Frame3d.atPoint (Point3d.meters 0 0 -0.5))
        ( floorWidth, floorWidth, Length.meters 1 )


tank : Id UserId -> Physics.Body.Body (Maybe (Id UserId))
tank userId =
    Physics.Body.block
        (Block3d.centeredOn
            Frame3d.atOrigin
            ( Quantity.multiplyBy 2 tankRadius, Quantity.multiplyBy 2 tankRadius, tankLength )
        )
        (Just userId)
        |> Physics.Body.moveTo (Point3d.xyz (Length.meters 0.5) (Length.meters 2) tankLength)


tankRadius : Length
tankRadius =
    Length.meters 0.15


tankLength : Length
tankLength =
    Length.meters 0.8


entity : Body (Maybe (Id UserId)) -> Maybe (Scene3d.Entity.Entity WorldCoordinates)
entity body =
    case Physics.Body.data body of
        Just tankData ->
            Scene3d.Entity.cylinder
                True
                True
                (Scene3d.Material.matte (Color.rgb 0.4 0.6 0.4))
                (Cylinder3d.centeredOn
                    Point3d.origin
                    Direction3d.z
                    { radius = tankRadius, length = tankLength }
                )
                |> Scene3d.Entity.placeIn (Physics.Body.frame body)
                |> Just

        Nothing ->
            Scene3d.Entity.block
                True
                True
                (Scene3d.Material.matte (Color.rgb 0.6 0.6 0.6))
                floor
                |> Just


entities : Match -> List (Scene3d.Entity.Entity WorldCoordinates)
entities (Match { world }) =
    Physics.World.bodies world |> List.filterMap entity
