module Match exposing (Match, entities, init, tankLength, tankRadius)

import Block3d exposing (Block3d)
import Color
import Cylinder3d
import Direction3d
import Frame3d
import Id exposing (Id)
import IdDict exposing (IdDict)
import Length
import Physics.Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World
import Point3d
import Quantity
import Scene3d.Entity
import Scene3d.Material
import User exposing (UserId)


type Match
    = Match { world : Physics.World.World (Maybe (Id UserId)) }


init : IdDict UserId () -> Match
init users =
    Match
        { world =
            IdDict.toList users
                |> List.map (Tuple.first >> tank)
                |> List.foldl Physics.World.add Physics.World.empty
                |> Physics.World.add (Physics.Body.block floor Nothing)
        }


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


tankRadius =
    Length.meters 0.15


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
