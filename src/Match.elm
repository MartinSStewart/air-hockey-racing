module Match exposing (Match, init)

import Block3d
import Frame3d
import Id exposing (Id)
import IdDict exposing (IdDict)
import Length
import Physics.Body
import Physics.World
import User exposing (UserId)


type Match
    = Match { world : Physics.World.World (Id UserId) }


init : IdDict UserId () -> Match
init users =
    Match
        { world =
            IdDict.toList users
                |> List.map (Tuple.first >> tank)
                |> List.foldl Physics.World.add Physics.World.empty
        }


tank : Id UserId -> Physics.Body.Body (Id UserId)
tank userId =
    Physics.Body.block
        (Block3d.centeredOn
            Frame3d.atOrigin
            ( Length.meters 1, Length.meters 1, Length.meters 1 )
        )
        userId
