module Match exposing (EventId, Match, entities, init, move, step, tankLength, tankRadius, users)

import Acceleration
import Angle
import Block3d exposing (Block3d)
import Color
import Cylinder3d
import Direction3d exposing (Direction3d)
import Duration exposing (Duration, Seconds)
import Force exposing (Newtons)
import Frame3d
import Id exposing (Id)
import IdDict exposing (IdDict)
import Keyboard.Arrows
import Length exposing (Length, Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World
import Point3d exposing (Point3d)
import Quantity exposing (Product, Quantity)
import Scene3d.Entity
import Scene3d.Material
import User exposing (UserId)
import Vector3d


type Match
    = Match
        { world : Physics.World.World (Maybe (Id UserId))
        , users : IdDict UserId UserData
        }


type EventId
    = EventId Never


type alias UserData =
    { move : Keyboard.Arrows.Direction }


init : List (Id UserId) -> Match
init users_ =
    Match
        { world =
            users_
                |> List.map tank
                |> List.foldl Physics.World.add Physics.World.empty
                |> Physics.World.add
                    (Body.block floor Nothing |> Body.withBehavior Body.static)
                |> Physics.World.withGravity (Acceleration.metersPerSecondSquared 9.8) Direction3d.negativeZ
        , users =
            List.map (\a -> ( a, () )) users_
                |> IdDict.fromList
                |> IdDict.map (\_ _ -> { move = Keyboard.Arrows.NoDirection })
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


stepSize : Duration
stepSize =
    Quantity.multiplyBy (1 / 60) Duration.second


acceleration : Quantity Float (Product Newtons Seconds)
acceleration =
    Quantity.times stepSize (Force.newtons 50)


keyToDirection : Keyboard.Arrows.Direction -> Maybe (Direction3d WorldCoordinates)
keyToDirection direction =
    (case direction of
        Keyboard.Arrows.North ->
            Just 6

        Keyboard.Arrows.NorthEast ->
            Just 5

        Keyboard.Arrows.East ->
            Just 4

        Keyboard.Arrows.SouthEast ->
            Just 3

        Keyboard.Arrows.South ->
            Just 2

        Keyboard.Arrows.SouthWest ->
            Just 1

        Keyboard.Arrows.West ->
            Just 0

        Keyboard.Arrows.NorthWest ->
            Just 7

        Keyboard.Arrows.NoDirection ->
            Nothing
    )
        |> Maybe.map ((*) 45 >> Angle.degrees >> Direction3d.xy)


step : Match -> Match
step (Match match) =
    { match
        | world =
            Physics.World.update
                (\body ->
                    case Body.data body of
                        Just userId ->
                            case currentMove userId (Match match) of
                                Just keyDirection ->
                                    case keyToDirection keyDirection of
                                        Just direction ->
                                            Body.applyImpulse
                                                acceleration
                                                direction
                                                (tankBase body)
                                                body

                                        Nothing ->
                                            body

                                Nothing ->
                                    body

                        Nothing ->
                            body
                )
                match.world
                |> Physics.World.simulate stepSize
    }
        |> Match


tankBase : Body a -> Point3d Meters WorldCoordinates
tankBase body =
    Body.originPoint body
        |> Point3d.translateBy
            (Body.frame body
                |> Frame3d.zDirection
                |> Vector3d.withLength (Quantity.multiplyBy -0.5 tankLength)
            )


currentMove : Id UserId -> Match -> Maybe Keyboard.Arrows.Direction
currentMove userId (Match match) =
    IdDict.get userId match.users |> Maybe.map .move


floorWidth =
    Length.meters 8


floor : Block3d Length.Meters coordinates
floor =
    Block3d.centeredOn
        (Frame3d.atPoint (Point3d.meters 0 0 -0.5))
        ( floorWidth, floorWidth, Length.meters 1 )


tank : Id UserId -> Body.Body (Maybe (Id UserId))
tank userId =
    Body.cylinder
        12
        tankModel
        (Just userId)
        |> Body.moveTo (Point3d.xyz (Length.meters 0.5) (Length.meters 2) tankLength)
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 2))


tankRadius : Length
tankRadius =
    Length.meters 0.15


tankLength : Length
tankLength =
    Length.meters 0.8


tankModel =
    Cylinder3d.centeredOn Point3d.origin Direction3d.z { radius = tankRadius, length = tankLength }


entity : Body (Maybe (Id UserId)) -> Maybe (Scene3d.Entity.Entity WorldCoordinates)
entity body =
    case Body.data body of
        Just tankData ->
            Scene3d.Entity.cylinder
                True
                True
                (Scene3d.Material.matte (Color.rgb 0.4 0.6 0.4))
                tankModel
                |> Scene3d.Entity.placeIn (Body.frame body)
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
