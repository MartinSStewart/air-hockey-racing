module Match exposing (Match, init)

import IdDict exposing (IdDict)
import Physics.World
import User exposing (UserId)


type Match
    = Match { world : Physics.World.World () }


init : IdDict UserId () -> Match
init users =
    Match { world = Physics.World.empty }
