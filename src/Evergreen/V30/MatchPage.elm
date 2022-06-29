module Evergreen.V30.MatchPage exposing (..)

import AssocList
import Effect.WebGL
import Evergreen.V30.ColorIndex
import Evergreen.V30.Decal
import Evergreen.V30.Id
import Evergreen.V30.Match
import Evergreen.V30.MatchName
import Evergreen.V30.NetworkModel
import Evergreen.V30.Point2d
import Evergreen.V30.TextMessage
import Evergreen.V30.Timeline
import Evergreen.V30.User
import Html.Events.Extra.Touch
import Math.Vector2
import Math.Vector3
import Pixels


type WorldPixel
    = WorldPixel Never


type MatchId
    = LobbyId Never


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V30.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V30.ColorIndex.ColorIndex
    | PressedDecal (Maybe Evergreen.V30.Decal.Decal)
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V30.Match.PlayerMode
    | PressedSaveMatchName Evergreen.V30.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V30.TextMessage.TextMessage
    | TypedMaxPlayers String
    | PressedSaveMaxPlayers Int
    | PressedResetMaxPlayers
    | ScrolledToBottom
    | PointerDown Html.Events.Extra.Touch.Event
    | PointerUp
    | PointerMoved Html.Events.Extra.Touch.Event


type alias MatchSetupLocal_ =
    { matchName : String
    , message : String
    , maxPlayers : String
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchActiveLocal_ =
    { timelineCache : Result Evergreen.V30.Timeline.Error (Evergreen.V30.Timeline.TimelineCache Evergreen.V30.Match.MatchState)
    , userIds : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , touchPosition : Maybe (Evergreen.V30.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V30.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_


type alias Model =
    { lobbyId : Evergreen.V30.Id.Id MatchId
    , networkModel :
        Evergreen.V30.NetworkModel.NetworkModel
            { userId : Evergreen.V30.Id.Id Evergreen.V30.User.UserId
            , msg : Evergreen.V30.Match.Msg
            }
            Evergreen.V30.Match.Match
    , matchData : MatchLocalOnly
    }


type ToBackend
    = MatchSetupRequest (Evergreen.V30.Id.Id MatchId) (Evergreen.V30.Id.Id Evergreen.V30.NetworkModel.EventId) Evergreen.V30.Match.Msg


type ToFrontend
    = MatchSetupBroadcast (Evergreen.V30.Id.Id MatchId) (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) Evergreen.V30.Match.Msg
    | MatchSetupResponse (Evergreen.V30.Id.Id MatchId) (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) Evergreen.V30.Match.Msg (Evergreen.V30.Id.Id Evergreen.V30.NetworkModel.EventId)
