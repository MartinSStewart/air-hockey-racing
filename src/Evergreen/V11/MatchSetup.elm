module Evergreen.V11.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V11.ColorIndex
import Evergreen.V11.Decal
import Evergreen.V11.Direction2d
import Evergreen.V11.Id
import Evergreen.V11.MatchName
import Evergreen.V11.Point2d
import Evergreen.V11.TextMessage
import Evergreen.V11.Timeline
import Evergreen.V11.User
import Evergreen.V11.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V11.MatchName.MatchName
    , userCount : Int
    }


type WorldCoordinate
    = WorldCoordinate Never


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V11.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V11.ColorIndex.ColorIndex
    | SetDecal Evergreen.V11.Decal.Decal
    | SetPlayerMode PlayerMode
    | StartMatch Time.Posix
    | MatchInputRequest (Evergreen.V11.Id.Id Evergreen.V11.Timeline.FrameId) (Maybe (Evergreen.V11.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V11.MatchName.MatchName
    | SendTextMessage Evergreen.V11.TextMessage.TextMessage
    | MatchFinished


type alias PlayerData =
    { primaryColor : Evergreen.V11.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V11.ColorIndex.ColorIndex
    , decal : Evergreen.V11.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V11.Id.Id Evergreen.V11.User.UserId
    , input : Maybe (Evergreen.V11.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : Time.Posix
    , timeline : Evergreen.V11.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V11.MatchName.MatchName
    , owner : Evergreen.V11.Id.Id Evergreen.V11.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V11.Id.Id Evergreen.V11.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V11.Id.Id Evergreen.V11.User.UserId
            , message : Evergreen.V11.TextMessage.TextMessage
            }
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V11.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V11.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V11.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Maybe (Evergreen.V11.Id.Id Evergreen.V11.Timeline.FrameId)
    , lastCollision : Maybe (Evergreen.V11.Id.Id Evergreen.V11.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V11.Id.Id Evergreen.V11.User.UserId) Player
    }
