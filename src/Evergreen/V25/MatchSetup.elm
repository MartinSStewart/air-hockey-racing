module Evergreen.V25.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V25.ColorIndex
import Evergreen.V25.Decal
import Evergreen.V25.Direction2d
import Evergreen.V25.Id
import Evergreen.V25.MatchName
import Evergreen.V25.Point2d
import Evergreen.V25.TextMessage
import Evergreen.V25.Timeline
import Evergreen.V25.User
import Evergreen.V25.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V25.MatchName.MatchName
    , userCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Place
    = Finished (Evergreen.V25.Id.Id Evergreen.V25.Timeline.FrameId)
    | DidNotFinish


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V25.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V25.ColorIndex.ColorIndex
    | SetDecal (Maybe Evergreen.V25.Decal.Decal)
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Evergreen.V25.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V25.MatchName.MatchName
    | SendTextMessage Evergreen.V25.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V25.Id.Id Evergreen.V25.User.UserId) Place)


type alias PlayerData =
    { primaryColor : Evergreen.V25.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V25.ColorIndex.ColorIndex
    , decal : Maybe Evergreen.V25.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V25.Id.Id Evergreen.V25.User.UserId
    , input : Maybe (Evergreen.V25.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : ServerTime
    , timeline : Evergreen.V25.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V25.MatchName.MatchName
    , owner : Evergreen.V25.Id.Id Evergreen.V25.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V25.Id.Id Evergreen.V25.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V25.Id.Id Evergreen.V25.User.UserId
            , message : Evergreen.V25.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V25.Id.Id Evergreen.V25.User.UserId) Place)
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V25.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V25.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V25.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V25.Id.Id Evergreen.V25.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V25.Id.Id Evergreen.V25.User.UserId) Player
    }
