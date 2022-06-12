module Evergreen.V15.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V15.ColorIndex
import Evergreen.V15.Decal
import Evergreen.V15.Direction2d
import Evergreen.V15.Id
import Evergreen.V15.MatchName
import Evergreen.V15.Point2d
import Evergreen.V15.TextMessage
import Evergreen.V15.Timeline
import Evergreen.V15.User
import Evergreen.V15.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V15.MatchName.MatchName
    , userCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Place
    = Finished (Evergreen.V15.Id.Id Evergreen.V15.Timeline.FrameId)
    | DidNotFinish


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V15.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V15.ColorIndex.ColorIndex
    | SetDecal Evergreen.V15.Decal.Decal
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Evergreen.V15.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V15.MatchName.MatchName
    | SendTextMessage Evergreen.V15.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) Place)


type alias PlayerData =
    { primaryColor : Evergreen.V15.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V15.ColorIndex.ColorIndex
    , decal : Evergreen.V15.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V15.Id.Id Evergreen.V15.User.UserId
    , input : Maybe (Evergreen.V15.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : ServerTime
    , timeline : Evergreen.V15.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V15.MatchName.MatchName
    , owner : Evergreen.V15.Id.Id Evergreen.V15.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V15.Id.Id Evergreen.V15.User.UserId
            , message : Evergreen.V15.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) Place)
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V15.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V15.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V15.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V15.Id.Id Evergreen.V15.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) Player
    }
