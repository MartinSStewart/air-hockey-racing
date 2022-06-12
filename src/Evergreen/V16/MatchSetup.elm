module Evergreen.V16.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V16.ColorIndex
import Evergreen.V16.Decal
import Evergreen.V16.Direction2d
import Evergreen.V16.Id
import Evergreen.V16.MatchName
import Evergreen.V16.Point2d
import Evergreen.V16.TextMessage
import Evergreen.V16.Timeline
import Evergreen.V16.User
import Evergreen.V16.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V16.MatchName.MatchName
    , userCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Place
    = Finished (Evergreen.V16.Id.Id Evergreen.V16.Timeline.FrameId)
    | DidNotFinish


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V16.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V16.ColorIndex.ColorIndex
    | SetDecal Evergreen.V16.Decal.Decal
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Evergreen.V16.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V16.MatchName.MatchName
    | SendTextMessage Evergreen.V16.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) Place)


type alias PlayerData =
    { primaryColor : Evergreen.V16.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V16.ColorIndex.ColorIndex
    , decal : Evergreen.V16.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V16.Id.Id Evergreen.V16.User.UserId
    , input : Maybe (Evergreen.V16.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : ServerTime
    , timeline : Evergreen.V16.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V16.MatchName.MatchName
    , owner : Evergreen.V16.Id.Id Evergreen.V16.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V16.Id.Id Evergreen.V16.User.UserId
            , message : Evergreen.V16.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) Place)
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V16.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V16.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V16.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V16.Id.Id Evergreen.V16.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) Player
    }
