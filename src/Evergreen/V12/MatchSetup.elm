module Evergreen.V12.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V12.ColorIndex
import Evergreen.V12.Decal
import Evergreen.V12.Direction2d
import Evergreen.V12.Id
import Evergreen.V12.MatchName
import Evergreen.V12.Point2d
import Evergreen.V12.TextMessage
import Evergreen.V12.Timeline
import Evergreen.V12.User
import Evergreen.V12.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V12.MatchName.MatchName
    , userCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Place
    = Finished (Evergreen.V12.Id.Id Evergreen.V12.Timeline.FrameId)
    | DidNotFinish


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V12.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V12.ColorIndex.ColorIndex
    | SetDecal Evergreen.V12.Decal.Decal
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Evergreen.V12.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V12.MatchName.MatchName
    | SendTextMessage Evergreen.V12.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) Place)


type alias PlayerData =
    { primaryColor : Evergreen.V12.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V12.ColorIndex.ColorIndex
    , decal : Evergreen.V12.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
    , input : Maybe (Evergreen.V12.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : ServerTime
    , timeline : Evergreen.V12.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V12.MatchName.MatchName
    , owner : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
            , message : Evergreen.V12.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) Place)
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V12.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V12.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V12.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V12.Id.Id Evergreen.V12.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) Player
    }
