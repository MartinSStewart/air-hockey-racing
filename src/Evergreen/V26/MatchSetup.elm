module Evergreen.V26.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V26.ColorIndex
import Evergreen.V26.Decal
import Evergreen.V26.Direction2d
import Evergreen.V26.Id
import Evergreen.V26.MatchName
import Evergreen.V26.Point2d
import Evergreen.V26.TextMessage
import Evergreen.V26.Timeline
import Evergreen.V26.User
import Evergreen.V26.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V26.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Place
    = Finished (Evergreen.V26.Id.Id Evergreen.V26.Timeline.FrameId)
    | DidNotFinish


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V26.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V26.ColorIndex.ColorIndex
    | SetDecal (Maybe Evergreen.V26.Decal.Decal)
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Evergreen.V26.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V26.MatchName.MatchName
    | SendTextMessage Evergreen.V26.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) Place)
    | SetMaxPlayers Int


type alias PlayerData =
    { primaryColor : Evergreen.V26.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V26.ColorIndex.ColorIndex
    , decal : Maybe Evergreen.V26.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V26.Id.Id Evergreen.V26.User.UserId
    , input : Maybe (Evergreen.V26.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : ServerTime
    , timeline : Evergreen.V26.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V26.MatchName.MatchName
    , owner : Evergreen.V26.Id.Id Evergreen.V26.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V26.Id.Id Evergreen.V26.User.UserId
            , message : Evergreen.V26.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) Place)
    , maxPlayers : Int
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V26.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V26.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V26.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V26.Id.Id Evergreen.V26.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) Player
    }
