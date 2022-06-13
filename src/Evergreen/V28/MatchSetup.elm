module Evergreen.V28.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V28.ColorIndex
import Evergreen.V28.Decal
import Evergreen.V28.Direction2d
import Evergreen.V28.Id
import Evergreen.V28.MatchName
import Evergreen.V28.Point2d
import Evergreen.V28.TextMessage
import Evergreen.V28.Timeline
import Evergreen.V28.User
import Evergreen.V28.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V28.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Place
    = Finished (Evergreen.V28.Id.Id Evergreen.V28.Timeline.FrameId)
    | DidNotFinish


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V28.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V28.ColorIndex.ColorIndex
    | SetDecal (Maybe Evergreen.V28.Decal.Decal)
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Evergreen.V28.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V28.MatchName.MatchName
    | SendTextMessage Evergreen.V28.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V28.Id.Id Evergreen.V28.User.UserId) Place)
    | SetMaxPlayers Int


type alias PlayerData =
    { primaryColor : Evergreen.V28.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V28.ColorIndex.ColorIndex
    , decal : Maybe Evergreen.V28.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V28.Id.Id Evergreen.V28.User.UserId
    , input : Maybe (Evergreen.V28.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : ServerTime
    , timeline : Evergreen.V28.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V28.MatchName.MatchName
    , owner : Evergreen.V28.Id.Id Evergreen.V28.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V28.Id.Id Evergreen.V28.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V28.Id.Id Evergreen.V28.User.UserId
            , message : Evergreen.V28.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V28.Id.Id Evergreen.V28.User.UserId) Place)
    , maxPlayers : Int
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V28.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V28.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V28.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V28.Id.Id Evergreen.V28.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V28.Id.Id Evergreen.V28.User.UserId) Player
    }
