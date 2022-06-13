module Evergreen.V27.MatchSetup exposing (..)

import Angle
import AssocList
import Evergreen.V27.ColorIndex
import Evergreen.V27.Decal
import Evergreen.V27.Direction2d
import Evergreen.V27.Id
import Evergreen.V27.MatchName
import Evergreen.V27.Point2d
import Evergreen.V27.TextMessage
import Evergreen.V27.Timeline
import Evergreen.V27.User
import Evergreen.V27.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V27.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Place
    = Finished (Evergreen.V27.Id.Id Evergreen.V27.Timeline.FrameId)
    | DidNotFinish


type MatchSetupMsg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V27.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V27.ColorIndex.ColorIndex
    | SetDecal (Maybe Evergreen.V27.Decal.Decal)
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime (Maybe (Evergreen.V27.Direction2d.Direction2d WorldCoordinate))
    | SetMatchName Evergreen.V27.MatchName.MatchName
    | SendTextMessage Evergreen.V27.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) Place)
    | SetMaxPlayers Int


type alias PlayerData =
    { primaryColor : Evergreen.V27.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V27.ColorIndex.ColorIndex
    , decal : Maybe Evergreen.V27.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V27.Id.Id Evergreen.V27.User.UserId
    , input : Maybe (Evergreen.V27.Direction2d.Direction2d WorldCoordinate)
    }


type alias Match =
    { startTime : ServerTime
    , timeline : Evergreen.V27.Timeline.Timeline TimelineEvent
    }


type alias MatchSetupData =
    { name : Evergreen.V27.MatchName.MatchName
    , owner : Evergreen.V27.Id.Id Evergreen.V27.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) PlayerData
    , match : Maybe Match
    , messages :
        List
            { userId : Evergreen.V27.Id.Id Evergreen.V27.User.UserId
            , message : Evergreen.V27.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) Place)
    , maxPlayers : Int
    }


type MatchSetup
    = MatchSetup MatchSetupData


type alias Player =
    { position : Evergreen.V27.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V27.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V27.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V27.Id.Id Evergreen.V27.Timeline.FrameId)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) Player
    }
