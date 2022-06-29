module Evergreen.V30.Match exposing (..)

import Angle
import AssocList
import Evergreen.V30.ColorIndex
import Evergreen.V30.Decal
import Evergreen.V30.Direction2d
import Evergreen.V30.Id
import Evergreen.V30.MatchName
import Evergreen.V30.Point2d
import Evergreen.V30.TextMessage
import Evergreen.V30.Timeline
import Evergreen.V30.User
import Evergreen.V30.Vector2d
import Length
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V30.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Emote
    = SurpriseEmote
    | ImpEmote


type alias Input =
    { movement : Maybe (Evergreen.V30.Direction2d.Direction2d WorldCoordinate)
    , emote : Maybe Emote
    }


type Place
    = Finished (Evergreen.V30.Id.Id Evergreen.V30.Timeline.FrameId)
    | DidNotFinish


type Msg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V30.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V30.ColorIndex.ColorIndex
    | SetDecal (Maybe Evergreen.V30.Decal.Decal)
    | SetPlayerMode PlayerMode
    | StartMatch ServerTime
    | MatchInputRequest ServerTime Input
    | SetMatchName Evergreen.V30.MatchName.MatchName
    | SendTextMessage Evergreen.V30.TextMessage.TextMessage
    | MatchFinished (AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) Place)
    | SetMaxPlayers Int


type alias PlayerData =
    { primaryColor : Evergreen.V30.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V30.ColorIndex.ColorIndex
    , decal : Maybe Evergreen.V30.Decal.Decal
    , mode : PlayerMode
    }


type alias TimelineEvent =
    { userId : Evergreen.V30.Id.Id Evergreen.V30.User.UserId
    , input : Input
    }


type alias MatchActive =
    { startTime : ServerTime
    , timeline : Evergreen.V30.Timeline.Timeline TimelineEvent
    }


type alias Match_ =
    { name : Evergreen.V30.MatchName.MatchName
    , owner : Evergreen.V30.Id.Id Evergreen.V30.User.UserId
    , ownerPlayerData : PlayerData
    , users : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) PlayerData
    , matchActive : Maybe MatchActive
    , messages :
        List
            { userId : Evergreen.V30.Id.Id Evergreen.V30.User.UserId
            , message : Evergreen.V30.TextMessage.TextMessage
            }
    , previousMatch : Maybe (AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) Place)
    , maxPlayers : Int
    }


type Match
    = Match Match_


type alias Player =
    { position : Evergreen.V30.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V30.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V30.Direction2d.Direction2d WorldCoordinate)
    , finishTime : Place
    , lastCollision : Maybe (Evergreen.V30.Id.Id Evergreen.V30.Timeline.FrameId)
    , lastEmote :
        Maybe
            { time : Evergreen.V30.Id.Id Evergreen.V30.Timeline.FrameId
            , emote : Emote
            }
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) Player
    }
