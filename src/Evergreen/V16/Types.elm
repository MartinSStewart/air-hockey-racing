module Evergreen.V16.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V16.Audio
import Evergreen.V16.ColorIndex
import Evergreen.V16.Decal
import Evergreen.V16.Id
import Evergreen.V16.Keyboard
import Evergreen.V16.MatchName
import Evergreen.V16.MatchSetup
import Evergreen.V16.NetworkModel
import Evergreen.V16.Point2d
import Evergreen.V16.Sounds
import Evergreen.V16.TextMessage
import Evergreen.V16.Timeline
import Evergreen.V16.User
import Html.Events.Extra.Touch
import Math.Vector2
import Math.Vector3
import Pixels
import Quantity
import Url


type alias WindowSize =
    { width : Quantity.Quantity Int Pixels.Pixels
    , height : Quantity.Quantity Int Pixels.Pixels
    }


type WorldPixel
    = WorldPixel Never


type LobbyId
    = LobbyId Never


type MatchMsg
    = PointerDown Html.Events.Extra.Touch.Event
    | PointerUp Html.Events.Extra.Touch.Event
    | PointerMoved Html.Events.Extra.Touch.Event


type MatchSetupMsg_
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V16.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V16.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V16.Decal.Decal
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V16.MatchSetup.PlayerMode
    | PressedSaveMatchName Evergreen.V16.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V16.TextMessage.TextMessage


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | KeyMsg Evergreen.V16.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V16.Id.Id LobbyId)
    | SoundLoaded String (Result Evergreen.V16.Audio.LoadError Evergreen.V16.Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Effect.Time.Posix
    | ScrolledToBottom
    | RandomInput Effect.Time.Posix


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V16.Id.Id LobbyId) Evergreen.V16.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V16.Id.Id Evergreen.V16.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V16.Audio.LoadError Evergreen.V16.Audio.Source)
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { timelineCache : Evergreen.V16.Timeline.TimelineCache Evergreen.V16.MatchSetup.MatchState
    , userIds : AssocList.Dict (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , touchPosition : Maybe (Evergreen.V16.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V16.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchData
    = MatchSetupData
        { matchName : String
        , message : String
        }
    | MatchData MatchPage_


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V16.Id.Id LobbyId
    , networkModel :
        Evergreen.V16.NetworkModel.NetworkModel
            { userId : Evergreen.V16.Id.Id Evergreen.V16.User.UserId
            , msg : Evergreen.V16.MatchSetup.MatchSetupMsg
            }
            Evergreen.V16.MatchSetup.MatchSetup
    , matchData : MatchData
    }


type Page
    = LobbyPage LobbyData
    | MatchPage MatchSetupPage_


type alias PingData =
    { roundTripTime : Duration.Duration
    , serverTime : Effect.Time.Posix
    , sendTime : Effect.Time.Posix
    , receiveTime : Effect.Time.Posix
    , lowEstimate : Duration.Duration
    , highEstimate : Duration.Duration
    , pingCount : Int
    }


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , currentKeys : List Evergreen.V16.Keyboard.Key
    , previousKeys : List Evergreen.V16.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V16.Sounds.Sounds
    , userId : Evergreen.V16.Id.Id Evergreen.V16.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V16.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V16.Id.Id Evergreen.V16.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V16.Id.Id LobbyId) Evergreen.V16.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V16.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Evergreen.V16.Id.Id LobbyId) (Evergreen.V16.Id.Id Evergreen.V16.NetworkModel.EventId) Evergreen.V16.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V16.MatchSetup.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V16.MatchSetup.ServerTime


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V16.Id.Id LobbyId) Evergreen.V16.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V16.Id.Id LobbyId)
    | CreateLobbyBroadcast (Evergreen.V16.Id.Id LobbyId) Evergreen.V16.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V16.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V16.MatchSetup.MatchSetup)
    | PingResponse Evergreen.V16.MatchSetup.ServerTime
    | MatchSetupBroadcast (Evergreen.V16.Id.Id LobbyId) (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) Evergreen.V16.MatchSetup.MatchSetupMsg
    | MatchSetupResponse (Evergreen.V16.Id.Id LobbyId) (Evergreen.V16.Id.Id Evergreen.V16.User.UserId) Evergreen.V16.MatchSetup.MatchSetupMsg (Maybe LobbyData) (Evergreen.V16.Id.Id Evergreen.V16.NetworkModel.EventId)
