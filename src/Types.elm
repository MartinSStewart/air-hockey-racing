module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel
    , FrontendModel_(..)
    , FrontendMsg
    , FrontendMsg_(..)
    , JoinLobbyError(..)
    , LobbyData
    , LobbyId
    , MatchData(..)
    , MatchMsg(..)
    , MatchPage_
    , MatchSetupMsg_(..)
    , MatchSetupPage_
    , Page(..)
    , PingData
    , ScreenCoordinate
    , ToBackend(..)
    , ToFrontend(..)
    , Vertex
    , WindowSize
    , WorldPixel
    )

import AssocList exposing (Dict)
import Audio
import Browser
import ColorIndex exposing (ColorIndex)
import Decal exposing (Decal)
import Duration exposing (Duration)
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Effect.WebGL exposing (Mesh)
import Html.Events.Extra.Touch
import Id exposing (Id)
import Keyboard
import MatchName exposing (MatchName)
import MatchSetup exposing (LobbyPreview, MatchSetup, MatchSetupMsg, MatchState, PlayerMode, ServerTime)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import NetworkModel exposing (EventId, NetworkModel)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Sounds exposing (Sounds)
import TextMessage exposing (TextMessage)
import Timeline exposing (FrameId, Timeline, TimelineCache)
import Url exposing (Url)
import User exposing (UserId)


type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type WorldPixel
    = WorldPixel Never


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Maybe Time.Posix
    , debugTimeOffset : Duration
    , initData : Maybe ( Id UserId, LobbyData )
    , sounds : Dict String (Result Audio.LoadError Audio.Source)
    }


type alias WindowSize =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , currentKeys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , debugTimeOffset : Duration
    , page : Page
    , sounds : Sounds
    , userId : Id UserId
    , pingStartTime : Maybe Time.Posix
    , pingData : Maybe PingData
    }


type alias PingData =
    { roundTripTime : Duration
    , serverTime : Time.Posix
    , sendTime : Time.Posix
    , receiveTime : Time.Posix
    , lowEstimate : Duration
    , highEstimate : Duration
    , pingCount : Int
    }


type Page
    = LobbyPage LobbyData
    | MatchPage MatchSetupPage_


type alias MatchSetupPage_ =
    { lobbyId : Id LobbyId
    , networkModel : NetworkModel { userId : Id UserId, msg : MatchSetupMsg } MatchSetup
    , matchData : MatchData
    }


type MatchData
    = MatchSetupData { matchName : String, message : String }
    | MatchData MatchPage_


type alias LobbyData =
    { lobbies : Dict (Id LobbyId) LobbyPreview
    }


type alias MatchPage_ =
    { timelineCache : TimelineCache MatchState
    , userIds : Dict (Id UserId) (Mesh Vertex)
    , wallMesh : Mesh Vertex
    , touchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    }


type alias Vertex =
    { position : Vec2, color : Vec3 }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias BackendModel =
    { userSessions : Dict SessionId { clientIds : Dict ClientId (), userId : Id UserId }
    , users : Dict (Id UserId) BackendUserData
    , lobbies : Dict (Id LobbyId) MatchSetup
    , counter : Int
    }


type alias BackendUserData =
    { name : String }


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | KeyMsg Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Id LobbyId)
    | SoundLoaded String (Result Audio.LoadError Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Time.Posix
    | ScrolledToBottom
    | RandomInput Time.Posix


type MatchSetupMsg_
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor ColorIndex
    | PressedSecondaryColor ColorIndex
    | PressedDecal Decal
    | TypedMatchName String
    | PressedPlayerMode PlayerMode
    | PressedSaveMatchName MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage TextMessage


type MatchMsg
    = PointerDown Html.Events.Extra.Touch.Event
    | PointerUp Html.Events.Extra.Touch.Event
    | PointerMoved Html.Events.Extra.Touch.Event


type LobbyId
    = LobbyId Never


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Id LobbyId) (Id EventId) MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | ClientDisconnectedWithTime SessionId ClientId ServerTime
    | UpdateFromFrontendWithTime SessionId ClientId ToBackend ServerTime


type ToFrontend
    = CreateLobbyResponse (Id LobbyId) MatchSetup
    | RemoveLobbyBroadcast (Id LobbyId)
    | CreateLobbyBroadcast (Id LobbyId) LobbyPreview
    | ClientInit (Id UserId) LobbyData
    | JoinLobbyResponse (Id LobbyId) (Result JoinLobbyError MatchSetup)
    | PingResponse ServerTime
    | MatchSetupBroadcast (Id LobbyId) (Id UserId) MatchSetupMsg
    | MatchSetupResponse (Id LobbyId) (Id UserId) MatchSetupMsg (Maybe LobbyData) (Id EventId)


type JoinLobbyError
    = LobbyNotFound
