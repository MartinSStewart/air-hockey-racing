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
    , MainLobbyInitData
    , MatchId
    , MatchLobbyLocalOnly_
    , MatchLocalOnly(..)
    , MatchMsg(..)
    , MatchPage_
    , MatchSetupLocalOnly_
    , MatchSetupMsg_(..)
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
import Match exposing (LobbyPreview, MatchSetup, MatchSetupMsg, MatchState, PlayerMode, ServerTime)
import MatchName exposing (MatchName)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import NetworkModel exposing (EventId, NetworkModel)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Sounds exposing (Sounds)
import TextMessage exposing (TextMessage)
import Timeline exposing (TimelineCache)
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
    , initData : Maybe ( Id UserId, MainLobbyInitData )
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
    = MainLobbyPage MainLobbyPage_
    | MatchPage MatchPage_


type alias MatchPage_ =
    { lobbyId : Id MatchId
    , networkModel : NetworkModel { userId : Id UserId, msg : MatchSetupMsg } MatchSetup
    , matchData : MatchLocalOnly
    }


type MatchLocalOnly
    = MatchSetupLocalOnly MatchSetupLocalOnly_
    | MatchLobbyLocalOnly MatchLobbyLocalOnly_


type alias MatchSetupLocalOnly_ =
    { matchName : String, message : String, maxPlayers : String }


type alias MainLobbyPage_ =
    { lobbies : Dict (Id MatchId) LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type alias MainLobbyInitData =
    { lobbies : Dict (Id MatchId) LobbyPreview }


type alias MatchLobbyLocalOnly_ =
    { timelineCache : Result Timeline.Error (TimelineCache MatchState)
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
    , lobbies : Dict (Id MatchId) MatchSetup
    , dummyChange : Float
    , counter : Int
    }


type alias BackendUserData =
    { name : String }


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged
    | KeyMsg Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Id MatchId)
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
    | PressedDecal (Maybe Decal)
    | TypedMatchName String
    | PressedPlayerMode PlayerMode
    | PressedSaveMatchName MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage TextMessage
    | TypedMaxPlayers String
    | PressedSaveMaxPlayers Int
    | PressedResetMaxPlayers


type MatchMsg
    = PointerDown Html.Events.Extra.Touch.Event
    | PointerUp
    | PointerMoved Html.Events.Extra.Touch.Event


type MatchId
    = LobbyId Never


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Id MatchId) (Id EventId) MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | ClientDisconnectedWithTime SessionId ClientId ServerTime
    | UpdateFromFrontendWithTime SessionId ClientId ToBackend ServerTime


type ToFrontend
    = CreateLobbyResponse (Id MatchId) MatchSetup
    | RemoveLobbyBroadcast (Id MatchId)
    | UpdateLobbyBroadcast (Id MatchId) LobbyPreview
    | CreateLobbyBroadcast (Id MatchId) LobbyPreview
    | ClientInit (Id UserId) MainLobbyInitData
    | JoinLobbyResponse (Id MatchId) (Result JoinLobbyError MatchSetup)
    | PingResponse ServerTime
    | MatchSetupBroadcast (Id MatchId) (Id UserId) MatchSetupMsg
    | MatchSetupResponse (Id MatchId) (Id UserId) MatchSetupMsg (Maybe MainLobbyInitData) (Id EventId)


type JoinLobbyError
    = LobbyNotFound
    | LobbyFull
