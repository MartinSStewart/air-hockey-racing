module Evergreen.V27.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V27.Audio
import Evergreen.V27.ColorIndex
import Evergreen.V27.Decal
import Evergreen.V27.Id
import Evergreen.V27.Keyboard
import Evergreen.V27.MatchName
import Evergreen.V27.MatchSetup
import Evergreen.V27.NetworkModel
import Evergreen.V27.Point2d
import Evergreen.V27.Sounds
import Evergreen.V27.TextMessage
import Evergreen.V27.Timeline
import Evergreen.V27.User
import Html.Events.Extra.Touch
import Math.Vector2
import Math.Vector3
import Pixels
import Quantity


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
    | PointerUp
    | PointerMoved Html.Events.Extra.Touch.Event


type MatchSetupMsg_
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V27.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V27.ColorIndex.ColorIndex
    | PressedDecal (Maybe Evergreen.V27.Decal.Decal)
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V27.MatchSetup.PlayerMode
    | PressedSaveMatchName Evergreen.V27.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V27.TextMessage.TextMessage
    | TypedMaxPlayers String
    | PressedSaveMaxPlayers Int
    | PressedResetMaxPlayers


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged
    | KeyMsg Evergreen.V27.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V27.Id.Id LobbyId)
    | SoundLoaded String (Result Evergreen.V27.Audio.LoadError Evergreen.V27.Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Effect.Time.Posix
    | ScrolledToBottom
    | RandomInput Effect.Time.Posix


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V27.Id.Id LobbyId) Evergreen.V27.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V27.Id.Id Evergreen.V27.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V27.Audio.LoadError Evergreen.V27.Audio.Source)
    }


type JoinLobbyError
    = LobbyNotFound
    | LobbyFull


type alias LobbyPage_ =
    { lobbies : AssocList.Dict (Evergreen.V27.Id.Id LobbyId) Evergreen.V27.MatchSetup.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type alias MatchSetupData_ =
    { matchName : String
    , message : String
    , maxPlayers : String
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { timelineCache : Result Evergreen.V27.Timeline.Error (Evergreen.V27.Timeline.TimelineCache Evergreen.V27.MatchSetup.MatchState)
    , userIds : AssocList.Dict (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , touchPosition : Maybe (Evergreen.V27.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V27.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchData
    = MatchSetupData MatchSetupData_
    | MatchData MatchPage_


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V27.Id.Id LobbyId
    , networkModel :
        Evergreen.V27.NetworkModel.NetworkModel
            { userId : Evergreen.V27.Id.Id Evergreen.V27.User.UserId
            , msg : Evergreen.V27.MatchSetup.MatchSetupMsg
            }
            Evergreen.V27.MatchSetup.MatchSetup
    , matchData : MatchData
    }


type Page
    = LobbyPage LobbyPage_
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
    , currentKeys : List Evergreen.V27.Keyboard.Key
    , previousKeys : List Evergreen.V27.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V27.Sounds.Sounds
    , userId : Evergreen.V27.Id.Id Evergreen.V27.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V27.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V27.Id.Id Evergreen.V27.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V27.Id.Id LobbyId) Evergreen.V27.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V27.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Evergreen.V27.Id.Id LobbyId) (Evergreen.V27.Id.Id Evergreen.V27.NetworkModel.EventId) Evergreen.V27.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V27.MatchSetup.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V27.MatchSetup.ServerTime


type ToFrontend
    = CreateLobbyResponse (Evergreen.V27.Id.Id LobbyId) Evergreen.V27.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V27.Id.Id LobbyId)
    | UpdateLobbyBroadcast (Evergreen.V27.Id.Id LobbyId) Evergreen.V27.MatchSetup.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V27.Id.Id LobbyId) Evergreen.V27.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V27.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V27.MatchSetup.MatchSetup)
    | PingResponse Evergreen.V27.MatchSetup.ServerTime
    | MatchSetupBroadcast (Evergreen.V27.Id.Id LobbyId) (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) Evergreen.V27.MatchSetup.MatchSetupMsg
    | MatchSetupResponse (Evergreen.V27.Id.Id LobbyId) (Evergreen.V27.Id.Id Evergreen.V27.User.UserId) Evergreen.V27.MatchSetup.MatchSetupMsg (Maybe LobbyData) (Evergreen.V27.Id.Id Evergreen.V27.NetworkModel.EventId)
