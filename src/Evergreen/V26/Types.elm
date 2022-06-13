module Evergreen.V26.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V26.Audio
import Evergreen.V26.ColorIndex
import Evergreen.V26.Decal
import Evergreen.V26.Id
import Evergreen.V26.Keyboard
import Evergreen.V26.MatchName
import Evergreen.V26.MatchSetup
import Evergreen.V26.NetworkModel
import Evergreen.V26.Point2d
import Evergreen.V26.Sounds
import Evergreen.V26.TextMessage
import Evergreen.V26.Timeline
import Evergreen.V26.User
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
    | PressedPrimaryColor Evergreen.V26.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V26.ColorIndex.ColorIndex
    | PressedDecal (Maybe Evergreen.V26.Decal.Decal)
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V26.MatchSetup.PlayerMode
    | PressedSaveMatchName Evergreen.V26.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V26.TextMessage.TextMessage
    | TypedMaxPlayers String
    | PressedSaveMaxPlayers Int
    | PressedResetMaxPlayers


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | KeyMsg Evergreen.V26.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V26.Id.Id LobbyId)
    | SoundLoaded String (Result Evergreen.V26.Audio.LoadError Evergreen.V26.Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Effect.Time.Posix
    | ScrolledToBottom
    | RandomInput Effect.Time.Posix


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V26.Id.Id LobbyId) Evergreen.V26.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V26.Id.Id Evergreen.V26.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V26.Audio.LoadError Evergreen.V26.Audio.Source)
    }


type JoinLobbyError
    = LobbyNotFound
    | LobbyFull


type alias LobbyPage_ =
    { lobbies : AssocList.Dict (Evergreen.V26.Id.Id LobbyId) Evergreen.V26.MatchSetup.LobbyPreview
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
    { timelineCache : Result Evergreen.V26.Timeline.Error (Evergreen.V26.Timeline.TimelineCache Evergreen.V26.MatchSetup.MatchState)
    , userIds : AssocList.Dict (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , touchPosition : Maybe (Evergreen.V26.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V26.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchData
    = MatchSetupData MatchSetupData_
    | MatchData MatchPage_


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V26.Id.Id LobbyId
    , networkModel :
        Evergreen.V26.NetworkModel.NetworkModel
            { userId : Evergreen.V26.Id.Id Evergreen.V26.User.UserId
            , msg : Evergreen.V26.MatchSetup.MatchSetupMsg
            }
            Evergreen.V26.MatchSetup.MatchSetup
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
    , currentKeys : List Evergreen.V26.Keyboard.Key
    , previousKeys : List Evergreen.V26.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V26.Sounds.Sounds
    , userId : Evergreen.V26.Id.Id Evergreen.V26.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V26.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V26.Id.Id Evergreen.V26.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V26.Id.Id LobbyId) Evergreen.V26.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V26.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Evergreen.V26.Id.Id LobbyId) (Evergreen.V26.Id.Id Evergreen.V26.NetworkModel.EventId) Evergreen.V26.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V26.MatchSetup.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V26.MatchSetup.ServerTime


type ToFrontend
    = CreateLobbyResponse (Evergreen.V26.Id.Id LobbyId) Evergreen.V26.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V26.Id.Id LobbyId)
    | UpdateLobbyBroadcast (Evergreen.V26.Id.Id LobbyId) Evergreen.V26.MatchSetup.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V26.Id.Id LobbyId) Evergreen.V26.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V26.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V26.MatchSetup.MatchSetup)
    | PingResponse Evergreen.V26.MatchSetup.ServerTime
    | MatchSetupBroadcast (Evergreen.V26.Id.Id LobbyId) (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) Evergreen.V26.MatchSetup.MatchSetupMsg
    | MatchSetupResponse (Evergreen.V26.Id.Id LobbyId) (Evergreen.V26.Id.Id Evergreen.V26.User.UserId) Evergreen.V26.MatchSetup.MatchSetupMsg (Maybe LobbyData) (Evergreen.V26.Id.Id Evergreen.V26.NetworkModel.EventId)
