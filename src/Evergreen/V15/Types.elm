module Evergreen.V15.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V15.Audio
import Evergreen.V15.ColorIndex
import Evergreen.V15.Decal
import Evergreen.V15.Id
import Evergreen.V15.Keyboard
import Evergreen.V15.MatchName
import Evergreen.V15.MatchSetup
import Evergreen.V15.NetworkModel
import Evergreen.V15.Point2d
import Evergreen.V15.Sounds
import Evergreen.V15.TextMessage
import Evergreen.V15.Timeline
import Evergreen.V15.User
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
    | PressedPrimaryColor Evergreen.V15.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V15.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V15.Decal.Decal
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V15.MatchSetup.PlayerMode
    | PressedSaveMatchName Evergreen.V15.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V15.TextMessage.TextMessage


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | KeyMsg Evergreen.V15.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V15.Id.Id LobbyId)
    | SoundLoaded String (Result Evergreen.V15.Audio.LoadError Evergreen.V15.Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Effect.Time.Posix
    | ScrolledToBottom


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V15.Id.Id LobbyId) Evergreen.V15.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V15.Id.Id Evergreen.V15.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V15.Audio.LoadError Evergreen.V15.Audio.Source)
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { timelineCache : Evergreen.V15.Timeline.TimelineCache Evergreen.V15.MatchSetup.MatchState
    , userIds : AssocList.Dict (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , touchPosition : Maybe (Evergreen.V15.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V15.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchData
    = MatchSetupData
        { matchName : String
        , message : String
        }
    | MatchData MatchPage_


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V15.Id.Id LobbyId
    , networkModel :
        Evergreen.V15.NetworkModel.NetworkModel
            { userId : Evergreen.V15.Id.Id Evergreen.V15.User.UserId
            , msg : Evergreen.V15.MatchSetup.MatchSetupMsg
            }
            Evergreen.V15.MatchSetup.MatchSetup
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
    , currentKeys : List Evergreen.V15.Keyboard.Key
    , previousKeys : List Evergreen.V15.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V15.Sounds.Sounds
    , userId : Evergreen.V15.Id.Id Evergreen.V15.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V15.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V15.Id.Id Evergreen.V15.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V15.Id.Id LobbyId) Evergreen.V15.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V15.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Evergreen.V15.Id.Id LobbyId) (Evergreen.V15.Id.Id Evergreen.V15.NetworkModel.EventId) Evergreen.V15.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V15.MatchSetup.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V15.MatchSetup.ServerTime


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V15.Id.Id LobbyId) Evergreen.V15.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V15.Id.Id LobbyId)
    | CreateLobbyBroadcast (Evergreen.V15.Id.Id LobbyId) Evergreen.V15.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V15.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V15.MatchSetup.MatchSetup)
    | PingResponse Evergreen.V15.MatchSetup.ServerTime
    | MatchSetupBroadcast (Evergreen.V15.Id.Id LobbyId) (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) Evergreen.V15.MatchSetup.MatchSetupMsg
    | MatchSetupResponse (Evergreen.V15.Id.Id LobbyId) (Evergreen.V15.Id.Id Evergreen.V15.User.UserId) Evergreen.V15.MatchSetup.MatchSetupMsg (Maybe LobbyData) (Evergreen.V15.Id.Id Evergreen.V15.NetworkModel.EventId)
