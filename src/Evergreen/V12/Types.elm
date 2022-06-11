module Evergreen.V12.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V12.Audio
import Evergreen.V12.ColorIndex
import Evergreen.V12.Decal
import Evergreen.V12.Id
import Evergreen.V12.Keyboard
import Evergreen.V12.MatchName
import Evergreen.V12.MatchSetup
import Evergreen.V12.NetworkModel
import Evergreen.V12.Point2d
import Evergreen.V12.Sounds
import Evergreen.V12.TextMessage
import Evergreen.V12.Timeline
import Evergreen.V12.User
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
    | PressedPrimaryColor Evergreen.V12.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V12.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V12.Decal.Decal
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V12.MatchSetup.PlayerMode
    | PressedSaveMatchName Evergreen.V12.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V12.TextMessage.TextMessage


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | KeyMsg Evergreen.V12.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V12.Id.Id LobbyId)
    | SoundLoaded String (Result Evergreen.V12.Audio.LoadError Evergreen.V12.Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Effect.Time.Posix
    | ScrolledToBottom


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V12.Id.Id LobbyId) Evergreen.V12.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V12.Id.Id Evergreen.V12.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V12.Audio.LoadError Evergreen.V12.Audio.Source)
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { timelineCache : Evergreen.V12.Timeline.TimelineCache Evergreen.V12.MatchSetup.MatchState
    , userIds : AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V12.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V12.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchData
    = MatchSetupData
        { matchName : String
        , message : String
        }
    | MatchData MatchPage_


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V12.Id.Id LobbyId
    , networkModel :
        Evergreen.V12.NetworkModel.NetworkModel
            { userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
            , msg : Evergreen.V12.MatchSetup.MatchSetupMsg
            }
            Evergreen.V12.MatchSetup.MatchSetup
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
    , currentKeys : List Evergreen.V12.Keyboard.Key
    , previousKeys : List Evergreen.V12.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V12.Sounds.Sounds
    , userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V12.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V12.Id.Id LobbyId) Evergreen.V12.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V12.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Evergreen.V12.Id.Id LobbyId) (Evergreen.V12.Id.Id Evergreen.V12.NetworkModel.EventId) Evergreen.V12.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V12.MatchSetup.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V12.MatchSetup.ServerTime


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V12.Id.Id LobbyId) Evergreen.V12.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V12.Id.Id LobbyId)
    | CreateLobbyBroadcast (Evergreen.V12.Id.Id LobbyId) Evergreen.V12.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V12.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V12.MatchSetup.MatchSetup)
    | PingResponse Evergreen.V12.MatchSetup.ServerTime
    | MatchSetupBroadcast (Evergreen.V12.Id.Id LobbyId) (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) Evergreen.V12.MatchSetup.MatchSetupMsg
    | MatchSetupResponse (Evergreen.V12.Id.Id LobbyId) (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) Evergreen.V12.MatchSetup.MatchSetupMsg (Maybe LobbyData) (Evergreen.V12.Id.Id Evergreen.V12.NetworkModel.EventId)
