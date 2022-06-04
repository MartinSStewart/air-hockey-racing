module Evergreen.V3.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V3.Audio
import Evergreen.V3.ColorIndex
import Evergreen.V3.Decal
import Evergreen.V3.Id
import Evergreen.V3.Keyboard
import Evergreen.V3.MatchSetup
import Evergreen.V3.NetworkModel
import Evergreen.V3.Point2d
import Evergreen.V3.Sounds
import Evergreen.V3.Timeline
import Evergreen.V3.User
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


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | KeyMsg Evergreen.V3.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V3.Id.Id LobbyId)
    | PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V3.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V3.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V3.Decal.Decal
    | SoundLoaded String (Result Evergreen.V3.Audio.LoadError Evergreen.V3.Audio.Source)
    | MatchMsg MatchMsg
    | GotTime Effect.Time.Posix
    | PressedPlayerMode Evergreen.V3.MatchSetup.PlayerMode


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V3.Id.Id Evergreen.V3.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V3.Audio.LoadError Evergreen.V3.Audio.Source)
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { timelineCache : Evergreen.V3.Timeline.TimelineCache Evergreen.V3.MatchSetup.MatchState
    , userIds : AssocList.Dict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V3.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V3.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V3.Id.Id LobbyId
    , networkModel :
        Evergreen.V3.NetworkModel.NetworkModel
            { userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
            , msg : Evergreen.V3.MatchSetup.MatchSetupMsg
            }
            Evergreen.V3.MatchSetup.MatchSetup
    , matchData : Maybe MatchPage_
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
    , currentKeys : List Evergreen.V3.Keyboard.Key
    , previousKeys : List Evergreen.V3.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V3.Sounds.Sounds
    , userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V3.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V3.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | MatchSetupRequest (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V3.Id.Id LobbyId)
    | CreateLobbyBroadcast (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V3.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V3.MatchSetup.MatchSetup)
    | PingResponse Effect.Time.Posix
    | MatchSetupBroadcast (Evergreen.V3.Id.Id LobbyId) (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) Evergreen.V3.MatchSetup.MatchSetupMsg (Maybe LobbyData)
