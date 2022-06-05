module Evergreen.V7.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V7.Audio
import Evergreen.V7.ColorIndex
import Evergreen.V7.Decal
import Evergreen.V7.Id
import Evergreen.V7.Keyboard
import Evergreen.V7.MatchName
import Evergreen.V7.MatchSetup
import Evergreen.V7.NetworkModel
import Evergreen.V7.Point2d
import Evergreen.V7.Sounds
import Evergreen.V7.Timeline
import Evergreen.V7.User
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
    | PressedPrimaryColor Evergreen.V7.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V7.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V7.Decal.Decal
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V7.MatchSetup.PlayerMode
    | PressedSaveMatchName Evergreen.V7.MatchName.MatchName
    | PressedResetMatchName


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | KeyMsg Evergreen.V7.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V7.Id.Id LobbyId)
    | SoundLoaded String (Result Evergreen.V7.Audio.LoadError Evergreen.V7.Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Effect.Time.Posix


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V7.Id.Id Evergreen.V7.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V7.Audio.LoadError Evergreen.V7.Audio.Source)
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { timelineCache : Evergreen.V7.Timeline.TimelineCache Evergreen.V7.MatchSetup.MatchState
    , userIds : AssocList.Dict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchData
    = MatchSetupData
        { matchName : String
        }
    | MatchData MatchPage_


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V7.Id.Id LobbyId
    , networkModel :
        Evergreen.V7.NetworkModel.NetworkModel
            { userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
            , msg : Evergreen.V7.MatchSetup.MatchSetupMsg
            }
            Evergreen.V7.MatchSetup.MatchSetup
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
    , currentKeys : List Evergreen.V7.Keyboard.Key
    , previousKeys : List Evergreen.V7.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V7.Sounds.Sounds
    , userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V7.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V7.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | MatchSetupRequest (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V7.Id.Id LobbyId)
    | CreateLobbyBroadcast (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V7.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V7.MatchSetup.MatchSetup)
    | PingResponse Effect.Time.Posix
    | MatchSetupBroadcast (Evergreen.V7.Id.Id LobbyId) (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) Evergreen.V7.MatchSetup.MatchSetupMsg (Maybe LobbyData)
