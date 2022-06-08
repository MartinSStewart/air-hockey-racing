module Evergreen.V11.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V11.Audio
import Evergreen.V11.ColorIndex
import Evergreen.V11.Decal
import Evergreen.V11.Id
import Evergreen.V11.Keyboard
import Evergreen.V11.MatchName
import Evergreen.V11.MatchSetup
import Evergreen.V11.NetworkModel
import Evergreen.V11.Point2d
import Evergreen.V11.Sounds
import Evergreen.V11.TextMessage
import Evergreen.V11.Timeline
import Evergreen.V11.User
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
    | PressedPrimaryColor Evergreen.V11.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V11.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V11.Decal.Decal
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V11.MatchSetup.PlayerMode
    | PressedSaveMatchName Evergreen.V11.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V11.TextMessage.TextMessage


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | KeyMsg Evergreen.V11.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V11.Id.Id LobbyId)
    | SoundLoaded String (Result Evergreen.V11.Audio.LoadError Evergreen.V11.Audio.Source)
    | MatchMsg MatchMsg
    | MatchSetupMsg MatchSetupMsg_
    | GotTime Effect.Time.Posix
    | ScrolledToBottom


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V11.Id.Id LobbyId) Evergreen.V11.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V11.Id.Id Evergreen.V11.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V11.Audio.LoadError Evergreen.V11.Audio.Source)
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { timelineCache : Evergreen.V11.Timeline.TimelineCache Evergreen.V11.MatchSetup.MatchState
    , userIds : AssocList.Dict (Evergreen.V11.Id.Id Evergreen.V11.User.UserId) (Effect.WebGL.Mesh Vertex)
    , wallMesh : Effect.WebGL.Mesh Vertex
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V11.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V11.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type MatchData
    = MatchSetupData
        { matchName : String
        , message : String
        }
    | MatchData MatchPage_


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V11.Id.Id LobbyId
    , networkModel :
        Evergreen.V11.NetworkModel.NetworkModel
            { userId : Evergreen.V11.Id.Id Evergreen.V11.User.UserId
            , msg : Evergreen.V11.MatchSetup.MatchSetupMsg
            }
            Evergreen.V11.MatchSetup.MatchSetup
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
    , currentKeys : List Evergreen.V11.Keyboard.Key
    , previousKeys : List Evergreen.V11.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V11.Sounds.Sounds
    , userId : Evergreen.V11.Id.Id Evergreen.V11.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V11.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V11.Id.Id Evergreen.V11.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V11.Id.Id Evergreen.V11.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V11.Id.Id LobbyId) Evergreen.V11.MatchSetup.MatchSetup
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V11.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | MatchSetupRequest (Evergreen.V11.Id.Id LobbyId) Evergreen.V11.MatchSetup.MatchSetupMsg
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V11.Id.Id LobbyId) Evergreen.V11.MatchSetup.MatchSetup
    | RemoveLobbyBroadcast (Evergreen.V11.Id.Id LobbyId)
    | CreateLobbyBroadcast (Evergreen.V11.Id.Id LobbyId) Evergreen.V11.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V11.Id.Id Evergreen.V11.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V11.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V11.MatchSetup.MatchSetup)
    | PingResponse Effect.Time.Posix
    | MatchSetupBroadcast (Evergreen.V11.Id.Id LobbyId) (Evergreen.V11.Id.Id Evergreen.V11.User.UserId) Evergreen.V11.MatchSetup.MatchSetupMsg (Maybe LobbyData)
