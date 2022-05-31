module Evergreen.V12.Types exposing (..)

import Angle
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
import Evergreen.V12.Direction2d
import Evergreen.V12.Id
import Evergreen.V12.Keyboard
import Evergreen.V12.MatchSetup
import Evergreen.V12.NetworkModel
import Evergreen.V12.Point2d
import Evergreen.V12.Sounds
import Evergreen.V12.Timeline
import Evergreen.V12.User
import Evergreen.V12.Vector2d
import Html.Events.Extra.Touch
import Length
import List.Nonempty
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
    | KeyMsg Evergreen.V12.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V12.Id.Id LobbyId)
    | PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V12.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V12.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V12.Decal.Decal
    | SoundLoaded String (Result Evergreen.V12.Audio.LoadError Evergreen.V12.Audio.Source)
    | MatchMsg MatchMsg
    | GotTime Effect.Time.Posix


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


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V12.Id.Id LobbyId
    , networkModel :
        Evergreen.V12.NetworkModel.NetworkModel
            { userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
            , msg : Evergreen.V12.MatchSetup.MatchSetupMsg
            }
            Evergreen.V12.MatchSetup.MatchSetup
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias TimelineEvent =
    { userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
    , input : Maybe (Evergreen.V12.Direction2d.Direction2d WorldCoordinate)
    }


type alias Player =
    { position : Evergreen.V12.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V12.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V12.Direction2d.Direction2d WorldCoordinate)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) Player
    }


type alias Vertex =
    { position : Math.Vector2.Vec2
    , color : Math.Vector3.Vec3
    }


type MatchId
    = MatchId Never


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { startTime : Effect.Time.Posix
    , localStartTime : Effect.Time.Posix
    , timeline : Evergreen.V12.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V12.Timeline.TimelineCache MatchState
    , userIds :
        List.Nonempty.Nonempty
            { userId : Evergreen.V12.Id.Id Evergreen.V12.User.UserId
            , playerData : Evergreen.V12.MatchSetup.PlayerData
            , mesh : Effect.WebGL.Mesh Vertex
            }
    , wallMesh : Effect.WebGL.Mesh Vertex
    , matchId : Evergreen.V12.Id.Id MatchId
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V12.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V12.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type Page
    = LobbyPage LobbyData
    | MatchSetupPage MatchSetupPage_
    | MatchPage MatchPage_


type alias PingData =
    { roundTripTime : Duration.Duration
    , serverTime : Effect.Time.Posix
    , sendTime : Effect.Time.Posix
    , receiveTime : Effect.Time.Posix
    , lowEstimate : Duration.Duration
    , highEstimate : Duration.Duration
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
    , lastButtonPress : Maybe Effect.Time.Posix
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
    , matches :
        AssocList.Dict
            (Evergreen.V12.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V12.Id.Id Evergreen.V12.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V12.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | MatchSetupRequest (Evergreen.V12.Id.Id LobbyId) Evergreen.V12.MatchSetup.MatchSetupMsg
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V12.Id.Id MatchId) (Evergreen.V12.Id.Id Evergreen.V12.Timeline.FrameId) (Maybe (Evergreen.V12.Direction2d.Direction2d WorldCoordinate))
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V12.Id.Id LobbyId) Evergreen.V12.MatchSetup.MatchSetup
    | CreateLobbyBroadcast (Evergreen.V12.Id.Id LobbyId) Evergreen.V12.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V12.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V12.MatchSetup.MatchSetup)
    | StartMatchBroadcast (Evergreen.V12.Id.Id MatchId) Effect.Time.Posix (List.Nonempty.Nonempty ( Evergreen.V12.Id.Id Evergreen.V12.User.UserId, Evergreen.V12.MatchSetup.PlayerData ))
    | MatchInputBroadcast (Evergreen.V12.Id.Id MatchId) (Evergreen.V12.Id.Id Evergreen.V12.Timeline.FrameId) TimelineEvent
    | PingResponse Effect.Time.Posix
    | MatchSetupBroadcast (Evergreen.V12.Id.Id LobbyId) (Evergreen.V12.Id.Id Evergreen.V12.User.UserId) Evergreen.V12.MatchSetup.MatchSetupMsg (Maybe LobbyData)
