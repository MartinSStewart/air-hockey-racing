module Evergreen.V1.Types exposing (..)

import Angle
import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Evergreen.V1.Audio
import Evergreen.V1.ColorIndex
import Evergreen.V1.Decal
import Evergreen.V1.Direction2d
import Evergreen.V1.Id
import Evergreen.V1.Keyboard
import Evergreen.V1.MatchSetup
import Evergreen.V1.NetworkModel
import Evergreen.V1.Point2d
import Evergreen.V1.Sounds
import Evergreen.V1.Timeline
import Evergreen.V1.User
import Evergreen.V1.Vector2d
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
    | KeyMsg Evergreen.V1.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V1.Id.Id LobbyId)
    | PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V1.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V1.ColorIndex.ColorIndex
    | PressedDecal Evergreen.V1.Decal.Decal
    | SoundLoaded String (Result Evergreen.V1.Audio.LoadError Evergreen.V1.Audio.Source)
    | MatchMsg MatchMsg
    | GotTime Effect.Time.Posix


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.MatchSetup.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V1.Id.Id Evergreen.V1.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V1.Audio.LoadError Evergreen.V1.Audio.Source)
    }


type alias MatchSetupPage_ =
    { lobbyId : Evergreen.V1.Id.Id LobbyId
    , networkModel :
        Evergreen.V1.NetworkModel.NetworkModel
            { userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
            , msg : Evergreen.V1.MatchSetup.MatchSetupMsg
            }
            Evergreen.V1.MatchSetup.MatchSetup
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias TimelineEvent =
    { userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , input : Maybe (Evergreen.V1.Direction2d.Direction2d WorldCoordinate)
    }


type alias Player =
    { position : Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V1.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotationalVelocity : Angle.Angle
    , rotation : Angle.Angle
    , input : Maybe (Evergreen.V1.Direction2d.Direction2d WorldCoordinate)
    }


type alias MatchState =
    { players : AssocList.Dict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) Player
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
    , timeline : Evergreen.V1.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V1.Timeline.TimelineCache MatchState
    , userIds :
        List.Nonempty.Nonempty
            { userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
            , playerData : Evergreen.V1.MatchSetup.PlayerData
            , mesh : Effect.WebGL.Mesh Vertex
            }
    , wallMesh : Effect.WebGL.Mesh Vertex
    , matchId : Evergreen.V1.Id.Id MatchId
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V1.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V1.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
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
    , pingCount : Int
    }


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , currentKeys : List Evergreen.V1.Keyboard.Key
    , previousKeys : List Evergreen.V1.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V1.Sounds.Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
    , userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V1.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.MatchSetup.MatchSetup
    , matches :
        AssocList.Dict
            (Evergreen.V1.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V1.Id.Id Evergreen.V1.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V1.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | MatchSetupRequest (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.MatchSetup.MatchSetupMsg
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId) (Maybe (Evergreen.V1.Direction2d.Direction2d WorldCoordinate))
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.MatchSetup.MatchSetup
    | CreateLobbyBroadcast (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.MatchSetup.LobbyPreview
    | ClientInit (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V1.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V1.MatchSetup.MatchSetup)
    | StartMatchBroadcast (Evergreen.V1.Id.Id MatchId) Effect.Time.Posix (List.Nonempty.Nonempty ( Evergreen.V1.Id.Id Evergreen.V1.User.UserId, Evergreen.V1.MatchSetup.PlayerData ))
    | MatchInputBroadcast (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId) TimelineEvent
    | PingResponse Effect.Time.Posix
    | MatchSetupBroadcast (Evergreen.V1.Id.Id LobbyId) (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) Evergreen.V1.MatchSetup.MatchSetupMsg (Maybe LobbyData)
