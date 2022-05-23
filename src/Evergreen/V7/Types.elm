module Evergreen.V7.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V7.Audio
import Evergreen.V7.Direction2d
import Evergreen.V7.Id
import Evergreen.V7.Keyboard
import Evergreen.V7.Lobby
import Evergreen.V7.Point2d
import Evergreen.V7.Sounds
import Evergreen.V7.Timeline
import Evergreen.V7.User
import Evergreen.V7.Vector2d
import Html.Events.Extra.Pointer
import Html.Events.Extra.Touch
import Length
import List.Nonempty
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
    | PressedStartMatch
    | SoundLoaded String (Result Evergreen.V7.Audio.LoadError Evergreen.V7.Audio.Source)
    | PointerDown Html.Events.Extra.Touch.Event
    | PointerUp Html.Events.Extra.Pointer.Event
    | PointerMoved Html.Events.Extra.Pointer.Event


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.Lobby.LobbyPreview
    , currentLobby :
        Maybe
            { id : Evergreen.V7.Id.Id LobbyId
            , lobby : Evergreen.V7.Lobby.Lobby
            }
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , initData : Maybe ( Evergreen.V7.Id.Id Evergreen.V7.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V7.Audio.LoadError Evergreen.V7.Audio.Source)
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias TimelineEvent =
    { userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , input : Maybe (Evergreen.V7.Direction2d.Direction2d WorldCoordinate)
    }


type alias MatchState =
    { players :
        AssocList.Dict
            (Evergreen.V7.Id.Id Evergreen.V7.User.UserId)
            { position : Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate
            , velocity : Evergreen.V7.Vector2d.Vector2d Length.Meters WorldCoordinate
            , input : Maybe (Evergreen.V7.Direction2d.Direction2d WorldCoordinate)
            }
    }


type MatchId
    = MatchId Never


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { startTime : Effect.Time.Posix
    , localStartTime : Effect.Time.Posix
    , timeline : Evergreen.V7.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V7.Timeline.TimelineCache MatchState
    , userIds : List.Nonempty.Nonempty (Evergreen.V7.Id.Id Evergreen.V7.User.UserId)
    , matchId : Evergreen.V7.Id.Id MatchId
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    }


type Page
    = LobbyPage LobbyData
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
    , currentKeys : List Evergreen.V7.Keyboard.Key
    , previousKeys : List Evergreen.V7.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , page : Page
    , sounds : Evergreen.V7.Sounds.Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
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
    , lobbies : AssocList.Dict (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.Lobby.Lobby
    , matches :
        AssocList.Dict
            (Evergreen.V7.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V7.Id.Id Evergreen.V7.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V7.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Evergreen.V7.Id.Id LobbyId)
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V7.Id.Id MatchId) Effect.Time.Posix (Maybe (Evergreen.V7.Direction2d.Direction2d WorldCoordinate))
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.Lobby.Lobby
    | CreateLobbyBroadcast (Evergreen.V7.Id.Id LobbyId) Evergreen.V7.Lobby.LobbyPreview
    | ClientInit (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V7.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V7.Lobby.Lobby)
    | JoinLobbyBroadcast (Evergreen.V7.Id.Id LobbyId) (Evergreen.V7.Id.Id Evergreen.V7.User.UserId)
    | StartMatchBroadcast (Evergreen.V7.Id.Id MatchId) Effect.Time.Posix (List.Nonempty.Nonempty (Evergreen.V7.Id.Id Evergreen.V7.User.UserId))
    | MatchInputBroadcast (Evergreen.V7.Id.Id MatchId) Effect.Time.Posix TimelineEvent
    | PingResponse Effect.Time.Posix
