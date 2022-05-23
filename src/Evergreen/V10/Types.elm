module Evergreen.V10.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V10.Audio
import Evergreen.V10.Direction2d
import Evergreen.V10.Id
import Evergreen.V10.Keyboard
import Evergreen.V10.Lobby
import Evergreen.V10.Point2d
import Evergreen.V10.Sounds
import Evergreen.V10.Timeline
import Evergreen.V10.User
import Evergreen.V10.Vector2d
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
    | KeyMsg Evergreen.V10.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V10.Id.Id LobbyId)
    | PressedStartMatch
    | SoundLoaded String (Result Evergreen.V10.Audio.LoadError Evergreen.V10.Audio.Source)
    | PointerDown Html.Events.Extra.Touch.Event
    | PointerUp Html.Events.Extra.Touch.Event
    | PointerMoved Html.Events.Extra.Touch.Event


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V10.Id.Id LobbyId) Evergreen.V10.Lobby.LobbyPreview
    , currentLobby :
        Maybe
            { id : Evergreen.V10.Id.Id LobbyId
            , lobby : Evergreen.V10.Lobby.Lobby
            }
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , initData : Maybe ( Evergreen.V10.Id.Id Evergreen.V10.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V10.Audio.LoadError Evergreen.V10.Audio.Source)
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias TimelineEvent =
    { userId : Evergreen.V10.Id.Id Evergreen.V10.User.UserId
    , input : Maybe (Evergreen.V10.Direction2d.Direction2d WorldCoordinate)
    }


type alias MatchState =
    { players :
        AssocList.Dict
            (Evergreen.V10.Id.Id Evergreen.V10.User.UserId)
            { position : Evergreen.V10.Point2d.Point2d Length.Meters WorldCoordinate
            , velocity : Evergreen.V10.Vector2d.Vector2d Length.Meters WorldCoordinate
            , input : Maybe (Evergreen.V10.Direction2d.Direction2d WorldCoordinate)
            }
    }


type MatchId
    = MatchId Never


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchPage_ =
    { startTime : Effect.Time.Posix
    , localStartTime : Effect.Time.Posix
    , timeline : Evergreen.V10.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V10.Timeline.TimelineCache MatchState
    , userIds : List.Nonempty.Nonempty (Evergreen.V10.Id.Id Evergreen.V10.User.UserId)
    , matchId : Evergreen.V10.Id.Id MatchId
    , zoom : Float
    , touchPosition : Maybe (Evergreen.V10.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V10.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
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
    , currentKeys : List Evergreen.V10.Keyboard.Key
    , previousKeys : List Evergreen.V10.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , page : Page
    , sounds : Evergreen.V10.Sounds.Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
    , userId : Evergreen.V10.Id.Id Evergreen.V10.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V10.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V10.Id.Id Evergreen.V10.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V10.Id.Id Evergreen.V10.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V10.Id.Id LobbyId) Evergreen.V10.Lobby.Lobby
    , matches :
        AssocList.Dict
            (Evergreen.V10.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V10.Id.Id Evergreen.V10.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V10.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Evergreen.V10.Id.Id LobbyId)
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V10.Id.Id MatchId) Effect.Time.Posix (Maybe (Evergreen.V10.Direction2d.Direction2d WorldCoordinate))
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V10.Id.Id LobbyId) Evergreen.V10.Lobby.Lobby
    | CreateLobbyBroadcast (Evergreen.V10.Id.Id LobbyId) Evergreen.V10.Lobby.LobbyPreview
    | ClientInit (Evergreen.V10.Id.Id Evergreen.V10.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V10.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V10.Lobby.Lobby)
    | JoinLobbyBroadcast (Evergreen.V10.Id.Id LobbyId) (Evergreen.V10.Id.Id Evergreen.V10.User.UserId)
    | StartMatchBroadcast (Evergreen.V10.Id.Id MatchId) Effect.Time.Posix (List.Nonempty.Nonempty (Evergreen.V10.Id.Id Evergreen.V10.User.UserId))
    | MatchInputBroadcast (Evergreen.V10.Id.Id MatchId) Effect.Time.Posix TimelineEvent
    | PingResponse Effect.Time.Posix
