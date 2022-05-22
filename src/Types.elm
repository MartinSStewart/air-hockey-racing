module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel
    , FrontendModel_(..)
    , FrontendMsg
    , FrontendMsg_(..)
    , JoinLobbyError(..)
    , LobbyData
    , LobbyId
    , MatchId
    , MatchPage_
    , MatchState
    , Page(..)
    , PingData
    , TimelineEvent
    , ToBackend(..)
    , ToFrontend(..)
    , WindowSize
    , WorldCoordinate
    , WorldPixel
    )

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Audio
import Browser
import Duration exposing (Duration)
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Id exposing (Id)
import Keyboard
import Keyboard.Arrows
import Length exposing (Meters)
import List.Nonempty exposing (Nonempty)
import Lobby exposing (Lobby, LobbyPreview)
import LocalModel exposing (LocalModel)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Sounds exposing (Sounds)
import Timeline exposing (FrameId, Timeline, TimelineCache)
import Url exposing (Url)
import User exposing (UserId)


type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type WorldPixel
    = WorldPixel Never


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , initData : Maybe ( Id UserId, LobbyData )
    , sounds : Dict String (Result Audio.LoadError Audio.Source)
    }


type alias WindowSize =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , pressedKeys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , page : Page
    , sounds : Sounds
    , lastButtonPress : Maybe Time.Posix
    , userId : Id UserId
    , pingStartTime : Maybe Time.Posix
    , pingData : Maybe PingData
    }


type alias PingData =
    { roundTripTime : Duration
    , serverTime : Time.Posix
    , sendTime : Time.Posix
    , receiveTime : Time.Posix
    , lowEstimate : Duration
    , highEstimate : Duration
    }


type Page
    = LobbyPage LobbyData
    | MatchPage MatchPage_


type alias LobbyData =
    { lobbies : Dict (Id LobbyId) LobbyPreview
    , currentLobby : Maybe { id : Id LobbyId, lobby : Lobby }
    }


type alias MatchPage_ =
    { startTime : Time.Posix
    , localStartTime : Time.Posix
    , timeline : Timeline TimelineEvent
    , timelineCache : TimelineCache MatchState
    , userIds : Nonempty (Id UserId)
    , matchId : Id MatchId
    }


type alias MatchState =
    { players :
        Dict
            (Id UserId)
            { position : Point2d Meters WorldCoordinate
            , input : Keyboard.Arrows.Direction
            }
    }


type WorldCoordinate
    = WorldCoordinate Never


type alias TimelineEvent =
    { userId : Id UserId, input : Keyboard.Arrows.Direction }


type alias BackendModel =
    { userSessions : Dict SessionId { clientIds : Dict ClientId (), userId : Id UserId }
    , users : Dict (Id UserId) BackendUserData
    , lobbies : Dict (Id LobbyId) Lobby
    , matches : Dict (Id MatchId) { users : Nonempty (Id UserId) }
    , counter : Int
    }


type alias BackendUserData =
    { name : String }


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | KeyMsg Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Id LobbyId)
    | PressedStartMatch
    | SoundLoaded String (Result Audio.LoadError Audio.Source)


type LobbyId
    = LobbyId Never


type MatchId
    = MatchId Never


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Id LobbyId)
    | StartMatchRequest
    | MatchInputRequest (Id MatchId) Time.Posix Keyboard.Arrows.Direction
    | PingRequest


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | GotTimeForUpdateFromFrontend SessionId ClientId ToBackend Time.Posix


type ToFrontend
    = CreateLobbyResponse (Id LobbyId) Lobby
    | CreateLobbyBroadcast (Id LobbyId) LobbyPreview
    | ClientInit (Id UserId) LobbyData
    | JoinLobbyResponse (Id LobbyId) (Result JoinLobbyError Lobby)
    | JoinLobbyBroadcast (Id LobbyId) (Id UserId)
    | StartMatchBroadcast (Id MatchId) Time.Posix (Nonempty (Id UserId))
    | MatchInputBroadcast (Id MatchId) (Id UserId) Time.Posix Keyboard.Arrows.Direction
    | PingResponse Time.Posix


type JoinLobbyError
    = LobbyNotFound
