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
    , MatchState
    , Page(..)
    , TimelineEvent
    , ToBackend(..)
    , ToFrontend(..)
    , WindowSize
    , WorldPixel
    )

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Audio
import Browser
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Id exposing (Id)
import Keyboard
import Keyboard.Arrows
import Lobby exposing (Lobby, LobbyPreview)
import LocalModel exposing (LocalModel)
import Pixels exposing (Pixels)
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
    , initData : Maybe LobbyData
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
    }


type Page
    = LobbyPage LobbyData
    | MatchPage MatchState


type alias LobbyData =
    { userId : Id UserId
    , lobbies : Dict (Id LobbyId) LobbyPreview
    , currentLobby : Maybe { id : Id LobbyId, lobby : Lobby }
    }


type alias MatchState =
    { startTime : Time.Posix, timeline : Timeline TimelineEvent, otherUsers : List (Id UserId) }


type alias TimelineEvent =
    { userId : Id UserId, input : Keyboard.Arrows.Direction }


type alias BackendModel =
    { userSessions : Dict SessionId { clientIds : Dict ClientId (), userId : Id UserId }
    , users : Dict (Id UserId) BackendUserData
    , lobbies : Dict (Id LobbyId) Lobby
    , matches : Dict (Id MatchId) { users : Dict UserId () }
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
    | CreateLobbyPressed
    | JoinLobbyPressed (Id LobbyId)
    | StartMatchPressed
    | SoundLoaded String (Result Audio.LoadError Audio.Source)


type LobbyId
    = LobbyId Never


type MatchId
    = MatchId Never


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Id LobbyId)
    | StartMatchRequest Time.Posix


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = CreateLobbyResponse (Id LobbyId) Lobby
    | ClientInit LobbyData
    | JoinLobbyResponse (Result JoinLobbyError Lobby)
    | JoinLobbyBroadcast (Id UserId)


type JoinLobbyError
    = LobbyNotFound
