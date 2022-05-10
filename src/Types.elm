module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , BroadcastChange(..)
    , ClientInitData
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel
    , FrontendModel_(..)
    , FrontendMsg
    , FrontendMsg_(..)
    , Lobby
    , LobbyId
    , Local
    , MatchId
    , MatchState
    , SessionChange(..)
    , TimelineEvent
    , ToBackend(..)
    , ToFrontend(..)
    , ToFrontendChange(..)
    , WindowSize
    , WorldPixel
    )

import AssocList as Dict exposing (Dict)
import Audio
import Browser
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time
import Id exposing (Id)
import IdDict exposing (IdDict)
import Keyboard
import Keyboard.Arrows
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
    , time : Effect.Time.Posix
    , initData : Maybe ClientInitData
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
    , time : Effect.Time.Posix
    , localModel : LocalModel ToFrontendChange Local
    , sounds : Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
    }


type alias Local =
    { lobbies : IdDict LobbyId Lobby
    , userId : Id UserId
    , match : Maybe MatchState
    }


type alias MatchState =
    { startTime : Effect.Time.Posix, timeline : Timeline TimelineEvent, otherUsers : List (Id UserId) }


type alias TimelineEvent =
    { userId : Id UserId, input : Keyboard.Arrows.Direction }


type alias BackendModel =
    { userSessions : Dict SessionId { clientIds : Dict ClientId (), userId : Id UserId }
    , users : IdDict UserId BackendUserData
    , lobbies : IdDict LobbyId Lobby
    , matches : IdDict MatchId { users : IdDict UserId () }
    }


type alias Lobby =
    { users : IdDict UserId () }


type alias BackendUserData =
    { name : String }


type alias ClientInitData =
    { lobbies : IdDict LobbyId Lobby, userId : Id UserId }


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | KeyMsg Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Effect.Time.Posix
    | CreateLobbyPressed
    | JoinLobbyPressed (Id LobbyId)
    | StartMatchPressed
    | SoundLoaded String (Result Audio.LoadError Audio.Source)


type LobbyId
    = LobbyId Never


type MatchId
    = MatchId Never


type ToBackend
    = SessionChange_ SessionChange


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = Change ToFrontendChange
    | ClientInit ClientInitData


type ToFrontendChange
    = BroadcastChange BroadcastChange
    | SessionChange SessionChange


type BroadcastChange
    = BroadcastCreateLobby (Id UserId)
    | BroadcastJoinLobby (Id UserId) (Id LobbyId)
    | BroadcastStartMatch Effect.Time.Posix (Id LobbyId)
    | BroadcastMatchInput (Id FrameId) TimelineEvent


type SessionChange
    = CreateLobby
    | JoinLobby (Id LobbyId)
    | StartMatch Effect.Time.Posix
    | MatchInput (Id FrameId) Keyboard.Arrows.Direction
