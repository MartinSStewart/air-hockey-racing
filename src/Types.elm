module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , BroadcastChange(..)
    , ClientInitData
    , FrameId
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

import Audio
import Browser exposing (UrlRequest)
import Browser.Navigation
import Dict exposing (Dict)
import Id exposing (Id)
import IdDict exposing (IdDict)
import Keyboard
import Keyboard.Arrows
import Lamdera exposing (ClientId, SessionId)
import LocalModel exposing (LocalModel)
import Match exposing (Match)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Sounds exposing (Sounds)
import Time
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


type FrameId
    = FrameId Never


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , initData : Maybe ClientInitData
    , sounds : Dict String (Result Audio.LoadError Audio.Source)
    }


type alias WindowSize =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , windowSize : WindowSize
    , pressedKeys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , localModel : LocalModel ToFrontendChange Local
    , matchCache : IdDict FrameId Match
    , visibleMatch : Maybe Match
    , sounds : Sounds
    , lastButtonPress : Maybe Time.Posix
    }


type alias Local =
    { lobbies : IdDict LobbyId Lobby
    , userId : Id UserId
    , match : Maybe MatchState
    }


type alias MatchState =
    { startTime : Time.Posix, events : List TimelineEvent, otherUsers : List (Id UserId) }


type alias TimelineEvent =
    { userId : Id UserId, frameId : Id FrameId, input : Keyboard.Arrows.Direction }


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
    = UrlClicked UrlRequest
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
    = SessionChange_ SessionChange


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = Change ToFrontendChange
    | ClientInit ClientInitData



--| BroadcastMove (Id UserId) Time.Posix Keyboard.Arrows.Direction


type ToFrontendChange
    = BroadcastChange BroadcastChange
    | SessionChange SessionChange


type BroadcastChange
    = BroadcastCreateLobby (Id UserId)
    | BroadcastJoinLobby (Id UserId) (Id LobbyId)
    | BroadcastStartMatch Time.Posix (Id LobbyId)
    | BroadcastMatchInput TimelineEvent


type SessionChange
    = CreateLobby
    | JoinLobby (Id LobbyId)
    | StartMatch Time.Posix
    | MatchInput (Id FrameId) Keyboard.Arrows.Direction
