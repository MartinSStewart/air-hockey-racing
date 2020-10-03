module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , BroadcastChange(..)
    , ClientId
    , ClientInitData
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel(..)
    , FrontendMsg(..)
    , Lobby
    , LobbyId
    , Local
    , MatchId
    , SessionChange(..)
    , SessionId
    , ToBackend(..)
    , ToFrontend(..)
    , ToFrontendChange(..)
    , WindowSize
    , WorldPixel
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Id exposing (Id)
import IdDict exposing (IdDict)
import Keyboard
import Keyboard.Arrows
import LocalModel exposing (LocalModel)
import Match exposing (Match)
import Physics.World
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Time
import Url exposing (Url)
import User exposing (UserId)
import WebGL.Texture exposing (Texture)


type alias SessionId =
    String


type alias ClientId =
    String


type FrontendModel
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type WorldPixel
    = WorldPixel Never


type alias FrontendLoading =
    { key : Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , initData : Maybe ClientInitData
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
    }


type alias Local =
    { lobbies : IdDict LobbyId Lobby
    , match : Maybe Match
    , userId : Id UserId
    }


type alias BackendModel =
    { userSessions : Dict SessionId { clientIds : Dict ClientId (), userId : Id UserId }
    , users : IdDict UserId BackendUserData
    , lobbies : IdDict LobbyId Lobby
    , matches : IdDict MatchId Match
    }


type alias Lobby =
    { users : IdDict UserId () }


type alias BackendUserData =
    { name : String }


type alias ClientInitData =
    { lobbies : IdDict LobbyId Lobby, userId : Id UserId }


type FrontendMsg
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
    | BroadcastStartMatch (Id LobbyId)
    | BroadcastMove (Id UserId) Time.Posix Keyboard.Arrows.Direction


type SessionChange
    = CreateLobby
    | JoinLobby (Id LobbyId)
    | StartMatch
    | Move Time.Posix Keyboard.Arrows.Direction
