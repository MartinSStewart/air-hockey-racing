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
    , MainLobbyInitData
    , Page(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import AssocList exposing (Dict)
import Audio
import Browser
import Duration exposing (Duration)
import EditorPage
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Id exposing (Id)
import Keyboard
import Match exposing (LobbyPreview, Match, MatchState, PlayerMode, ServerTime)
import MatchPage exposing (MatchId, WorldPixel)
import PingData exposing (PingData)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Sounds exposing (Sounds)
import Ui exposing (Size)
import User exposing (UserId)


type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : Size
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Maybe Time.Posix
    , debugTimeOffset : Duration
    , initData : Maybe ( Id UserId, MainLobbyInitData )
    , sounds : Dict String (Result Audio.LoadError Audio.Source)
    }


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : Size
    , currentKeys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , debugTimeOffset : Duration
    , page : Page
    , sounds : Sounds
    , userId : Id UserId
    , pingStartTime : Maybe Time.Posix
    , pingData : Maybe PingData
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage MatchPage.Model
    | EditorPage EditorPage.Model


type alias MainLobbyPage_ =
    { lobbies : Dict (Id MatchId) LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type alias MainLobbyInitData =
    { lobbies : Dict (Id MatchId) LobbyPreview }


type alias BackendModel =
    { userSessions : Dict SessionId { clientIds : Dict ClientId (), userId : Id UserId }
    , users : Dict (Id UserId) BackendUserData
    , lobbies : Dict (Id MatchId) Match
    , dummyChange : Float
    , counter : Int
    }


type alias BackendUserData =
    { name : String }


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged
    | KeyMsg Keyboard.Msg
    | WindowResized Size
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Id MatchId)
    | SoundLoaded String (Result Audio.LoadError Audio.Source)
    | MatchPageMsg MatchPage.Msg
    | GotTime Time.Posix
    | RandomInput Time.Posix
    | EditorPageMsg EditorPage.Msg


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend MatchPage.ToBackend
    | EditorPageToBackend EditorPage.ToBackend


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | ClientDisconnectedWithTime SessionId ClientId ServerTime
    | UpdateFromFrontendWithTime SessionId ClientId ToBackend ServerTime


type ToFrontend
    = CreateLobbyResponse (Id MatchId) Match
    | RemoveLobbyBroadcast (Id MatchId)
    | UpdateLobbyBroadcast (Id MatchId) LobbyPreview
    | CreateLobbyBroadcast (Id MatchId) LobbyPreview
    | ClientInit (Id UserId) MainLobbyInitData
    | JoinLobbyResponse (Id MatchId) (Result JoinLobbyError Match)
    | PingResponse ServerTime
    | MatchPageToFrontend MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend EditorPage.ToFrontend


type JoinLobbyError
    = LobbyNotFound
    | LobbyFull
