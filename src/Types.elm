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
    , JoinMatch(..)
    , MainLobbyInitData
    , Page(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Audio
import Browser
import Duration exposing (Duration)
import EditorPage
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Id exposing (Id)
import Keyboard
import Length exposing (Meters)
import List.Nonempty exposing (Nonempty)
import Match exposing (LobbyPreview, Match, MatchState, ServerTime, WorldCoordinate)
import MatchPage exposing (MatchId, Mouse, ScreenCoordinate, WorldPixel)
import NonemptySet exposing (NonemptySet)
import PingData exposing (PingData)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Route exposing (Route)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Size exposing (Size)
import Sounds exposing (Sounds)
import Timeline exposing (FrameId)
import Url exposing (Url)
import User exposing (UserId)


type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Size
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Maybe Time.Posix
    , debugTimeOffset : Duration
    , initData : Maybe ( Id UserId, MainLobbyInitData )
    , sounds : SeqDict String (Result Audio.LoadError Audio.Source)
    , route : Route
    }


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Size
    , currentKeys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , currentMouse : Mouse
    , previousMouse : Mouse
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , debugTimeOffset : Duration
    , page : Page
    , sounds : Sounds
    , userId : Id UserId
    , pingStartTime : Maybe Time.Posix
    , pingData : Maybe PingData
    , route : Route
    , loadMatchError : Maybe Time.Posix
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage MatchPage.Model
    | EditorPage EditorPage.Model


type alias MainLobbyPage_ =
    { lobbies : SeqDict (Id MatchId) LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type alias MainLobbyInitData =
    { lobbies : SeqDict (Id MatchId) LobbyPreview }


type alias BackendModel =
    { userSessions : SeqDict SessionId { clientIds : SeqDict ClientId (), userId : Id UserId }
    , users : SeqDict (Id UserId) BackendUserData
    , lobbies : SeqDict (Id MatchId) Match
    , joiningActiveMatch : SeqDict ( Id MatchId, Id FrameId ) (NonemptySet ClientId)
    , dummyChange : Float
    , counter : Int
    , playerPositions : SeqDict (Id MatchId) (SeqDict (Id FrameId) (SeqDict (Id UserId) (Point2d Meters WorldCoordinate)))
    }


type alias BackendUserData =
    { name : String }


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | KeyMsg Keyboard.Msg
    | WindowResized Size
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Audio.LoadError Audio.Source)
    | MatchPageMsg MatchPage.Msg
    | GotTime Time.Posix
    | RandomInput Time.Posix
    | EditorPageMsg EditorPage.Msg
    | RejoinMatchTimedOut (Id MatchId)


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
    | JoinLobbyResponse (Id MatchId) JoinMatch
    | PingResponse ServerTime
    | MatchPageToFrontend MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend EditorPage.ToFrontend


type JoinMatch
    = JoinedLobby Match
    | JoinedActiveMatch Match (Id FrameId) MatchState
    | JoinLobbyError JoinLobbyError


type JoinLobbyError
    = MatchNotFound
    | MatchFull
