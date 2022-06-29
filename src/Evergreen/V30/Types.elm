module Evergreen.V30.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V30.Audio
import Evergreen.V30.EditorPage
import Evergreen.V30.Id
import Evergreen.V30.Keyboard
import Evergreen.V30.Match
import Evergreen.V30.MatchPage
import Evergreen.V30.PingData
import Evergreen.V30.Size
import Evergreen.V30.Sounds
import Evergreen.V30.User
import Pixels
import Quantity


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged
    | KeyMsg Evergreen.V30.Keyboard.Msg
    | WindowResized Evergreen.V30.Size.Size
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V30.MatchPage.WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | PressedJoinLobby (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId)
    | SoundLoaded String (Result Evergreen.V30.Audio.LoadError Evergreen.V30.Audio.Source)
    | MatchPageMsg Evergreen.V30.MatchPage.Msg
    | GotTime Effect.Time.Posix
    | RandomInput Effect.Time.Posix
    | EditorPageMsg Evergreen.V30.EditorPage.Msg


type alias MainLobbyInitData =
    { lobbies : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId) Evergreen.V30.Match.LobbyPreview
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V30.Size.Size
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V30.MatchPage.WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V30.Id.Id Evergreen.V30.User.UserId, MainLobbyInitData )
    , sounds : AssocList.Dict String (Result Evergreen.V30.Audio.LoadError Evergreen.V30.Audio.Source)
    }


type JoinLobbyError
    = LobbyNotFound
    | LobbyFull


type alias MainLobbyPage_ =
    { lobbies : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId) Evergreen.V30.Match.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage Evergreen.V30.MatchPage.Model
    | EditorPage Evergreen.V30.EditorPage.Model


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V30.Size.Size
    , currentKeys : List Evergreen.V30.Keyboard.Key
    , previousKeys : List Evergreen.V30.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V30.MatchPage.WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V30.Sounds.Sounds
    , userId : Evergreen.V30.Id.Id Evergreen.V30.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe Evergreen.V30.PingData.PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V30.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V30.Id.Id Evergreen.V30.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId) Evergreen.V30.Match.Match
    , dummyChange : Float
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V30.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend Evergreen.V30.MatchPage.ToBackend
    | EditorPageToBackend Evergreen.V30.EditorPage.ToBackend


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V30.Match.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V30.Match.ServerTime


type ToFrontend
    = CreateLobbyResponse (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId) Evergreen.V30.Match.Match
    | RemoveLobbyBroadcast (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId)
    | UpdateLobbyBroadcast (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId) Evergreen.V30.Match.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId) Evergreen.V30.Match.LobbyPreview
    | ClientInit (Evergreen.V30.Id.Id Evergreen.V30.User.UserId) MainLobbyInitData
    | JoinLobbyResponse (Evergreen.V30.Id.Id Evergreen.V30.MatchPage.MatchId) (Result JoinLobbyError Evergreen.V30.Match.Match)
    | PingResponse Evergreen.V30.Match.ServerTime
    | MatchPageToFrontend Evergreen.V30.MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend Evergreen.V30.EditorPage.ToFrontend
