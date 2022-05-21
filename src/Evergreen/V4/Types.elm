module Evergreen.V4.Types exposing (..)

import AssocList
import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V4.Audio
import Evergreen.V4.Id
import Evergreen.V4.Keyboard
import Evergreen.V4.Keyboard.Arrows
import Evergreen.V4.Lobby
import Evergreen.V4.Sounds
import Evergreen.V4.Timeline
import Evergreen.V4.User
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
    | KeyMsg Evergreen.V4.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V4.Id.Id LobbyId)
    | PressedStartMatch
    | SoundLoaded String (Result Evergreen.V4.Audio.LoadError Evergreen.V4.Audio.Source)


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V4.Id.Id LobbyId) Evergreen.V4.Lobby.LobbyPreview
    , currentLobby :
        Maybe
            { id : Evergreen.V4.Id.Id LobbyId
            , lobby : Evergreen.V4.Lobby.Lobby
            }
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , initData : Maybe ( Evergreen.V4.Id.Id Evergreen.V4.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V4.Audio.LoadError Evergreen.V4.Audio.Source)
    }


type alias TimelineEvent =
    { userId : Evergreen.V4.Id.Id Evergreen.V4.User.UserId
    , input : Evergreen.V4.Keyboard.Arrows.Direction
    }


type alias MatchState =
    { players :
        AssocList.Dict
            (Evergreen.V4.Id.Id Evergreen.V4.User.UserId)
            { position : ( Float, Float )
            , input : Evergreen.V4.Keyboard.Arrows.Direction
            }
    }


type MatchId
    = MatchId Never


type alias MatchPage_ =
    { startTime : Effect.Time.Posix
    , localStartTime : Effect.Time.Posix
    , timeline : Evergreen.V4.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V4.Timeline.TimelineCache MatchState
    , userIds : List.Nonempty.Nonempty (Evergreen.V4.Id.Id Evergreen.V4.User.UserId)
    , matchId : Evergreen.V4.Id.Id MatchId
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
    , pressedKeys : List Evergreen.V4.Keyboard.Key
    , previousKeys : List Evergreen.V4.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , page : Page
    , sounds : Evergreen.V4.Sounds.Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
    , userId : Evergreen.V4.Id.Id Evergreen.V4.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe PingData
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V4.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V4.Id.Id Evergreen.V4.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V4.Id.Id Evergreen.V4.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V4.Id.Id LobbyId) Evergreen.V4.Lobby.Lobby
    , matches :
        AssocList.Dict
            (Evergreen.V4.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V4.Id.Id Evergreen.V4.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V4.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Evergreen.V4.Id.Id LobbyId)
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V4.Id.Id MatchId) Effect.Time.Posix Evergreen.V4.Keyboard.Arrows.Direction
    | PingRequest


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V4.Id.Id LobbyId) Evergreen.V4.Lobby.Lobby
    | CreateLobbyBroadcast (Evergreen.V4.Id.Id LobbyId) Evergreen.V4.Lobby.LobbyPreview
    | ClientInit (Evergreen.V4.Id.Id Evergreen.V4.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V4.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V4.Lobby.Lobby)
    | JoinLobbyBroadcast (Evergreen.V4.Id.Id LobbyId) (Evergreen.V4.Id.Id Evergreen.V4.User.UserId)
    | StartMatchBroadcast (Evergreen.V4.Id.Id MatchId) Effect.Time.Posix (List.Nonempty.Nonempty (Evergreen.V4.Id.Id Evergreen.V4.User.UserId))
    | MatchInputBroadcast (Evergreen.V4.Id.Id MatchId) (Evergreen.V4.Id.Id Evergreen.V4.User.UserId) Effect.Time.Posix Evergreen.V4.Keyboard.Arrows.Direction
    | PingResponse Effect.Time.Posix
