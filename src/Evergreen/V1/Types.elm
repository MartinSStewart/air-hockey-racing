module Evergreen.V1.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V1.Audio
import Evergreen.V1.Id
import Evergreen.V1.Keyboard
import Evergreen.V1.Keyboard.Arrows
import Evergreen.V1.Lobby
import Evergreen.V1.Sounds
import Evergreen.V1.Timeline
import Evergreen.V1.User
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
    | KeyMsg Evergreen.V1.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V1.Id.Id LobbyId)
    | PressedStartMatch
    | SoundLoaded String (Result Evergreen.V1.Audio.LoadError Evergreen.V1.Audio.Source)


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.Lobby.LobbyPreview
    , currentLobby :
        Maybe
            { id : Evergreen.V1.Id.Id LobbyId
            , lobby : Evergreen.V1.Lobby.Lobby
            }
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , initData : Maybe ( Evergreen.V1.Id.Id Evergreen.V1.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V1.Audio.LoadError Evergreen.V1.Audio.Source)
    }


type alias TimelineEvent =
    { userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , input : Evergreen.V1.Keyboard.Arrows.Direction
    }


type alias MatchState =
    { players :
        AssocList.Dict
            (Evergreen.V1.Id.Id Evergreen.V1.User.UserId)
            { position : ( Float, Float )
            , input : Evergreen.V1.Keyboard.Arrows.Direction
            }
    }


type MatchId
    = MatchId Never


type alias MatchPage_ =
    { startTime : Effect.Time.Posix
    , timeline : Evergreen.V1.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V1.Timeline.TimelineCache MatchState
    , userIds : List.Nonempty.Nonempty (Evergreen.V1.Id.Id Evergreen.V1.User.UserId)
    , matchId : Evergreen.V1.Id.Id MatchId
    }


type Page
    = LobbyPage LobbyData
    | MatchPage MatchPage_


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , pressedKeys : List Evergreen.V1.Keyboard.Key
    , previousKeys : List Evergreen.V1.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , page : Page
    , sounds : Evergreen.V1.Sounds.Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
    , userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V1.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.Lobby.Lobby
    , matches :
        AssocList.Dict
            (Evergreen.V1.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V1.Id.Id Evergreen.V1.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V1.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Evergreen.V1.Id.Id LobbyId)
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V1.Id.Id MatchId) Effect.Time.Posix Evergreen.V1.Keyboard.Arrows.Direction


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.Lobby.Lobby
    | CreateLobbyBroadcast (Evergreen.V1.Id.Id LobbyId) Evergreen.V1.Lobby.LobbyPreview
    | ClientInit (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V1.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V1.Lobby.Lobby)
    | JoinLobbyBroadcast (Evergreen.V1.Id.Id LobbyId) (Evergreen.V1.Id.Id Evergreen.V1.User.UserId)
    | StartMatchBroadcast (Evergreen.V1.Id.Id MatchId) (List.Nonempty.Nonempty (Evergreen.V1.Id.Id Evergreen.V1.User.UserId))
    | MatchInputBroadcast (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) Effect.Time.Posix Evergreen.V1.Keyboard.Arrows.Direction
