module Evergreen.V2.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V2.Audio
import Evergreen.V2.Id
import Evergreen.V2.Keyboard
import Evergreen.V2.Keyboard.Arrows
import Evergreen.V2.Lobby
import Evergreen.V2.Sounds
import Evergreen.V2.Timeline
import Evergreen.V2.User
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
    | KeyMsg Evergreen.V2.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V2.Id.Id LobbyId)
    | PressedStartMatch
    | SoundLoaded String (Result Evergreen.V2.Audio.LoadError Evergreen.V2.Audio.Source)


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V2.Id.Id LobbyId) Evergreen.V2.Lobby.LobbyPreview
    , currentLobby :
        Maybe
            { id : Evergreen.V2.Id.Id LobbyId
            , lobby : Evergreen.V2.Lobby.Lobby
            }
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , initData : Maybe ( Evergreen.V2.Id.Id Evergreen.V2.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V2.Audio.LoadError Evergreen.V2.Audio.Source)
    }


type alias TimelineEvent =
    { userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    , input : Evergreen.V2.Keyboard.Arrows.Direction
    }


type alias MatchState =
    { players :
        AssocList.Dict
            (Evergreen.V2.Id.Id Evergreen.V2.User.UserId)
            { position : ( Float, Float )
            , input : Evergreen.V2.Keyboard.Arrows.Direction
            }
    }


type MatchId
    = MatchId Never


type alias MatchPage_ =
    { startTime : Effect.Time.Posix
    , localStartTime : Effect.Time.Posix
    , timeline : Evergreen.V2.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V2.Timeline.TimelineCache MatchState
    , userIds : List.Nonempty.Nonempty (Evergreen.V2.Id.Id Evergreen.V2.User.UserId)
    , matchId : Evergreen.V2.Id.Id MatchId
    }


type Page
    = LobbyPage LobbyData
    | MatchPage MatchPage_


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , pressedKeys : List Evergreen.V2.Keyboard.Key
    , previousKeys : List Evergreen.V2.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , page : Page
    , sounds : Evergreen.V2.Sounds.Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
    , userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V2.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V2.Id.Id LobbyId) Evergreen.V2.Lobby.Lobby
    , matches :
        AssocList.Dict
            (Evergreen.V2.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V2.Id.Id Evergreen.V2.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V2.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Evergreen.V2.Id.Id LobbyId)
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V2.Id.Id MatchId) Effect.Time.Posix Evergreen.V2.Keyboard.Arrows.Direction


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V2.Id.Id LobbyId) Evergreen.V2.Lobby.Lobby
    | CreateLobbyBroadcast (Evergreen.V2.Id.Id LobbyId) Evergreen.V2.Lobby.LobbyPreview
    | ClientInit (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V2.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V2.Lobby.Lobby)
    | JoinLobbyBroadcast (Evergreen.V2.Id.Id LobbyId) (Evergreen.V2.Id.Id Evergreen.V2.User.UserId)
    | StartMatchBroadcast (Evergreen.V2.Id.Id MatchId) (List.Nonempty.Nonempty (Evergreen.V2.Id.Id Evergreen.V2.User.UserId))
    | MatchInputBroadcast (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) Effect.Time.Posix Evergreen.V2.Keyboard.Arrows.Direction
