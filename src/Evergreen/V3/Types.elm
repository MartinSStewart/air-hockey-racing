module Evergreen.V3.Types exposing (..)

import AssocList
import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V3.Audio
import Evergreen.V3.Id
import Evergreen.V3.Keyboard
import Evergreen.V3.Keyboard.Arrows
import Evergreen.V3.Lobby
import Evergreen.V3.Sounds
import Evergreen.V3.Timeline
import Evergreen.V3.User
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
    | KeyMsg Evergreen.V3.Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedJoinLobby (Evergreen.V3.Id.Id LobbyId)
    | PressedStartMatch
    | SoundLoaded String (Result Evergreen.V3.Audio.LoadError Evergreen.V3.Audio.Source)


type alias LobbyData =
    { lobbies : AssocList.Dict (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.Lobby.LobbyPreview
    , currentLobby :
        Maybe
            { id : Evergreen.V3.Id.Id LobbyId
            , lobby : Evergreen.V3.Lobby.Lobby
            }
    }


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , initData : Maybe ( Evergreen.V3.Id.Id Evergreen.V3.User.UserId, LobbyData )
    , sounds : AssocList.Dict String (Result Evergreen.V3.Audio.LoadError Evergreen.V3.Audio.Source)
    }


type alias TimelineEvent =
    { userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , input : Evergreen.V3.Keyboard.Arrows.Direction
    }


type alias MatchState =
    { players :
        AssocList.Dict
            (Evergreen.V3.Id.Id Evergreen.V3.User.UserId)
            { position : ( Float, Float )
            , input : Evergreen.V3.Keyboard.Arrows.Direction
            }
    }


type MatchId
    = MatchId Never


type alias MatchPage_ =
    { startTime : Effect.Time.Posix
    , localStartTime : Effect.Time.Posix
    , timeline : Evergreen.V3.Timeline.Timeline TimelineEvent
    , timelineCache : Evergreen.V3.Timeline.TimelineCache MatchState
    , userIds : List.Nonempty.Nonempty (Evergreen.V3.Id.Id Evergreen.V3.User.UserId)
    , matchId : Evergreen.V3.Id.Id MatchId
    }


type Page
    = LobbyPage LobbyData
    | MatchPage MatchPage_


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , windowSize : WindowSize
    , pressedKeys : List Evergreen.V3.Keyboard.Key
    , previousKeys : List Evergreen.V3.Keyboard.Key
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , page : Page
    , sounds : Evergreen.V3.Sounds.Sounds
    , lastButtonPress : Maybe Effect.Time.Posix
    , userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V3.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        AssocList.Dict
            Effect.Lamdera.SessionId
            { clientIds : AssocList.Dict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
            }
    , users : AssocList.Dict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) BackendUserData
    , lobbies : AssocList.Dict (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.Lobby.Lobby
    , matches :
        AssocList.Dict
            (Evergreen.V3.Id.Id MatchId)
            { users : List.Nonempty.Nonempty (Evergreen.V3.Id.Id Evergreen.V3.User.UserId)
            }
    , counter : Int
    }


type alias FrontendMsg =
    Evergreen.V3.Audio.Msg FrontendMsg_


type ToBackend
    = CreateLobbyRequest
    | JoinLobbyRequest (Evergreen.V3.Id.Id LobbyId)
    | StartMatchRequest
    | MatchInputRequest (Evergreen.V3.Id.Id MatchId) Effect.Time.Posix Evergreen.V3.Keyboard.Arrows.Direction


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | GotTimeForUpdateFromFrontend Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Effect.Time.Posix


type JoinLobbyError
    = LobbyNotFound


type ToFrontend
    = CreateLobbyResponse (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.Lobby.Lobby
    | CreateLobbyBroadcast (Evergreen.V3.Id.Id LobbyId) Evergreen.V3.Lobby.LobbyPreview
    | ClientInit (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) LobbyData
    | JoinLobbyResponse (Evergreen.V3.Id.Id LobbyId) (Result JoinLobbyError Evergreen.V3.Lobby.Lobby)
    | JoinLobbyBroadcast (Evergreen.V3.Id.Id LobbyId) (Evergreen.V3.Id.Id Evergreen.V3.User.UserId)
    | StartMatchBroadcast (Evergreen.V3.Id.Id MatchId) Effect.Time.Posix (List.Nonempty.Nonempty (Evergreen.V3.Id.Id Evergreen.V3.User.UserId))
    | MatchInputBroadcast (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) Effect.Time.Posix Evergreen.V3.Keyboard.Arrows.Direction
