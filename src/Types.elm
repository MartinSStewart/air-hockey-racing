module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BackendUserData
    , ClientId
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel(..)
    , FrontendMsg(..)
    , SessionId
    , ToBackend(..)
    , ToFrontend(..)
    , WindowSize
    , WorldPixel
    )

import Browser exposing (UrlRequest)
import Browser.Navigation
import Dict exposing (Dict)
import EverySet exposing (EverySet)
import Keyboard
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Time
import Url exposing (Url)
import User exposing (RawUserId, UserId)
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
    }


type alias WindowSize =
    { width : Quantity Int Pixels, height : Quantity Int Pixels }


type alias FrontendLoaded =
    { key : Browser.Navigation.Key
    , windowSize : WindowSize
    , pressedKeys : List Keyboard.Key
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    }


type alias BackendModel =
    { userSessions : Dict SessionId { clientIds : Dict ClientId (), userId : UserId }
    , users : Dict RawUserId BackendUserData
    }


type alias BackendUserData =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | KeyMsg Keyboard.Msg
    | WindowResized WindowSize
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Time.Posix


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
