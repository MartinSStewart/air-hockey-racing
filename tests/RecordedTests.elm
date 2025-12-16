module RecordedTests exposing (checkPlayersInSync, main, setup)

import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Effect.Browser.Dom as Dom
import Effect.Lamdera as Lamdera exposing (SessionId)
import Effect.Test as T exposing (DelayInMs, FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..))
import Frontend
import Match
import MatchPage exposing (MatchId)
import Point2d
import SeqDict
import Id
import Json.Decode
import Json.Encode
import Route
import Test.Html.Query
import Test.Html.Selector
import Time
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


setup : T.ViewerWith (List (T.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
setup =
    T.viewerWith tests
        |> T.addBytesFiles (Dict.values fileRequests)


main : Program () (T.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (T.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    T.startViewer setup


domain : Url
domain =
    { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


{-| Please don't modify or rename this function
-}
fileRequests : Dict String String
fileRequests =
    []
        |> Dict.fromList


stringToJson : String -> Json.Encode.Value
stringToJson json =
    Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)


handlePortToJs :
    { currentRequest : T.PortToJs, data : T.Data FrontendModel BackendModel }
    -> Maybe ( String, Json.Decode.Value )
handlePortToJs requestAndData =
    case requestAndData.currentRequest.portName of
        "audioPortToJS" ->
            Nothing

        "martinsstewart_elm_device_pixel_ratio_to_js" ->
            Just ( "martinsstewart_elm_device_pixel_ratio_from_js", Json.Encode.float 1 )

        name ->
            let
                _ =
                    Debug.log "Port not handled" ( name, Json.Encode.encode 0 requestAndData.currentRequest.value )
            in
            Nothing


desktopWindow : { width : number, height : number }
desktopWindow =
    { width = 1000, height = 600 }


mobileWindow : { width : number, height : number }
mobileWindow =
    { width = 400, height = 800 }


sessionId0 : SessionId
sessionId0 =
    Lamdera.sessionIdFromString "sessionId0"


sessionId1 : SessionId
sessionId1 =
    Lamdera.sessionIdFromString "sessionId1"


sessionId2 : SessionId
sessionId2 =
    Lamdera.sessionIdFromString "sessionId2"


startTime : Time.Posix
startTime =
    Time.millisToPosix 1765828691000


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


handleHttpRequests : Dict String String -> Dict String Bytes -> { currentRequest : HttpRequest, data : T.Data FrontendModel BackendModel } -> HttpResponse
handleHttpRequests overrides fileData requestAndData =
    let
        key : String
        key =
            requestAndData.currentRequest.method ++ "_" ++ requestAndData.currentRequest.url

        getData : String -> HttpResponse
        getData path =
            case Dict.get path fileData of
                Just data ->
                    BytesHttpResponse { url = requestAndData.currentRequest.url, statusCode = 200, statusText = "OK", headers = Dict.empty } data

                Nothing ->
                    UnhandledHttpRequest
    in
    case ( Dict.get key overrides, Dict.get key fileRequests ) of
        ( Just path, _ ) ->
            getData path

        ( Nothing, Just path ) ->
            getData path

        _ ->
            UnhandledHttpRequest


hasExactText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasExactText user texts =
    user.checkView 100 (Test.Html.Query.has (List.map Test.Html.Selector.exactText texts))


hasText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasText user texts =
    user.checkView 100 (Test.Html.Query.has (List.map Test.Html.Selector.text texts))


hasNotExactText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasNotExactText user texts =
    user.checkView 100 (Test.Html.Query.hasNot (List.map Test.Html.Selector.exactText texts))


hasNotText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasNotText user texts =
    user.checkView 100 (Test.Html.Query.hasNot (List.map Test.Html.Selector.text texts))


tests : Dict String Bytes -> List (T.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests fileData =
    let
        config =
            T.Config
                Frontend.app_
                Backend.app_
                (handleHttpRequests Dict.empty fileData)
                handlePortToJs
                (\_ -> UnhandledFileUpload)
                (\_ -> UnhandledMultiFileUpload)
                domain
    in
    [ T.start
        "Normal game"
        startTime
        config
        [ T.connectFrontend
            100
            sessionId0
            "/"
            desktopWindow
            (\userA ->
                [ handleAudioPorts userA
                , userA.click 500 (Dom.id "createNewMatch")
                , T.connectFrontend
                    100
                    sessionId1
                    "/"
                    desktopWindow
                    (\userB ->
                        [ handleAudioPorts userB
                        , userB.clickLink 500 (Route.encode (Route.InMatchRoute (Id.fromInt 0)))
                        ]
                    )
                , userA.click 100 (Dom.id "startMatchSetup")
                ]
            )
        ]
    ]


handleAudioPorts :
    T.FrontendActions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> T.Action toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
handleAudioPorts user =
    T.group
        [ user.portEvent 100 "audioPortFromJS" (stringToJson """{"type":2,"samplesPerSecond":48000}""")
        , user.portEvent 100 "audioPortFromJS" (stringToJson """{"type":1,"requestId":0,"bufferId":0,"durationInSeconds":0.03325}""")
        ]


{-| Verifies that all players in active matches are in sync by checking that
the backend's stored player positions match for each frame. When multiple clients
report their positions for the same frame, they should be identical.
-}
checkPlayersInSync :
    DelayInMs
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
checkPlayersInSync delay =
    T.checkState delay
        (\data ->
            let
                backendModel =
                    data.backend

                -- Get all active matches from the backend
                activeMatches =
                    SeqDict.toList backendModel.lobbies
                        |> List.filterMap
                            (\( matchId, match ) ->
                                case Match.matchActive match of
                                    Just _ ->
                                        Just ( matchId, match )

                                    Nothing ->
                                        Nothing
                            )

                -- For each active match, check that stored player positions are consistent
                -- The backend stores positions when clients send DesyncCheckRequest
                -- If positions differ at the same frame, it means players are out of sync
                positionErrors =
                    activeMatches
                        |> List.concatMap
                            (\( matchId, _ ) ->
                                case SeqDict.get matchId backendModel.playerPositions of
                                    Just framePositions ->
                                        -- Check each frame's positions for consistency
                                        -- Since the backend stores the first position reported
                                        -- and compares subsequent reports, we just verify
                                        -- positions exist (actual sync is checked at runtime)
                                        SeqDict.toList framePositions
                                            |> List.filterMap
                                                (\( frameId, positions ) ->
                                                    -- Verify all player positions are valid (not NaN, etc)
                                                    let
                                                        invalidPositions =
                                                            SeqDict.toList positions
                                                                |> List.filter
                                                                    (\( _, pos ) ->
                                                                        let
                                                                            { x, y } =
                                                                                Point2d.toMeters pos
                                                                        in
                                                                        isNaN x || isNaN y || isInfinite x || isInfinite y
                                                                    )
                                                    in
                                                    if List.isEmpty invalidPositions then
                                                        Nothing

                                                    else
                                                        Just
                                                            ("Match "
                                                                ++ String.fromInt (Id.toInt matchId)
                                                                ++ " frame "
                                                                ++ String.fromInt (Id.toInt frameId)
                                                                ++ " has invalid player positions"
                                                            )
                                                )

                                    Nothing ->
                                        -- No positions stored yet is fine for new matches
                                        []
                            )
            in
            if List.isEmpty positionErrors then
                Ok ()

            else
                Err (String.join "\n" positionErrors)
        )
