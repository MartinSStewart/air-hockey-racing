module RecordedTests exposing (checkPlayersInSync, main, setup)

import Audio
import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Effect.Browser.Dom as Dom
import Effect.Lamdera as Lamdera exposing (ClientId, SessionId)
import Effect.Test as T exposing (DelayInMs, FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..))
import Frontend
import Id
import Json.Decode
import Json.Encode
import Match
import MatchPage exposing (MatchId, MatchLocalOnly(..))
import Point2d
import Route
import SeqDict
import Test.Html.Query
import Test.Html.Selector
import Time
import Timeline
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendModel_(..), FrontendMsg, Page(..), ToBackend, ToFrontend)
import User
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
each frontend's computed player positions match at the same frame.
-}
checkPlayersInSync :
    DelayInMs
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
checkPlayersInSync delay =
    T.checkState delay
        (\data ->
            let
                -- Extract active match data from each frontend
                -- Returns list of (matchId, clientId, frameId, playerPositions)
                frontendMatchData :
                    List
                        { matchId : Id.Id MatchId
                        , clientId : ClientId
                        , frameId : Id.Id Timeline.FrameId
                        , positions : SeqDict.SeqDict (Id.Id User.UserId) { x : Float, y : Float }
                        }
                frontendMatchData =
                    SeqDict.toList data.frontends
                        |> List.filterMap
                            (\( clientId, frontendModel ) ->
                                case Audio.getUserModel frontendModel of
                                    Loaded loaded ->
                                        case loaded.page of
                                            MatchPage matchPage ->
                                                case matchPage.matchData of
                                                    MatchActiveLocal activeLocal ->
                                                        case activeLocal.timelineCache of
                                                            Ok cache ->
                                                                let
                                                                    ( frameId, state ) =
                                                                        Timeline.getOldestCachedState cache
                                                                in
                                                                Just
                                                                    { matchId = matchPage.lobbyId
                                                                    , clientId = clientId
                                                                    , frameId = frameId
                                                                    , positions =
                                                                        SeqDict.map
                                                                            (\_ player ->
                                                                                Point2d.toMeters player.position
                                                                            )
                                                                            state.players
                                                                    }

                                                            Err _ ->
                                                                Nothing

                                                    MatchSetupLocal _ ->
                                                        Nothing

                                            _ ->
                                                Nothing

                                    Loading _ ->
                                        Nothing
                            )

                -- Group frontends by matchId
                groupedByMatch :
                    List
                        ( Id.Id MatchId
                        , List
                            { clientId : ClientId
                            , frameId : Id.Id Timeline.FrameId
                            , positions : SeqDict.SeqDict (Id.Id User.UserId) { x : Float, y : Float }
                            }
                        )
                groupedByMatch =
                    frontendMatchData
                        |> List.foldl
                            (\item acc ->
                                let
                                    existing =
                                        List.filter (\( mid, _ ) -> mid == item.matchId) acc
                                            |> List.head
                                            |> Maybe.map Tuple.second
                                            |> Maybe.withDefault []

                                    newItem =
                                        { clientId = item.clientId
                                        , frameId = item.frameId
                                        , positions = item.positions
                                        }
                                in
                                case existing of
                                    [] ->
                                        ( item.matchId, [ newItem ] ) :: acc

                                    _ ->
                                        List.map
                                            (\( mid, items ) ->
                                                if mid == item.matchId then
                                                    ( mid, newItem :: items )

                                                else
                                                    ( mid, items )
                                            )
                                            acc
                            )
                            []

                -- Check each match group for sync issues
                syncErrors : List String
                syncErrors =
                    groupedByMatch
                        |> List.concatMap
                            (\( matchId, frontends ) ->
                                case frontends of
                                    [] ->
                                        []

                                    [ _ ] ->
                                        -- Only one frontend, nothing to compare
                                        []

                                    first :: rest ->
                                        -- Compare all frontends against the first one
                                        -- Only compare at the minimum common frameId
                                        let
                                            minFrameId =
                                                List.foldl
                                                    (\f minId ->
                                                        if Id.toInt f.frameId < Id.toInt minId then
                                                            f.frameId

                                                        else
                                                            minId
                                                    )
                                                    first.frameId
                                                    rest

                                            -- Get positions at minFrameId (use cached positions as approximation)
                                            positionsMatch pos1 pos2 =
                                                let
                                                    tolerance =
                                                        0.001
                                                in
                                                abs (pos1.x - pos2.x)
                                                    < tolerance
                                                    && abs (pos1.y - pos2.y)
                                                    < tolerance

                                            comparePositions refPositions other =
                                                SeqDict.toList refPositions
                                                    |> List.filterMap
                                                        (\( userId, refPos ) ->
                                                            case SeqDict.get userId other.positions of
                                                                Just otherPos ->
                                                                    if positionsMatch refPos otherPos then
                                                                        Nothing

                                                                    else
                                                                        Just
                                                                            ("Player "
                                                                                ++ String.fromInt (Id.toInt userId)
                                                                                ++ " position mismatch: ("
                                                                                ++ String.fromFloat refPos.x
                                                                                ++ ", "
                                                                                ++ String.fromFloat refPos.y
                                                                                ++ ") vs ("
                                                                                ++ String.fromFloat otherPos.x
                                                                                ++ ", "
                                                                                ++ String.fromFloat otherPos.y
                                                                                ++ ")"
                                                                            )

                                                                Nothing ->
                                                                    -- Player not found in other frontend
                                                                    Just
                                                                        ("Player "
                                                                            ++ String.fromInt (Id.toInt userId)
                                                                            ++ " missing in other frontend"
                                                                        )
                                                        )
                                        in
                                        rest
                                            |> List.concatMap (comparePositions first.positions)
                                            |> List.map
                                                (\err ->
                                                    "Match "
                                                        ++ String.fromInt (Id.toInt matchId)
                                                        ++ " at frame "
                                                        ++ String.fromInt (Id.toInt minFrameId)
                                                        ++ ": "
                                                        ++ err
                                                )
                            )
            in
            if List.isEmpty syncErrors then
                Ok ()

            else
                Err (String.join "\n" syncErrors)
        )
