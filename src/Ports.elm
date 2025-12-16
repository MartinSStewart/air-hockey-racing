port module Ports exposing (audioFromJs, audioToJs, devicePixelRatioRequest, devicePixelRatioResponse, writeToClipboard)

import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Subscription as Subscription exposing (Subscription)
import Json.Decode
import Json.Encode


port martinsstewart_elm_device_pixel_ratio_from_js : (Json.Decode.Value -> msg) -> Sub msg


port martinsstewart_elm_device_pixel_ratio_to_js : () -> Cmd msg


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


port supermario_copy_to_clipboard_to_js : Json.Encode.Value -> Cmd msg


audioFromJs : (Json.Decode.Value -> msg) -> Subscription FrontendOnly msg
audioFromJs msg =
    Subscription.fromJs "audioPortFromJS" audioPortFromJS msg


audioToJs : Json.Encode.Value -> Command FrontendOnly toMsg msg
audioToJs json =
    --let
    --    _ =
    --        Debug.log "audioToJs" (Json.Encode.encode 0 json)
    --in
    Command.sendToJs "audioPortToJS" audioPortToJS json


devicePixelRatioRequest : Command FrontendOnly toMsg msg
devicePixelRatioRequest =
    Command.sendToJs
        "martinsstewart_elm_device_pixel_ratio_to_js"
        (\_ -> martinsstewart_elm_device_pixel_ratio_to_js ())
        Json.Encode.null


devicePixelRatioResponse : (Float -> msg) -> Subscription FrontendOnly msg
devicePixelRatioResponse msg =
    Subscription.fromJs
        "martinsstewart_elm_device_pixel_ratio_from_js"
        martinsstewart_elm_device_pixel_ratio_from_js
        (\json ->
            case Json.Decode.decodeValue Json.Decode.float json of
                Ok ok ->
                    msg ok

                Err _ ->
                    msg 1
        )


writeToClipboard : String -> Command FrontendOnly toMsg msg
writeToClipboard text =
    Command.sendToJs
        "supermario_copy_to_clipboard_to_js"
        supermario_copy_to_clipboard_to_js
        (Json.Encode.string text)
