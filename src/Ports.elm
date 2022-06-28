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


audioFromJs =
    Subscription.fromJs "audioPortFromJS" audioPortFromJS


audioToJs =
    Command.sendToJs "audioPortToJS" audioPortToJS


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
