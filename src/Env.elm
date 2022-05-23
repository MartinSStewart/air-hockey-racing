module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.


isProduction_ : String
isProduction_ =
    "False"


isProduction : Bool
isProduction =
    case String.toLower isProduction_ |> String.trim of
        "true" ->
            True

        "false" ->
            False

        _ ->
            False


domain : String
domain =
    "localhost:8000"
