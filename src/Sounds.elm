module Sounds exposing (Sounds, loadingFinished, requestSounds)

import Audio exposing (AudioCmd)
import Dict exposing (Dict)


type alias Sounds =
    { buttonPress : Audio.Source }


soundUrls : List String
soundUrls =
    [ "./blip.mp3" ]


requestSounds : (String -> Result Audio.LoadError Audio.Source -> msg) -> AudioCmd msg
requestSounds loadedSound =
    List.map (\url -> Audio.loadAudio (loadedSound url) url) soundUrls |> Audio.cmdBatch


loadingFinished : Dict String (Result Audio.LoadError Audio.Source) -> Maybe Sounds
loadingFinished sounds =
    let
        loadSound : ( List String, Maybe (Audio.Source -> b) ) -> ( List String, Maybe b )
        loadSound ( urlsLeft, soundsFinished ) =
            case ( urlsLeft, soundsFinished ) of
                ( head :: rest, Just soundsFinished_ ) ->
                    case Dict.get head sounds of
                        Just (Ok source) ->
                            ( rest, soundsFinished_ source |> Just )

                        _ ->
                            ( [], Nothing )

                _ ->
                    ( [], Nothing )
    in
    loadSound ( soundUrls, Just Sounds )
        |> Tuple.second
