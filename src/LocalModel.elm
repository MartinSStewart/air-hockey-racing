module LocalModel exposing (Config, LocalModel, init, localModel, localMsgs, update, updateFromBackend)

import List.Nonempty exposing (Nonempty)
import Time


type LocalModel msg model
    = LocalModel { timeline : List ( Time.Posix, msg ), localModel : model, model : model }


type alias Config msg model =
    { msgEqual : msg -> msg -> Bool
    , update : msg -> model -> model
    }


init : model -> LocalModel msg model
init model =
    LocalModel { timeline = [], localModel = model, model = model }


update : Config msg model -> Time.Posix -> msg -> LocalModel msg model -> LocalModel msg model
update config time msg (LocalModel localModel_) =
    LocalModel
        { timeline = localModel_.timeline ++ [ ( time, msg ) ]
        , localModel = config.update msg localModel_.localModel
        , model = localModel_.model
        }


localModel : LocalModel msg model -> model
localModel (LocalModel localModel_) =
    localModel_.localModel


timeline : LocalModel msg model -> List ( Time.Posix, msg )
timeline (LocalModel localModel_) =
    localModel_.timeline


updateFromBackend : Config msg model -> Nonempty msg -> LocalModel msg model -> LocalModel msg model
updateFromBackend config msgs (LocalModel localModel_) =
    let
        newModel =
            List.Nonempty.foldl config.update localModel_.model msgs

        newLocalMsgs =
            List.Nonempty.foldl
                (\serverMsg localMsgs_ ->
                    List.foldl
                        (\localMsg ( newList, isDone ) ->
                            if isDone then
                                ( localMsg :: newList, True )

                            else if config.msgEqual (Tuple.second localMsg) serverMsg then
                                ( newList, True )

                            else
                                ( localMsg :: newList, False )
                        )
                        ( [], False )
                        localMsgs_
                        |> Tuple.first
                        |> List.reverse
                )
                localModel_.timeline
                msgs
    in
    LocalModel
        { timeline = newLocalMsgs
        , localModel = List.foldl config.update newModel (List.map Tuple.second newLocalMsgs)
        , model = newModel
        }
