module Tests exposing (..)

import Array
import Ascii exposing (Ascii)
import BackendLogic exposing (Effect(..))
import Bounds exposing (Bounds)
import Change exposing (LocalChange(..))
import Dict
import Element exposing (Element)
import Element.Background
import EverySet
import Grid
import GridCell
import Helper exposing (Coord)
import Html exposing (Html)
import List.Nonempty as Nonempty
import LocalGrid
import LocalModel
import Time
import Types exposing (BackendModel, ClientId, FrontendModel, SessionId, ToBackend(..), ToFrontend(..))
import Units exposing (CellUnit)
import User


type TestResult
    = Passed
    | Failed String


main : Html msg
main =
    Element.layout [] <|
        Element.column [ Element.padding 16 ]
            [
            ]


test : String -> Test model -> Element msg
test name (Test testResults _) =
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.alignTop, Element.padding 8 ] (Element.text name)
        , List.map
            (\testResult ->
                case testResult of
                    Failed error ->
                        Element.paragraph
                            [ Element.Background.color (Element.rgb 1 0 0)
                            , Element.padding 8
                            , Element.width Element.fill
                            ]
                            [ Element.text error ]

                    Passed ->
                        Element.el
                            [ Element.Background.color (Element.rgb 0 1 0)
                            , Element.padding 8
                            , Element.width Element.fill
                            ]
                            (Element.text "Passed")
            )
            testResults
            |> Element.column [ Element.width Element.fill ]
        ]


newUserState : ( BackendModel, List BackendLogic.Effect )
newUserState =
    BackendLogic.init
        |> BackendLogic.updateFromFrontend
            "session0"
            "client0"
            (RequestData smallViewBounds)


smallViewBounds : Bounds Units.CellUnit
smallViewBounds =
    Bounds.bounds ( Units.cellUnit 0, Units.cellUnit 0 ) ( Units.cellUnit 1, Units.cellUnit 1 )


time seconds =
    Time.millisToPosix ((seconds * 1000) + 10000000)


type Test model
    = Test (List TestResult) model


testAssert : (model -> TestResult) -> Test model -> Test model
testAssert assertion (Test results model) =
    Test (results ++ [ assertion model ]) model


testInit : model -> Test model
testInit model =
    Test [] model


testMap : (a -> b) -> Test a -> Test b
testMap mapFunc (Test results model) =
    Test results (mapFunc model)


testSingle : TestResult -> Test ()
testSingle result =
    Test [ result ] ()


asciiA =
    Ascii.fromChar 'a' |> Maybe.withDefault Ascii.default


checkGridValue : ( Coord CellUnit, Int ) -> Maybe Ascii -> LocalModel.LocalModel a LocalGrid.LocalGrid -> TestResult
checkGridValue ( cellPosition, localPosition ) value =
    LocalGrid.localModel
        >> .grid
        >> Grid.getCell cellPosition
        >> Maybe.andThen (GridCell.flatten EverySet.empty EverySet.empty >> Array.get localPosition >> Maybe.map Tuple.second)
        >> (\ascii ->
                if ascii == value then
                    Passed

                else
                    Failed
                        ("Wrong value found in grid "
                            ++ Debug.toString ascii
                            ++ " at cell position "
                            ++ Debug.toString cellPosition
                            ++ " and local position "
                            ++ Debug.toString localPosition
                        )
           )
