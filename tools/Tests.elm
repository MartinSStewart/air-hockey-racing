module Tests exposing (Test(..), TestResult(..), main, test, testAssert, testInit, testMap, testSingle, time)

import Element exposing (Element)
import Element.Background
import Html exposing (Html)
import Id
import Time
import Timeline


type TestResult
    = Passed
    | Failed String


main : Html msg
main =
    Element.layout [] <|
        Element.column [ Element.padding 16 ]
            [ test "Simple timeline" <|
                (testInit (Timeline.init 0)
                    |> testMap (Timeline.getStateAt updateFunc (Id.fromInt 3))
                    |> testAssert
                        (Tuple.first
                            >> .cache
                            >> areEqual [ ( Id.fromInt 3, 3 ), ( Id.fromInt 2, 2 ), ( Id.fromInt 1, 1 ) ]
                        )
                    |> testAssert (Tuple.second >> areEqual 3)
                    |> testMap (Tuple.first >> Timeline.addInput (Id.fromInt 2) True)
                    |> testAssert (.cache >> areEqual [ ( Id.fromInt 2, 2 ), ( Id.fromInt 1, 1 ) ])
                    |> testMap (Timeline.getStateAt updateFunc (Id.fromInt 4))
                    |> testAssert
                        (Tuple.first
                            >> .cache
                            >> areEqual
                                [ ( Id.fromInt 4, 5 )
                                , ( Id.fromInt 3, 4 )
                                , ( Id.fromInt 2, 2 )
                                , ( Id.fromInt 1, 1 )
                                ]
                        )
                )
            ]


areEqual : a -> a -> TestResult
areEqual expected actual =
    if expected == actual then
        Passed

    else
        Failed ("Expected: " ++ Debug.toString expected ++ " but got " ++ Debug.toString actual)


updateFunc : List Bool -> number -> number
updateFunc inputs state =
    List.foldl
        (\input newState ->
            (if input then
                1

             else
                -1
            )
                + newState
        )
        state
        inputs
        + 1


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
