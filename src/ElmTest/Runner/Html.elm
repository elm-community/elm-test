module ElmTest.Runner.Html exposing (runSuite)

{-| Run a test suite as an Html Program

# Run
@docs runSuite

-}

import List
import String
import ElmTest.Test exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.String as String
import Html.App as Html
import Html.Attributes exposing (style)
import Html


{-| Run a test and create an Html display of the results
-}
runDisplay : Test -> Html.Html ()
runDisplay tests =
    case String.run tests of
        ( summary, allPassed ) :: results ->
            Html.pre []
                <| List.append [ Html.div [] [ Html.text summary ] ]
                <| List.map displayTestResult results

        _ ->
            Debug.crash ""


{-| Run the test suite and display as Html.
-}
runSuite : Test -> Program Never
runSuite tests =
    Html.beginnerProgram
        { model = tests
        , view = (\model -> runDisplay model)
        , update = (\_ model -> model)
        }


displayTestResult : ( String, Run.Result ) -> Html.Html ()
displayTestResult ( message, result ) =
    let
        passStyle =
            style [ ( "background-color", "#0AE00A" ) ]

        failStyle =
            style [ ( "background-color", "#FF0000" ) ]

        resultStyle =
            case result of
                Run.Pass _ ->
                    passStyle

                Run.Fail _ _ ->
                    failStyle

                Run.Report _ summary ->
                    if List.isEmpty summary.failures then
                        passStyle
                    else
                        failStyle
    in
        Html.div [ resultStyle ] [ Html.text message ]
