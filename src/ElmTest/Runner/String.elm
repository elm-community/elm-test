module ElmTest.Runner.String exposing (runDisplay, run)

{-| Run a test suite and display it as a string.

# Run
@docs runDisplay, run

-}

import List exposing ((::))
import List
import String
import ElmTest.Run as Run
import ElmTest.Test exposing (..)


-- | Some pretty printing stuff. Should be factored into a pretty printing library.


vcat : List String -> String
vcat =
  String.concat << List.intersperse "\n"


replicate : Int -> Char -> String
replicate n c =
  let
    go n =
      if n <= 0 then
        []
      else
        c :: go (n - 1)
  in
    String.fromList << go <| n


indent : Int -> String -> String
indent n =
  let
    indents =
      replicate n ' '
  in
    vcat << List.map (String.append indents) << String.lines


pretty : Int -> Run.Result -> List ( String, Run.Result )
pretty n result =
  let
    passed =
      Run.pass result
  in
    case result of
      Run.Pass name ->
        [ ( indent n <| name ++ ": passed.", result ) ]

      Run.Fail name msg ->
        [ ( indent n <| name ++ ": FAILED. " ++ msg, result ) ]

      Run.Report name r ->
        let
          msg =
            "Test Suite: "
              ++ name
              ++ ": "
              ++ if passed then
                  "all tests passed"
                 else
                  "FAILED"

          allPassed =
            Run.failedTests result == 0

          subResults =
            if allPassed then
              []
            else
              List.concatMap (pretty (n + 2)) r.results
        in
          ( indent n msg, result ) :: subResults


{-| Undocumented.
-}
run : Test -> List ( String, Run.Result )
run t =
  let
    result =
      Run.run t

    tests =
      case t of
        TestCase n a ->
          [ TestCase n a ]

        Suite n ts ->
          ts

    passedTests' =
      Run.passedTests result

    passedSuites' =
      Run.passedSuites result

    failedTests' =
      Run.failedTests result

    failedSuites' =
      Run.failedSuites result

    summary =
      vcat
        << List.map (indent 2)
        <| [ toString (numberOfSuites t) ++ " suites run, containing " ++ toString (numberOfTests t) ++ " tests"
           , if failedTests' == 0 then
              "All tests passed"
             else
              toString passedSuites' ++ " suites and " ++ toString passedTests' ++ " tests passed"
           , if failedTests' == 0 then
              ""
             else
              toString failedSuites' ++ " suites and " ++ toString failedTests' ++ " tests failed"
           ]

    --- TODO: implement results printing
    allPassed =
      if failedTests' == 0 then
        Run.Pass ""
      else
        Run.Fail "" ""

    results' =
      case allPassed of
        Run.Pass _ ->
          [ ( "", allPassed ) ]

        _ ->
          pretty 0 result
  in
    ( summary, allPassed ) :: results'


{-| Runs a test or test suite. Returns the report as a String
-}
runDisplay : Test -> String
runDisplay t =
  case run t of
    ( summary, _ ) :: results ->
      vcat <| (summary ++ "\n") :: List.map fst results

    _ ->
      ""
