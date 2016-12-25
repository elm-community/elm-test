module Test.Message exposing (failureMessage)

import String
import Test.Expectation exposing (Reason(..))


verticalBar : String -> String -> String -> String
verticalBar comparison expected actual =
    [ actual
    , "╷"
    , "│ " ++ comparison
    , "╵"
    , expected
    ]
        |> String.join "\n"


failureMessage : { given : Maybe String, description : String, reason : Reason } -> String
failureMessage { given, description, reason } =
    case reason of
        Custom ->
            description

        Equals e a ->
            verticalBar description e a

        Comparison e a ->
            verticalBar description e a

        TODO ->
            "TODO: " ++ description

        ListDiff e a ( i, itemE, itemA ) ->
            String.join ""
                [ verticalBar description e a
                , "\n\nThe first diff is at index index "
                , toString i
                , ": it was `"
                , itemA
                , "`, but `"
                , itemE
                , "` was expected."
                ]

        CollectionDiff { expected, actual, extra, missing } ->
            let
                extraStr =
                    if List.isEmpty extra then
                        ""
                    else
                        "\nThese keys are extra: "
                            ++ (extra |> String.join ", " |> \d -> "[ " ++ d ++ " ]")

                missingStr =
                    if List.isEmpty missing then
                        ""
                    else
                        "\nThese keys are missing: "
                            ++ (missing |> String.join ", " |> \d -> "[ " ++ d ++ " ]")
            in
                String.join ""
                    [ verticalBar description expected actual
                    , "\n"
                    , extraStr
                    , missingStr
                    ]
