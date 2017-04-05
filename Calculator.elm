module Calculator
    exposing
        ( Stack
        , update
        )

import Dict exposing (Dict)


type alias Stack =
    List Float


type alias Operator =
    Float -> Float -> Float


type Token
    = Op Operator
    | Val Float


operators : Dict String Operator
operators =
    Dict.fromList
        [ ( "+", (+) )
        , ( "-", (-) )
        , ( "*", (*) )
        , ( "/", (/) )
        ]


toToken : String -> Maybe Token
toToken s =
    (s |> String.toFloat |> Result.toMaybe |> Maybe.map Val)
        |> orElse (Dict.get s operators |> Maybe.map Op)


update : String -> Stack -> Result String Stack
update s stack =
    case toToken s of
        Just (Val x) ->
            Ok (x :: stack)

        Just (Op op) ->
            case stack of
                x :: y :: rest ->
                    Ok (op y x :: rest)

                _ ->
                    Err ("Insufficient values in stack to perform (" ++ s ++ ")")

        Nothing ->
            Err ("Invalid input: " ++ s)



-- helpers


orElse : Maybe a -> Maybe a -> Maybe a
orElse mb ma =
    case ma of
        Just _ ->
            ma

        _ ->
            mb
