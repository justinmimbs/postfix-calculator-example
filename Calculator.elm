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


operators : Dict String Operator
operators =
    Dict.fromList
        [ ( "+", (+) )
        , ( "-", (-) )
        , ( "*", (*) )
        , ( "/", (/) )
        ]


type Token
    = Val Float
    | Op Operator


toValue : String -> Maybe Float
toValue =
    String.toFloat >> Result.toMaybe


toOperator : String -> Maybe Operator
toOperator =
    (flip Dict.get) operators



-- 1


toToken1 : String -> Maybe Token
toToken1 s =
    case toValue s of
        Just n ->
            Just (Val n)

        Nothing ->
            case toOperator s of
                Just op ->
                    Just (Op op)

                Nothing ->
                    Nothing



-- 2


toToken2 : String -> Maybe Token
toToken2 s =
    or
        (toValue s |> Maybe.map Val)
        (toOperator s |> Maybe.map Op)



--3


toToken3 : String -> Maybe Token
toToken3 s =
    Val <$> toValue s <|> Op <$> toOperator s



-- update


update : String -> Stack -> Result String Stack
update s stack =
    case toToken3 s of
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


or : Maybe a -> Maybe a -> Maybe a
or ma mb =
    case ma of
        Just _ ->
            ma

        _ ->
            mb


infixl 3 <$>
(<$>) : (a -> b) -> Maybe a -> Maybe b
(<$>) =
    Maybe.map


infixl 1 <|>
(<|>) : Maybe a -> Maybe a -> Maybe a
(<|>) =
    or
