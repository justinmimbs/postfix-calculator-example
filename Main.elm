module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)


-- project

import Calculator exposing (Stack)


main : Html a
main =
    let
        results =
            "5 1 2 + 4 * + 3 -"
                |> String.words
                |> tracel
                    (Result.andThen << Calculator.update)
                    (Ok [])
    in
        viewResults results


viewResults : ( List ( Result String Stack, String ), Result String Stack ) -> Html a
viewResults ( log, result ) =
    let
        items =
            log
                |> List.filterMap (\( r, s ) -> r |> Result.map ((,) s) |> Result.toMaybe)
                |> List.map (uncurry viewItem)
    in
        Html.ol
            []
            (items
                ++ [ Html.li [] [ result |> unpack viewError viewStack ] ]
            )


viewItem : String -> Stack -> Html a
viewItem s stack =
    Html.li
        []
        [ viewStack stack
        , Html.text " "
        , Html.span [] [ Html.text s ]
        ]


viewStack : Stack -> Html a
viewStack stack =
    Html.span [] [ Html.text ("[ " ++ (stack |> List.reverse |> List.map toString |> String.join ", ") ++ " ]") ]


viewError : String -> Html a
viewError message =
    Html.span [] [ Html.text message ]



-- helpers


tracel : (a -> r -> r) -> r -> List a -> ( List ( r, a ), r )
tracel f r0 =
    List.foldl
        (\x ( rs, r ) ->
            ( rs ++ [ ( r, x ) ], f x r )
        )
        ( [], r0 )


unpack : (x -> b) -> (a -> b) -> Result x a -> b
unpack mapErr mapOk result =
    case result of
        Ok a ->
            mapOk a

        Err x ->
            mapErr x
