module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, node, text, span, div, input)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)


-- project

import Calculator exposing (Stack)


main : Program Never String String
main =
    Html.beginnerProgram
        { model = ""
        , update = always
        , view = view
        }


view : String -> Html String
view string =
    let
        result : Result String Stack
        result =
            string
                |> String.words
                |> List.filter ((/=) "")
                |> List.foldl
                    (Calculator.update >> Result.andThen)
                    (Ok [])
    in
        div
            []
            [ node "style" [] [ text "@import url(./style.css);" ]
            , input [ value string, onInput identity ] []
            , div [] [ result |> unpack viewError viewStack ]
            ]


viewStack : Stack -> Html a
viewStack stack =
    let
        numbers : List (Html a)
        numbers =
            stack
                |> List.reverse
                |> List.map (\n -> span [] [ text (toString n) ])
    in
        span
            [ class "stack" ]
            ([ text "[ " ]
                ++ (numbers |> List.intersperse (text ", "))
                ++ [ text " ]" ]
            )


viewError : String -> Html a
viewError message =
    span [ class "error" ] [ text message ]



-- helpers


unpack : (x -> b) -> (a -> b) -> Result x a -> b
unpack mapErr mapOk result =
    case result of
        Ok a ->
            mapOk a

        Err x ->
            mapErr x
