module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events


-- project

import Calculator exposing (Stack)


main : Program Never String String
main =
    Html.beginnerProgram
        { model = ""
        , update = \s _ -> s
        , view = view
        }


view : String -> Html String
view string =
    let
        results =
            string
                |> String.words
                |> List.filter ((/=) "")
                |> tracel
                    (Calculator.update >> Result.andThen)
                    (Ok [])
    in
        Html.div
            []
            [ Html.node "style" [] [ Html.text "@import url(./style.css);" ]
            , Html.input
                [ Html.Attributes.value string
                , Html.Events.onInput identity
                ]
                []
            , viewResults results
            ]


type alias CalcResult =
    Result String Stack


viewResults : ( List ( CalcResult, String ), CalcResult ) -> Html a
viewResults ( log, result ) =
    let
        items =
            log
                |> List.filterMap (\( r, s ) -> r |> Result.map ((,) s) |> Result.toMaybe)
                |> List.map (uncurry viewItem)
    in
        Html.div
            []
            [ Html.ol [] items
            , result |> unpack viewError viewStack
            ]


viewItem : String -> Stack -> Html a
viewItem s stack =
    Html.li
        []
        [ viewStack stack
        , Html.text " "
        , Html.span [ Html.Attributes.class "next" ] [ Html.text s ]
        ]


viewStack : Stack -> Html a
viewStack stack =
    let
        numbers =
            stack |> List.reverse |> List.map (toString >> Html.text >> List.singleton >> Html.span [])
    in
        Html.span
            [ Html.Attributes.class "stack" ]
            ([ Html.text "[ " ]
                ++ (numbers |> List.intersperse (Html.text ", "))
                ++ [ Html.text " ]" ]
            )


viewError : String -> Html a
viewError message =
    Html.span [ Html.Attributes.class "error" ] [ Html.text message ]



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
