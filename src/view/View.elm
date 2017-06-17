module View.View exposing (view)

import Model.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, ismap, name, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (filter, length, map, range)
import Utils exposing (isNoun)


sameCard : Card -> Card -> Bool
sameCard card1 card2 =
    card1.face == card2.face


rootSelect : List String -> Html Msg
rootSelect roots =
    div [ class "rootSelect" ]
        [ select [ onInput SelectRoot ]
            (Html.option [ Html.Attributes.value "" ] [ text "All roots" ]
                :: List.map
                    (\root -> Html.option [ Html.Attributes.value root ] [ text root ])
                    roots
            )
        ]


filterSelect : Html Msg
filterSelect =
    div [ class "filter" ]
        [ fieldset []
            [ input
                [ type_ "radio"
                , name "partOfSpeech"
                , Html.Attributes.value "all"
                , onClick (SelectPartOfSpeech All)
                ]
                []
            , text " all"
            ]
        , fieldset []
            [ input
                [ type_ "radio"
                , name "partOfSpeech"
                , Html.Attributes.value "verbs"
                , onClick (SelectPartOfSpeech Verbs)
                ]
                []
            , text " verbs"
            ]
        , fieldset []
            [ input
                [ type_ "radio"
                , name "partOfSpeech"
                , Html.Attributes.value "nouns"
                , onClick (SelectPartOfSpeech Nouns)
                ]
                []
            , text " nouns"
            ]
        ]


filterWords : Model -> List (Html Msg)
filterWords model =
    let
        formatted =
            List.map
                (\pair ->
                    div [ class "table-row" ]
                        [ span [] [ text pair.german ]
                        , span [] [ text pair.english ]
                        ]
                )
    in
        case ( model.selectedRoot, model.selectedPartOfSpeech ) of
            ( Nothing, All ) ->
                formatted model.allWords

            ( Just selectedRoot, All ) ->
                List.filter (\pair -> pair.root == selectedRoot) model.allWords
                    |> formatted

            ( Just selectedRoot, _ ) ->
                List.filter (\pair -> pair.root == selectedRoot && isNoun pair) model.allWords
                    |> formatted

            ( Nothing, Nouns ) ->
                List.filter (\pair -> isNoun pair) model.allWords
                    |> formatted

            ( Nothing, Verbs ) ->
                List.filter (\pair -> (not << isNoun) pair) model.allWords
                    |> formatted


view : Model -> Html Msg
view model =
    let
        isFlipped card =
            case model.picked of
                NoCard ->
                    card.matched

                PickedOne c1 ->
                    sameCard c1 card || card.matched

                PickedTwo c1 c2 ->
                    sameCard c1 card || sameCard c2 card || card.matched

        isFinished =
            (length (filter (\c -> c.matched == False) model.cards)) == 0
    in
        div [ class "wrapper" ]
            [ nav [ class "navbar" ]
                [ h1 [] [ text "Wörter" ] ]
            , header []
                [ rootSelect model.roots
                , filterSelect
                ]
            , div [ class "container" ]
                [ div [ class "word-table" ]
                    (filterWords model)
                ]

            -- (Cards)
            -- (List.map (\c -> card (isFlipped c) c) model.cards)
            , modal isFinished
            ]


card : Bool -> Card -> Html Msg
card flipped c =
    div
        [ class
            (if flipped then
                "card--flipped"
             else
                "card"
            )
        , onClick (FlipCard c)
        ]
        [ div [ class "card__content" ]
            [ div [ class "card__front" ] [ text c.face ]
            , div [ class "card__back" ] [ text "Matching Game" ]
            ]
        ]


modal : Bool -> Html Msg
modal isFinished =
    div
        [ class
            (if isFinished then
                "modal--open"
             else
                "modal"
            )
        ]
        [ div [ class "modal__content" ]
            [ div [ class "modal__title" ] [ text "✨ You won! ✨" ]
            , div [ class "modal__text" ] [ text "Wanna play again? I bet you won't open all cards this time! 😉" ]
            , button [ class "modal__button", onClick Restart ] [ text "Restart" ]
            ]
        ]
