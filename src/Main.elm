module Main exposing (..)

import Debug exposing (log)
import View.View exposing (view)
import Model.Model exposing (..)
import Navigation
import Time exposing (Time, millisecond)
import Utils exposing (isNoun)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseCards t ->
            ( { model | picked = NoCard }, Cmd.none )

        FlipCard c ->
            case model.picked of
                NoCard ->
                    ( { model | picked = PickedOne c }, Cmd.none )

                PickedOne c1 ->
                    if isMatched c c1 then
                        ( { model
                            | picked = NoCard
                            , cards = flipCards c c1 model.cards
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | picked = PickedTwo c c1 }, Cmd.none )

                PickedTwo c1 c2 ->
                    ( model, Cmd.none )

        NewBoard board ->
            ( { model
                | cards = board
              }
            , Cmd.none
            )

        -- Restart ->
        --     init
        SelectPartOfSpeech filter ->
            case filter of
                All ->
                    ( { model | selectedPartOfSpeech = filter }, Cmd.none )

                Nouns ->
                    ( { model
                        | selectedPartOfSpeech = filter
                        , cards = prepareCards (List.filter isNoun model.allWords)
                      }
                    , Cmd.none
                    )

                Verbs ->
                    ( { model
                        | selectedPartOfSpeech = filter
                        , cards = prepareCards (List.filter (not << isNoun) model.allWords)
                      }
                    , Cmd.none
                    )

        SelectRoot root ->
            ( { model
                | selectedRoot =
                    case root of
                        "" ->
                            Nothing

                        _ ->
                            Just root

                -- | cards =
                --     model.allWords
                --         |> List.filter (\word -> word.root == root)
                --         |> prepareCards
              }
            , Cmd.none
            )

        Shuffle ->
            ( model, shuffleCards model.cards )

        UrlChange location ->
            ( { model | history = location }, Cmd.none )


isMatched : Card -> Card -> Bool
isMatched card1 card2 =
    card1.wordPair.german == card2.wordPair.german


flipCards : Card -> Card -> List Card -> List Card
flipCards firstPick secondPick cards =
    let
        matchCard c =
            if isMatched c firstPick || isMatched c secondPick then
                { c | matched = True }
            else
                c
    in
        List.map matchCard cards



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.picked of
        NoCard ->
            Sub.none

        PickedOne _ ->
            Sub.none

        PickedTwo _ _ ->
            Time.every (1000 * millisecond) CloseCards



-- MAIN


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
