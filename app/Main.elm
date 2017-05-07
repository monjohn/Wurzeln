module Main exposing (..)

import App.Worter exposing (json)
import Debug exposing (log)
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (class, ismap)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import List exposing (filter, length, map, range)
import Random
import Random.List exposing (shuffle)
import Time exposing (Time, millisecond)


wordPairDecoder : Decoder WordPair
wordPairDecoder =
    decode WordPair
        |> required "root" Json.Decode.string
        |> required "german" Json.Decode.string
        |> required "english" Json.Decode.string


decodeResults : String -> List WordPair
decodeResults json =
    case decodeString (list wordPairDecoder) json of
        Ok words ->
            words

        Err _ ->
            []



-- MODEL


type alias WordPair =
    { root : String
    , german : String
    , english : String
    }


type alias Card =
    { face : String
    , wordPair : WordPair
    , matched : Bool
    }


type CardPicked
    = NoCard
    | PickedOne Card
    | PickedTwo Card Card


type alias Model =
    { cards : List Card
    , picked : CardPicked
    , allWords : List WordPair
    }


prepareCards : List WordPair -> List Card
prepareCards words =
    let
        wordToCards wordPair =
            [ Card wordPair.english wordPair False
            , Card wordPair.german wordPair False
            ]
    in
        List.concatMap wordToCards words


shuffleCards : List Card -> Cmd Msg
shuffleCards cards =
    Random.generate NewBoard (shuffle cards)


init : ( Model, Cmd Msg )
init =
    let
        allWords =
            decodeResults json

        cards =
            prepareCards allWords
    in
        ( { picked = NoCard
          , allWords = allWords
          , cards = cards
          }
        , shuffleCards cards
        )


isMatched : Card -> Card -> Bool
isMatched card1 card2 =
    card1.wordPair.german == card2.wordPair.german



-- UPDATE


type Msg
    = FlipCard Card
    | CloseCards Time
    | NewBoard (List Card)
    | Restart
    | Shuffle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        CloseCards t ->
            ( { model | picked = NoCard }, Cmd.none )

        NewBoard board ->
            ( { model
                | cards = board
              }
            , Cmd.none
            )

        Shuffle ->
            ( model, shuffleCards model.cards )

        Restart ->
            init


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



-- VIEW


sameCard : Card -> Card -> Bool
sameCard card1 card2 =
    card1.face == card2.face


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
            [ div [ class "title" ] [ text "The Memory Game" ]
            , div [ class "caption" ] [ text "For the ultimate brain's pleasure... " ]
            , div [ class "container" ]
                (List.map (\c -> card (isFlipped c) c) model.cards)
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



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
