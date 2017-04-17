module Main exposing (..)

import App.Worter exposing (json)
import Debug exposing (log)
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (class)
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
    , card : WordPair
    , matched : Bool
    }



-- type CardPicked
--   = NoCard
--   | PickedOne Card
--   | PickedTwo Card Card


type alias Model =
    { cards : List Card
    , firstPick : Maybe Card
    , secondPick : Maybe Card
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
        ( { firstPick = Nothing
          , secondPick = Nothing
          , allWords = allWords
          , cards = cards
          }
        , shuffleCards cards
        )



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
            let
                firstPick =
                    getFirstPick model c

                secondPick =
                    getSecondPick model c

                isMatch =
                    (Maybe.map (\pick -> pick.card.german) firstPick) == (Maybe.map (\pick -> pick.card.german) secondPick)

                cards =
                    if isMatch then
                        flipCards firstPick secondPick model.cards
                    else
                        model.cards
            in
                ( { model
                    | cards = cards
                    , firstPick =
                        if isMatch then
                            Nothing
                        else
                            firstPick
                    , secondPick =
                        if isMatch then
                            Nothing
                        else
                            secondPick
                  }
                , Cmd.none
                )

        CloseCards t ->
            ( { model
                | firstPick = Nothing
                , secondPick = Nothing
              }
            , Cmd.none
            )

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


getFirstPick : Model -> Card -> Maybe Card
getFirstPick { firstPick, secondPick } card =
    if (firstPick == Nothing) then
        Just card
    else
        firstPick


getSecondPick : Model -> Card -> Maybe Card
getSecondPick { firstPick, secondPick } card =
    if (firstPick /= Nothing && secondPick == Nothing && Just card /= firstPick) then
        Just card
    else
        secondPick


flipCards : Maybe Card -> Maybe Card -> List Card -> List Card
flipCards firstPick secondPick cards =
    let
        getFace card =
            card.face

        matchCard card =
            if Just card.face == (Maybe.map getFace firstPick) || Just card.face == (Maybe.map getFace secondPick) then
                { card | matched = True }
            else
                card
    in
        List.map matchCard cards



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.firstPick == Nothing || model.secondPick == Nothing then
        Sub.none
    else
        Time.every (500 * millisecond) CloseCards



-- VIEW


view : Model -> Html Msg
view model =
    let
        isFlipped card =
            model.firstPick == (Just card) || model.secondPick == (Just card) || card.matched

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
