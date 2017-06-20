module Model.Model exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Random
import Random.List exposing (shuffle)
import Set
import Time exposing (Time, millisecond)
import Data.Roots as Roots
import Data.Worter as Worter
import Navigation


-- MESSAGES


type Msg
    = CloseCards Time
    | FlipCard Card
    | UrlChange Navigation.Location
    | NewBoard (List Card)
      -- | Restart
    | SelectRoot String
    | SelectPartOfSpeech FilterOptions
    | Shuffle



-- MODEL


type alias Description =
    { root : String
    , description : String
    }


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
    { allWords : List WordPair
    , cards : List Card
    , descriptions : List Description
    , history : Navigation.Location
    , picked : CardPicked
    , roots : List String
    , selectedPartOfSpeech : FilterOptions
    , selectedRoot : Maybe String
    }


type FilterOptions
    = All
    | Verbs
    | Nouns


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


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        allWords =
            decodeWordpairs Worter.json

        cards =
            prepareCards allWords

        roots =
            allWords
                |> List.foldl (\word -> Set.insert word.root) Set.empty
                |> Set.toList
    in
        ( { allWords = allWords
          , cards = cards
          , descriptions = decodeDescriptions Roots.json
          , history = location
          , picked = NoCard
          , roots = roots
          , selectedRoot = Nothing
          , selectedPartOfSpeech = All
          }
        , shuffleCards cards
        )



-- Decoders


wordPairDecoder : Decoder WordPair
wordPairDecoder =
    decode WordPair
        |> required "root" Json.Decode.string
        |> required "german" Json.Decode.string
        |> required "english" Json.Decode.string


descriptionDecoder : Decoder Description
descriptionDecoder =
    decode Description
        |> required "root" Json.Decode.string
        |> required "description" Json.Decode.string


decodeDescriptions : String -> List Description
decodeDescriptions json =
    case decodeString (list descriptionDecoder) json of
        Ok descriptions ->
            descriptions

        Err _ ->
            []


decodeWordpairs : String -> List WordPair
decodeWordpairs json =
    case decodeString (list wordPairDecoder) json of
        Ok words ->
            words

        Err _ ->
            []
