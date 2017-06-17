module Utils exposing (..)

import Regex exposing (contains, regex)
import Model.Model exposing (WordPair)


isNoun : WordPair -> Bool
isNoun pair =
    contains (regex "^(der|die|das)") pair.german
