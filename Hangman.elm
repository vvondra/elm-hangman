module Hangman where

import Char
import Set
import String
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

type alias Game =
  {
    word : String,
    chars : Set.Set Char,
    guesses : Int,
    maxGuesses: Int
  }

-- MODEL

init: Game
init = {
    word =  "hangman",
    chars = Set.empty,
    guesses = 0,
    maxGuesses = 5
    }

hiddenWord: Game -> String
hiddenWord game =
    String.map (\c -> if Set.member c game.chars then c else '_' ) game.word

badLetters: Game -> Set.Set Char
badLetters game =
    Set.filter (\c -> not (String.contains (String.fromChar c) game.word)) game.chars

letters = List.map (\code -> Char.fromCode code) [97..122]

-- VIEW

view: Signal.Address Action -> Game -> Html.Html
view address model =
  div [gameContainerStyle]
    (
        [ div [bigStyle] [ text (hiddenWord model) ] ] ++
        [ viewGuessRow address model ] ++
        [ div [] [
            text "Not in the word: ",
            text (String.join ", " (Set.toList (Set.map String.fromChar (badLetters model))) )
        ] ] ++
        [ div [] [
           text "Attempts left: ",
           text (toString (model.maxGuesses - model.guesses))
        ] ] ++
        [ div []
          [ button [buttonStyle, onClick address Reset] [ text "reset" ] ]
        ]
    )

viewGuessRow: Signal.Address Action -> Game -> Html.Html
viewGuessRow address model = div [] [
        if String.all (\c -> Set.member c model.chars) model.word then
            text "winner!!"
        else if model.guesses < model.maxGuesses then
            (div [] (viewLetterButtons address model.chars))
        else
            text "loser"
    ]

viewLetters: Set.Set Char -> List Html.Html
viewLetters chars =
  List.map
    (\letter -> span [] [ text (String.fromChar letter) ])
    (Set.toList chars)

viewLetterButtons: Signal.Address Action -> Set.Set Char -> List Html.Html
viewLetterButtons address guessedChars =
    List.map (
        \letter ->
            button [
                buttonStyle,
                disabled (Set.member letter guessedChars),
                onClick address (Guess letter)
            ] [ text (String.fromChar letter) ]
    )
    letters

-- VIEW STYLE

bigStyle : Attribute
bigStyle =
  style
    [ ("font-size", "30px")
    , ("font-weight", "bold")
    ]

gameContainerStyle : Attribute
gameContainerStyle =
  style
    [ ("margin", "20px")
    , ("font-weight", "bold")
    ]

buttonStyle : Attribute
buttonStyle =
  style
    [ ("font-size", "20px")
    ]

-- UPDATE

type Action = Guess Char | Reset

update action model =
    case action of
        Guess char -> { model |
            chars = Set.insert (Char.toLower char) model.chars,
            guesses = if String.contains (String.fromChar char) model.word then model.guesses else model.guesses + 1
        }
        Reset -> init

