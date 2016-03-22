import Hangman exposing (init, update, view)
import StartApp.Simple exposing (start)
import Set


main =
  start
    { model = init
    , update = update
    , view = view
    }
