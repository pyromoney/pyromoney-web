module Loadable exposing (Loadable(..), map)


type Loadable loaded loading
    = Loaded loaded
    | Failure String
    | Loading loading


map : (a -> b) -> Loadable a loading -> Loadable b loading
map f xs =
    case xs of
        Loading loading ->
            Loading loading

        Loaded loaded ->
            Loaded <| f loaded

        Failure err ->
            Failure err
