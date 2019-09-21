module Loadable exposing (Loadable(..))


type Loadable loaded loading
    = Loaded loaded
    | Failure String
    | Loading loading
