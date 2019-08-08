module Page.Account exposing (Model, Msg(..), init, view)

import Data.Account exposing (Account)
import Element exposing (..)


type Msg
    = NoOp


type alias Model =
    { account : Account
    }


init : Account -> ( Model, Cmd Msg )
init account =
    ( { account = account }, Cmd.none )


view : Model -> Element Msg
view model =
    el [] <| text model.account.name
