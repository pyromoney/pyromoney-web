module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, text)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode



---- MODEL ----


type Msg
    = RequestAccounts
    | ReceiveAccounts (Result Http.Error (List Account))


type alias ServerUrl =
    String


type alias Account =
    { id : String
    , parentId : Maybe String
    , name : String
    , type_ : String
    , currency : String
    , hidden : Bool
    , virtual : Bool
    }


type alias Model =
    { serverUrl : ServerUrl
    , accounts : List Account
    , lastError : String
    }


type alias Flags =
    { serverUrl : ServerUrl
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { serverUrl = flags.serverUrl
      , accounts = []
      , lastError = ""
      }
    , fetchAccounts flags.serverUrl
    )


decodeAccounts : Decode.Decoder (List Account)
decodeAccounts =
    Decode.field "data" (Decode.list decodeAccount)


decodeAccount : Decode.Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "id" Decode.string
        |> required "parent_id" (Decode.nullable Decode.string)
        |> required "name" Decode.string
        |> required "type" Decode.string
        |> required "currency" Decode.string
        |> required "hidden" Decode.bool
        |> required "virtual" Decode.bool


fetchAccounts : ServerUrl -> Cmd Msg
fetchAccounts serverUrl =
    Http.get
        { url = serverUrl ++ "/accounts"
        , expect = Http.expectJson ReceiveAccounts decodeAccounts
        }



---- UPDATE ----


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestAccounts ->
            ( model, fetchAccounts model.serverUrl )

        ReceiveAccounts (Ok accounts) ->
            ( { model | accounts = accounts }, Cmd.none )

        ReceiveAccounts (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        ([ text model.lastError ] ++ List.map viewAccount model.accounts)


viewAccount : Account -> Html Msg
viewAccount account =
    div [] [ text account.name ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
