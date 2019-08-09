module Page.Account exposing (Model, Msg(..), init, update, view)

import Data.Account exposing (Account)
import Data.Transaction exposing (Transaction, decodeTransaction)
import DateFormat
import Dict exposing (Dict)
import Element exposing (..)
import Http
import Json.Decode as Decode
import Time
import Utils


type Msg
    = RequestTransactions Account
    | ReceiveTransactions (Result Http.Error (List Transaction))


type alias Model =
    { account : Account
    , transactions : List Transaction
    , lastError : String
    , timezone : Time.Zone
    }


type alias AppState a =
    { a | serverUrl : ServerUrl, accountsDict : Dict AccountId Account }


type alias ServerUrl =
    String


type alias AccountId =
    String


init : AppState a -> Account -> ( Model, Cmd Msg )
init { serverUrl, accountsDict } account =
    ( { account = account
      , transactions = []
      , lastError = ""
      , timezone = Time.utc
      }
    , fetchTransactions serverUrl accountsDict account
    )


fetchTransactions : ServerUrl -> Dict AccountId Account -> Account -> Cmd Msg
fetchTransactions serverUrl accountsDict account =
    let
        decoder =
            Decode.field "data" (Decode.list (decodeTransaction accountsDict))
    in
    Http.get
        { url = serverUrl ++ "/accounts/" ++ account.id ++ "/transactions"
        , expect = Http.expectJson ReceiveTransactions decoder
        }


update : AppState a -> Msg -> Model -> ( Model, Cmd Msg )
update { serverUrl, accountsDict } msg model =
    case msg of
        RequestTransactions account ->
            ( model, fetchTransactions serverUrl accountsDict account )

        ReceiveTransactions (Ok transactions) ->
            ( { model | transactions = transactions }, Cmd.none )

        ReceiveTransactions (Err error) ->
            ( { model | lastError = Utils.httpErrorString error }, Cmd.none )


view : AppState a -> Model -> Element Msg
view appState model =
    column [ width fill ]
        [ el [] <| text model.lastError
        , el [] <| text model.account.name
        , viewTransactions appState model
        ]


viewTransactions : AppState a -> Model -> Element Msg
viewTransactions appState { transactions, timezone } =
    table
        [ width fill ]
        { data = transactions
        , columns =
            [ { header = text "Date"
              , width = fillPortion 10
              , view =
                    \transaction ->
                        viewTimestamp timezone transaction
              }
            , { header = text "Description"
              , width = fillPortion 40
              , view =
                    \transaction ->
                        text transaction.description
              }
            , { header = text "Transfer"
              , width = fillPortion 20
              , view =
                    \_ ->
                        text "TBD"
              }
            , { header = text "Own split"
              , width = fillPortion 10
              , view =
                    \_ ->
                        text "TBD"
              }
            , { header = text "Other split"
              , width = fillPortion 10
              , view =
                    \_ ->
                        text "TBD"
              }
            , { header = text "Balance"
              , width = fillPortion 10
              , view =
                    \_ ->
                        text "TBD"
              }
            ]
        }


viewTimestamp : Time.Zone -> Transaction -> Element Msg
viewTimestamp timezone transaction =
    text (formatTimestamp timezone transaction.timestamp)


formatTimestamp : Time.Zone -> Time.Posix -> String
formatTimestamp =
    DateFormat.format
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text "."
        , DateFormat.monthFixed
        , DateFormat.text "."
        , DateFormat.yearNumber
        ]
