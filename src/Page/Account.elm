module Page.Account exposing (Model, Msg(..), init, update, view)

import Data.Account exposing (Account)
import Data.Transaction exposing (LedgerEntry, decodeLedgerEntry)
import DateFormat
import Dict exposing (Dict)
import Element exposing (..)
import Http
import Json.Decode as Decode
import Time
import Utils


type Msg
    = RequestLedgerEntries Account
    | ReceiveLedgerEntries (Result Http.Error (List LedgerEntry))


type alias Model =
    { account : Account
    , ledgerEntries : List LedgerEntry
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
      , ledgerEntries = []
      , lastError = ""
      , timezone = Time.utc
      }
    , fetchLedgerEntries serverUrl accountsDict account
    )


fetchLedgerEntries : ServerUrl -> Dict AccountId Account -> Account -> Cmd Msg
fetchLedgerEntries serverUrl accountsDict account =
    let
        decoder =
            Decode.field "data" (Decode.list (decodeLedgerEntry accountsDict account))
    in
    Http.get
        { url = serverUrl ++ "/accounts/" ++ account.id ++ "/transactions"
        , expect = Http.expectJson ReceiveLedgerEntries decoder
        }


update : AppState a -> Msg -> Model -> ( Model, Cmd Msg )
update { serverUrl, accountsDict } msg model =
    case msg of
        RequestLedgerEntries account ->
            ( model, fetchLedgerEntries serverUrl accountsDict account )

        ReceiveLedgerEntries (Ok ledgerEntries) ->
            ( { model | ledgerEntries = ledgerEntries }, Cmd.none )

        ReceiveLedgerEntries (Err error) ->
            ( { model | lastError = Utils.httpErrorString error }, Cmd.none )


view : AppState a -> Model -> Element Msg
view appState model =
    column [ width fill ]
        [ el [] <| text model.lastError
        , el [] <| text model.account.name
        , viewLedgerEntries appState model
        ]


viewLedgerEntries : AppState a -> Model -> Element Msg
viewLedgerEntries appState { ledgerEntries, timezone } =
    table
        [ width fill ]
        { data = ledgerEntries
        , columns =
            [ { header = text "Date"
              , width = fillPortion 10
              , view =
                    \ledgerEntry ->
                        viewTimestamp timezone ledgerEntry
              }
            , { header = text "Description"
              , width = fillPortion 40
              , view =
                    \ledgerEntry ->
                        text ledgerEntry.transaction.description
              }
            , { header = text "Transfer"
              , width = fillPortion 20
              , view =
                    \ledgerEntry ->
                        case ledgerEntry.otherSplits of
                            [ split ] ->
                                text split.account.name

                            _ ->
                                text "Split transaction"
              }
            , { header = text "Own split"
              , width = fillPortion 10
              , view =
                    \ledgerEntry ->
                        text <| String.fromFloat <| ledgerEntry.split.amount
              }
            , { header = text "Other split"
              , width = fillPortion 10
              , view =
                    \ledgerEntry ->
                        case ledgerEntry.otherSplits of
                            [ split ] ->
                                text <| String.fromFloat <| split.amount

                            _ ->
                                text "Split transaction"
              }
            , { header = text "Balance"
              , width = fillPortion 10
              , view =
                    \_ ->
                        text "TBD"
              }
            ]
        }


viewTimestamp : Time.Zone -> LedgerEntry -> Element Msg
viewTimestamp timezone ledgerEntry =
    text (formatTimestamp timezone ledgerEntry.transaction.timestamp)


formatTimestamp : Time.Zone -> Time.Posix -> String
formatTimestamp =
    DateFormat.format
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text "."
        , DateFormat.monthFixed
        , DateFormat.text "."
        , DateFormat.yearNumber
        ]
