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


type alias Config a =
    { a | serverUrl : ServerUrl }


type alias AppState a =
    { a | accountsDict : Dict AccountId Account }


type alias ServerUrl =
    String


type alias AccountId =
    String


init : Config a -> AppState b -> Account -> ( Model, Cmd Msg )
init { serverUrl } { accountsDict } account =
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


update : Config a -> AppState b -> Msg -> Model -> ( Model, Cmd Msg )
update { serverUrl } { accountsDict } msg model =
    case msg of
        RequestLedgerEntries account ->
            ( model, fetchLedgerEntries serverUrl accountsDict account )

        ReceiveLedgerEntries (Ok ledgerEntries) ->
            ( { model | ledgerEntries = ledgerEntries }, Cmd.none )

        ReceiveLedgerEntries (Err error) ->
            ( { model | lastError = Utils.httpErrorString error }, Cmd.none )


view : AppState a -> Model -> Element Msg
view appState model =
    column [ width fill ] <|
        [ el [] <| text model.lastError
        , el [] <| text model.account.name
        ]
            ++ viewLedgerEntries appState model


viewLedgerEntries : AppState a -> Model -> List (Element Msg)
viewLedgerEntries appState model =
    ledgerRow
        [ text "Date"
        , text "Description"
        , text "Transfer"
        , text "Own split"
        , text "Other split"
        , text "Balance"
        ]
        :: List.map (viewLedgerEntry appState model) model.ledgerEntries


viewLedgerEntry : AppState a -> Model -> LedgerEntry -> Element Msg
viewLedgerEntry appState { timezone } ledgerEntry =
ledgerRow : List (Element Msg) -> Element Msg
ledgerRow cols =
    let
        widths =
            [ 10, 40, 20, 10, 10, 10 ]

        col w content =
            column [ width (fillPortion w) ] [ content ]
    in
    row [ width fill ] <|
        List.map2 col widths cols


    ledgerRow
        [ viewTimestamp timezone ledgerEntry
        , text ledgerEntry.transaction.description
        , case ledgerEntry.otherSplits of
            [ split ] ->
                text split.account.name

            _ ->
                text "Split transaction"
        , if ledgerEntry.split.amount > 0 then
            text <| String.fromFloat <| ledgerEntry.split.amount

          else
            text ""
        , case ledgerEntry.otherSplits of
            [ split ] ->
                if split.amount > 0 then
                    text <| String.fromFloat <| split.amount

                else
                    text ""

            _ ->
                text "Split transaction"
        , text "TBD"
        ]


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
