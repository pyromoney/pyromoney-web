module Page.Account exposing (Model, Msg(..), init, update, view)

import Data.Account exposing (Account)
import Data.Transaction as Transaction exposing (LedgerEntry, TransactionId, decodeLedgerEntry)
import DateFormat
import Dict exposing (Dict)
import Editable exposing (Editable(..))
import Element exposing (Element, column, el, fill, fillPortion, row, text, width)
import Element.Events exposing (onClick)
import Element.Input as Input exposing (labelHidden)
import FormValue exposing (FormValue(..))
import Http
import Json.Decode as Decode
import List.Extra as LE
import Time
import Utils


type Msg
    = RequestLedgerEntries Account
    | ReceiveLedgerEntries (Result Http.Error (List LedgerEntry))
    | EditLedgerEntry TransactionId
      -- TODO: Use lens?
    | ChangeLedgerEntryDescription TransactionId String



-- | ChangeLedgerEntrySplitAmount TransactionId Float


type alias Model =
    { account : Account
    , ledgerEntries : List (Editable LedgerEntryForm)
    , lastError : String
    , timezone : Time.Zone
    }


type alias Config a =
    { a | serverUrl : ServerUrl }

type alias LedgerEntryForm =
    { timestamp : Time.Posix
    , transactionId : TransactionId
    , description : FormValue String
    , splitAmount : FormValue Float
    , otherSplitAmounts : List (FormValue Float)
    }


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
editLedgerEntry : TransactionId -> Model -> Model
editLedgerEntry transactionId model =
    { model
        | ledgerEntries =
            model.ledgerEntries
                |> LE.updateIf (eqTransactionId transactionId)
                    Editable.edit
    }


setLedgerEntryDescription : TransactionId -> String -> Model -> Model
setLedgerEntryDescription transactionId newDescription model =
    { model
        | ledgerEntries =
            model.ledgerEntries
                |> LE.updateIf (eqTransactionId transactionId)
                    (Editable.mapTarget
                        (\entry ->
                            { entry | description = FormValue.fromString newDescription }
                        )
                    )
    }


eqTransactionId : TransactionId -> Editable LedgerEntryForm -> Bool
eqTransactionId id =
    (==) id << .transactionId << Editable.target


toForm : LedgerEntry -> LedgerEntryForm
toForm { transaction, split, otherSplits } =
    { timestamp = transaction.timestamp
    , transactionId = transaction.id
    , description = transaction.description |> FormValue.fromString
    , splitAmount = split.amount |> FormValue.fromFloat
    , otherSplitAmounts = otherSplits |> List.map (FormValue.fromFloat << .amount)
    }


update : AppState a -> Msg -> Model -> ( Model, Cmd Msg )
update { serverUrl, accountsDict } msg model =
    case msg of
        RequestLedgerEntries account ->
            ( model
            , fetchLedgerEntries serverUrl accountsDict account
            )

        ReceiveLedgerEntries (Ok ledgerEntries) ->
            ( { model
                | ledgerEntries =
                    ledgerEntries
                        |> List.map (Editable.fromSaved << toForm)
              }
            , Cmd.none
            )

        ReceiveLedgerEntries (Err error) ->
            ( { model | lastError = Utils.httpErrorString error }
            , Cmd.none
            )

        EditLedgerEntry transactionId ->
            ( model |> editLedgerEntry transactionId
            , Cmd.none
            )

        ChangeLedgerEntryDescription transactionId newDescription ->
            ( model |> setLedgerEntryDescription transactionId newDescription
            , Cmd.none
            )


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

        -- , text "Transfer"
        , text "Own split"
        , text "Other split"
        , text "Balance"
        ]
        :: List.map (viewLedgerEntry appState model) model.ledgerEntries


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


viewLedgerEntry : AppState a -> Model -> Editable LedgerEntryForm -> Element Msg
viewLedgerEntry appState { timezone } (Editable state ledgerEntry) =
    let
        id =
            ledgerEntry.transactionId
    in
    case state of
        Editable.Saved ->
            [ viewTimestamp timezone ledgerEntry.timestamp
            , text (ledgerEntry.description |> FormValue.toString)
            , viewSplitAmount ledgerEntry.splitAmount
            , case ledgerEntry.otherSplitAmounts of
                [ split ] ->
                    viewSplitAmount split

                _ ->
                    text "Split transaction"
            , text (ledgerEntry.splitAmount |> FormValue.toString)
            ]
                |> List.map (makeEditable <| EditLedgerEntry id)
                |> ledgerRow

        Editable.New ->
            Debug.todo "New entry"

        Editable.Editing modifiedEntry ->
            [ viewTimestamp timezone ledgerEntry.timestamp
            , textEdit modifiedEntry.description (ChangeLedgerEntryDescription id)
            , text "TODO"

            -- , textEdit String.fromFloat modifiedEntry.split.amount ChangeLedgerEntrySplitAmount
            , text "TODO"
            , text "TODO"
            ]
                |> ledgerRow


textEdit : FormValue a -> (String -> Msg) -> Element Msg
textEdit value msg =
    Input.text [ width fill ]
        { onChange = msg
        , text = FormValue.toString value
        , placeholder = Nothing
        , label = labelHidden ""
        }


viewSplitAmount : FormValue Float -> Element Msg
viewSplitAmount splitAmount =
    splitAmount
        |> FormValue.toFloat
        |> Maybe.andThen
            (\amount ->
                if amount > 0 then
                    Just <| String.fromFloat amount

                else
                    Nothing
            )
        |> Maybe.withDefault ""
        |> text


makeEditable : Msg -> Element Msg -> Element Msg
makeEditable msg content =
    el [ onClick msg ] content


viewTimestamp : Time.Zone -> Time.Posix -> Element Msg
viewTimestamp timezone timestamp =
    text (formatTimestamp timezone timestamp)


formatTimestamp : Time.Zone -> Time.Posix -> String
formatTimestamp =
    DateFormat.format
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text "."
        , DateFormat.monthFixed
        , DateFormat.text "."
        , DateFormat.yearNumber
        ]
