module Page.Account exposing (Model, Msg(..), init, update, view)

import Data.Account exposing (Account, AccountId)
import Data.Transaction as Transaction exposing (LedgerEntry, TransactionId, decodeLedgerEntry)
import DateFormat
import Dict exposing (Dict)
import Editable exposing (Editable(..))
import Element exposing (Element, column, el, fill, fillPortion, html, row, text, width)
import Element.Events exposing (onClick)
import Element.Input as Input exposing (labelHidden)
import FormValue exposing (FormValue(..))
import Http
import Json.Decode as Decode
import List.Extra as LE
import Time
import Tree
import Utils


type Msg
    = RequestLedgerEntries Account
    | ReceiveLedgerEntries (Result Http.Error (List LedgerEntry))
    | EditLedgerEntry TransactionId
      -- TODO: Use lens?
    | ChangeLedgerEntryDescription TransactionId String
    | ChangeLedgerEntryOtherSplitAmount TransactionId String
    | ChangeLedgerEntryOwnSplitAmount TransactionId String



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
    , ownSplitAmount : FormValue Float
    , otherSplit : OtherSplit
    }


type OtherSplit
    = SingleSplit
        { amount : FormValue Float
        }
    | MultipleSplits


type alias AppState a =
    { a | accountsDict : Dict AccountId Account }


type alias ServerUrl =
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


eqTransactionId : TransactionId -> Editable LedgerEntryForm -> Bool
eqTransactionId id =
    (==) id << .transactionId << Editable.target


updateLedgerEntry : TransactionId -> (Editable LedgerEntryForm -> Editable LedgerEntryForm) -> Model -> Model
updateLedgerEntry transactionId f model =
    { model
        | ledgerEntries =
            model.ledgerEntries
                |> LE.updateIf (eqTransactionId transactionId) f
    }


editLedgerEntry : TransactionId -> Model -> Model
editLedgerEntry transactionId =
    updateLedgerEntry transactionId Editable.edit


setLedgerEntryDescription : TransactionId -> String -> Model -> Model
setLedgerEntryDescription transactionId newDescription =
    updateLedgerEntry transactionId <|
        Editable.mapTarget
            (\entry ->
                { entry | description = FormValue.fromString newDescription }
            )


setLedgerEntryOwnSplitAmount : TransactionId -> String -> Model -> Model
setLedgerEntryOwnSplitAmount transactionId newAmount =
    updateLedgerEntry transactionId <|
        Editable.mapTarget
            (\entry ->
                { entry | ownSplitAmount = FormValue.parseFloat newAmount }
            )


setLedgerEntryOtherSplitAmount : TransactionId -> String -> Model -> Model
setLedgerEntryOtherSplitAmount transactionId newAmount =
    updateLedgerEntry transactionId <|
        Editable.mapTarget
            (\({ otherSplit } as entry) ->
                case otherSplit of
                    SingleSplit _ ->
                        { entry | otherSplit = SingleSplit { amount = FormValue.parseFloat newAmount } }

                    MultipleSplits ->
                        entry
            )


toForm : LedgerEntry -> LedgerEntryForm
toForm { transaction, split, otherSplits } =
    { timestamp = transaction.timestamp
    , transactionId = transaction.id
    , description = transaction.description |> FormValue.fromString
    , ownSplitAmount = split.amount |> FormValue.fromFloat
    , otherSplit =
        case otherSplits of
            [ otherSplit ] ->
                SingleSplit
                    { amount = otherSplit.amount |> FormValue.fromFloat
                    }

            _ ->
                MultipleSplits
    }


update : Config a -> AppState b -> Msg -> Model -> ( Model, Cmd Msg )
update { serverUrl } { accountsDict } msg model =
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

        ChangeLedgerEntryOwnSplitAmount transactionId newAmount ->
            ( model |> setLedgerEntryOwnSplitAmount transactionId newAmount
            , Cmd.none
            )

        ChangeLedgerEntryOtherSplitAmount transactionId newAmount ->
            ( model |> setLedgerEntryOtherSplitAmount transactionId newAmount
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
            , viewSplitAmount ledgerEntry.ownSplitAmount
            , case ledgerEntry.otherSplit of
                SingleSplit { amount } ->
                    viewSplitAmount amount

                MultipleSplits ->
                    text "Split transaction"
            ]
                |> List.map (makeEditable <| EditLedgerEntry id)
                |> ledgerRow

        Editable.New ->
            Debug.todo "New entry"

        Editable.Editing modifiedEntry ->
            [ viewTimestamp timezone ledgerEntry.timestamp
            , textEdit modifiedEntry.description (ChangeLedgerEntryDescription id)
            , case modifiedEntry.otherSplit of
                SingleSplit { amount } ->
                    textEdit amount <| ChangeLedgerEntryOtherSplitAmount id

                MultipleSplits ->
                    text "Split transaction"
            , case modifiedEntry.otherSplit of
                SingleSplit _ ->
                    textEdit modifiedEntry.ownSplitAmount <| ChangeLedgerEntryOwnSplitAmount id

                MultipleSplits ->
                    text "Split transaction"
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
viewSplitAmount ownSplitAmount =
    ownSplitAmount
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
