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
import UI exposing (accountSelect)
import Utils


type Msg
    = RequestLedgerEntries Account
    | ReceiveLedgerEntries (Result Http.Error (List LedgerEntry))
    | EditLedgerEntry TransactionId
      -- TODO: Use lens?
    | ChangeLedgerEntryDescription TransactionId String
    | ChangeLedgerEntryOtherSplitAmount TransactionId String
    | ChangeLedgerEntryOwnSplitAmount TransactionId String
    | ChangeLedgerEntryAccount TransactionId AccountId



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
        , accountId : AccountId
        }
    | MultipleSplits


type alias AppState a =
    { a
        | accountsDict : Dict AccountId Account
        , accountsTree : Tree.Multitree Account
    }


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
editLedgerEntry transactionId model =
    { model
        | ledgerEntries =
            model.ledgerEntries
                |> List.map Editable.cancel
    }
        |> updateLedgerEntry transactionId Editable.edit


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
            (\entry ->
                case entry.otherSplit of
                    SingleSplit otherSplit ->
                        { entry | otherSplit = SingleSplit { otherSplit | amount = FormValue.parseFloat newAmount } }

                    MultipleSplits ->
                        entry
            )


setLedgerEntryAccount : TransactionId -> AccountId -> Model -> Model
setLedgerEntryAccount transactionId accountId =
    updateLedgerEntry transactionId <|
        Editable.mapTarget
            (\entry ->
                case entry.otherSplit of
                    SingleSplit otherSplit ->
                        { entry | otherSplit = SingleSplit { otherSplit | accountId = accountId } }

                    MultipleSplits ->
                        entry
            )


ensureNewLedgerEntry : Model -> Model
ensureNewLedgerEntry model =
    let
        ( newEntries, rest ) =
            model.ledgerEntries
                |> List.partition Editable.isNew

        newEntry =
            newEntries
                |> List.head
                |> Maybe.withDefault (Editable.fromNew makeLedgerEntry)
    in
    { model
        | ledgerEntries = rest ++ [ newEntry ]
    }


makeLedgerEntry : LedgerEntryForm
makeLedgerEntry =
    { timestamp = Time.millisToPosix 0
    , transactionId = ""
    , description = FormValue.fromString ""
    , ownSplitAmount = FormValue.parseFloat ""
    , otherSplit =
        SingleSplit
            { amount = FormValue.parseFloat ""
            , accountId = ""
            }
    }


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
                    { accountId = otherSplit.account.id
                    , amount = otherSplit.amount |> FormValue.fromFloat
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
                |> ensureNewLedgerEntry
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

        ChangeLedgerEntryAccount transactionId accountId ->
            ( model |> setLedgerEntryAccount transactionId accountId, Cmd.none )


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
viewLedgerEntry { accountsTree, accountsDict } { timezone } (Editable state ledgerEntry) =
    case state of
        Editable.Saved ->
            [ viewTimestamp timezone ledgerEntry.timestamp
            , text (ledgerEntry.description |> FormValue.toString)
            , case ledgerEntry.otherSplit of
                SingleSplit { accountId } ->
                    accountsDict
                        |> Dict.get accountId
                        |> Maybe.map (text << .name)
                        |> Maybe.withDefault (text "")

                MultipleSplits ->
                    text "Split transaction"
            , viewSplitAmount ledgerEntry.ownSplitAmount
            , case ledgerEntry.otherSplit of
                SingleSplit { amount } ->
                    viewSplitAmount amount

                MultipleSplits ->
                    text "Split transaction"
            ]
                |> List.map (makeEditable <| EditLedgerEntry ledgerEntry.transactionId)
                |> ledgerRow

        Editable.New ->
            viewEntryEditor accountsTree timezone ledgerEntry

        Editable.Editing modifiedEntry ->
            viewEntryEditor accountsTree timezone modifiedEntry


viewEntryEditor : Tree.Multitree Account -> Time.Zone -> LedgerEntryForm -> Element Msg
viewEntryEditor accountsTree timezone ledgerEntry =
    [ viewTimestamp timezone ledgerEntry.timestamp
    , textEdit ledgerEntry.description (ChangeLedgerEntryDescription ledgerEntry.transactionId)
    , case ledgerEntry.otherSplit of
        SingleSplit _ ->
            -- TODO: Take accountId from SingleSplit
            accountSelect accountsTree Nothing <| ChangeLedgerEntryAccount ledgerEntry.transactionId

        MultipleSplits ->
            text "Split transaction"
    , case ledgerEntry.otherSplit of
        SingleSplit { amount } ->
            textEdit amount <| ChangeLedgerEntryOtherSplitAmount ledgerEntry.transactionId

        MultipleSplits ->
            text "Split transaction"
    , case ledgerEntry.otherSplit of
        SingleSplit _ ->
            textEdit ledgerEntry.ownSplitAmount <| ChangeLedgerEntryOwnSplitAmount ledgerEntry.transactionId

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
