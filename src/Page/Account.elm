module Page.Account exposing (Model, Msg(..), init, update, view)

import Data.Account exposing (Account, AccountId)
import Data.Transaction as Transaction exposing (LedgerEntry, TransactionId, decodeLedgerEntry)
import DateFormat
import Dict exposing (Dict)
import Editable exposing (Editable(..))
import Element exposing (Attribute, Element, column, el, fill, fillPortion, html, row, text, width)
import Element.Events exposing (onClick)
import Element.Input as Input exposing (focusedOnLoad)
import FormValue exposing (FormValue(..))
import Http
import Json.Decode as Decode
import List.Extra as LE
import Loadable exposing (Loadable(..))
import Time
import Tree
import UI exposing (accountSelect, columnRow, formValueEdit, onEnter)
import Utils


type Msg
    = RequestLedgerEntries Account
    | ReceiveLedgerEntries (Result Http.Error (List LedgerEntry))
    | EditLedgerEntry TransactionId
    | ChangeLedgerEntryDescription TransactionId String
    | ChangeLedgerEntryOtherSplitAmount TransactionId String
    | ChangeLedgerEntryOwnSplitAmount TransactionId String
    | ChangeLedgerEntryAccount TransactionId AccountId
    | SaveLedgerEntry (Editable LedgerEntryForm)
    | SavedLedgerEntry (Result Http.Error LedgerEntryForm)



-- MODEL


type alias Model =
    { account : Account
    , ledgerEntries : Loadable (List (Editable LedgerEntryForm)) ()
    , timezone : Time.Zone
    }



-- Representation of LedgerEntry for display and editing purposes.


type alias LedgerEntryForm =
    { timestamp : Time.Posix
    , transactionId : TransactionId
    , description : FormValue String
    , ownSplitAmount : FormValue Float
    , otherSplit : OtherSplit
    }



-- Represents the other split (or splits).


type OtherSplit
    = SingleSplit
        { amount : FormValue Float
        , accountId : AccountId
        }
    | MultipleSplits



-- Configuration received from Main.


type alias Config a =
    { a | serverUrl : ServerUrl }



-- Data received from Main.


type alias AppState a =
    { a
        | accountsDict : Dict AccountId Account
        , accountsTree : Tree.Multitree Account
    }


type alias ServerUrl =
    String


eqTransactionId : TransactionId -> Editable LedgerEntryForm -> Bool
eqTransactionId id =
    (==) id << .transactionId << Editable.target


updateLedgerEntry : TransactionId -> (Editable LedgerEntryForm -> Editable LedgerEntryForm) -> Model -> Model
updateLedgerEntry transactionId f model =
    { model
        | ledgerEntries =
            model.ledgerEntries
                |> Loadable.map (LE.updateIf (eqTransactionId transactionId) f)
    }


editLedgerEntry : TransactionId -> Model -> Model
editLedgerEntry transactionId model =
    { model
        | ledgerEntries =
            model.ledgerEntries
                |> Loadable.map (List.map Editable.cancel)
    }
        |> updateLedgerEntry transactionId Editable.edit


setLedgerEntryDescription : TransactionId -> String -> Model -> Model
setLedgerEntryDescription transactionId newDescription =
    updateLedgerEntry transactionId <|
        Editable.map
            (\entry ->
                { entry | description = FormValue.fromString newDescription }
            )


setLedgerEntryOwnSplitAmount : TransactionId -> String -> Model -> Model
setLedgerEntryOwnSplitAmount transactionId newAmount =
    updateLedgerEntry transactionId <|
        Editable.map
            (\entry ->
                { entry | ownSplitAmount = FormValue.parseFloat newAmount }
            )


setLedgerEntryOtherSplitAmount : TransactionId -> String -> Model -> Model
setLedgerEntryOtherSplitAmount transactionId newAmount =
    updateLedgerEntry transactionId <|
        Editable.map
            (\entry ->
                case entry.otherSplit of
                    SingleSplit otherSplit ->
                        let
                            updatedSplit =
                                SingleSplit { otherSplit | amount = FormValue.parseFloat newAmount }
                        in
                        { entry | otherSplit = updatedSplit }

                    MultipleSplits ->
                        entry
            )


setLedgerEntryAccount : TransactionId -> AccountId -> Model -> Model
setLedgerEntryAccount transactionId accountId =
    updateLedgerEntry transactionId <|
        Editable.map
            (\entry ->
                case entry.otherSplit of
                    SingleSplit otherSplit ->
                        let
                            updatedSplit =
                                SingleSplit { otherSplit | accountId = accountId }
                        in
                        { entry | otherSplit = updatedSplit }

                    MultipleSplits ->
                        entry
            )


ensureNewLedgerEntry : Model -> Model
ensureNewLedgerEntry model =
    let
        doEnsure entries =
            let
                ( newEntries, rest ) =
                    entries
                        |> List.partition Editable.isNew

                newEntry =
                    newEntries
                        |> List.head
                        |> Maybe.withDefault (Editable.fromNew makeLedgerEntry)
            in
            rest ++ [ newEntry ]
    in
    { model
        | ledgerEntries =
            model.ledgerEntries
                |> Loadable.map doEnsure
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


parseLedgerEntry : LedgerEntry -> LedgerEntryForm
parseLedgerEntry { transaction, split, otherSplits } =
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



-- INIT


init : Config a -> AppState b -> Account -> ( Model, Cmd Msg )
init { serverUrl } { accountsDict } account =
    ( { account = account
      , ledgerEntries = Loading ()
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



-- UPDATE


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
                        |> List.map (Editable.fromSaved << parseLedgerEntry)
                        |> Loaded
              }
                |> ensureNewLedgerEntry
            , Cmd.none
            )

        ReceiveLedgerEntries (Err error) ->
            ( { model | ledgerEntries = Failure <| Utils.httpErrorString error }
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
            ( model |> setLedgerEntryAccount transactionId accountId
            , Cmd.none
            )

        SaveLedgerEntry (Editable state originalEntry) ->
            -- TODO: Do an actual HTTP request.
            case state of
                Editable.New ->
                    ( model
                    , Utils.send <| SavedLedgerEntry <| Ok originalEntry
                    )

                Editable.Saved ->
                    -- Meh. It's probably better to handle saving entries that
                    -- are already saved because it makes it complete and we
                    -- avoid surprises if the unexpected happens and we start
                    -- having a case for saving already saved items.
                    ( model
                    , Utils.send <| SavedLedgerEntry <| Ok originalEntry
                    )

                Editable.Editing modifiedEntry ->
                    ( model
                    , Utils.send <| SavedLedgerEntry <| Ok modifiedEntry
                    )

        SavedLedgerEntry (Ok ledgerEntry) ->
            ( model
                |> updateLedgerEntry ledgerEntry.transactionId
                    (\_ -> Editable.fromSaved ledgerEntry)
                |> ensureNewLedgerEntry
            , Cmd.none
            )

        SavedLedgerEntry (Err err) ->
            Debug.todo "Implement flash messages?"



-- VIEW


view : AppState a -> Model -> Element Msg
view appState model =
    column [ width fill ] <|
        [ el [] <| text model.account.name
        , viewLedger appState model
        ]


viewLedger : AppState a -> Model -> Element Msg
viewLedger appState model =
    case model.ledgerEntries of
        Loaded ledgerEntries ->
            row [ width fill ] <|
                [ column [ width fill ] <|
                    viewLedgerRow []
                        [ text "Date"
                        , text "Description"
                        , text "Transfer"
                        , text "Own split"
                        , text "Other split"
                        ]
                        :: List.map (viewEntryRow appState model.timezone) ledgerEntries
                ]

        Loading _ ->
            text "Loading"

        Failure err ->
            text err


viewLedgerRow : List (Attribute Msg) -> List (Element Msg) -> Element Msg
viewLedgerRow attrs cols =
    columnRow ([ width fill ] ++ attrs) [ 10, 40, 20, 10, 10 ] cols


viewEntryRow : AppState a -> Time.Zone -> Editable LedgerEntryForm -> Element Msg
viewEntryRow { accountsTree, accountsDict } timezone ((Editable state ledgerEntry) as editableEntry) =
    case state of
        Editable.Saved ->
            viewSavedEntryRow accountsDict timezone ledgerEntry

        Editable.New ->
            viewEditingEntryRow accountsTree
                timezone
                ledgerEntry
                (SaveLedgerEntry editableEntry)

        Editable.Editing modifiedEntry ->
            viewEditingEntryRow accountsTree
                timezone
                modifiedEntry
                (SaveLedgerEntry editableEntry)


viewSavedEntryRow : Dict AccountId Account -> Time.Zone -> LedgerEntryForm -> Element Msg
viewSavedEntryRow accountsDict timezone ledgerEntry =
    let
        makeEditable : Msg -> Element Msg -> Element Msg
        makeEditable msg content =
            el [ onClick msg ] content
    in
    [ viewTimestamp timezone ledgerEntry.timestamp
    , text (ledgerEntry.description |> FormValue.toString)
    , viewIfSingleSplit ledgerEntry <|
        \{ accountId } ->
            accountsDict
                |> Dict.get accountId
                |> Maybe.map (text << .name)
                |> Maybe.withDefault (text "")
    , viewSplitAmount ledgerEntry.ownSplitAmount
    , viewIfSingleSplit ledgerEntry <|
        \{ amount } ->
            viewSplitAmount amount
    ]
        |> List.map (makeEditable <| EditLedgerEntry ledgerEntry.transactionId)
        |> viewLedgerRow [ onClick <| EditLedgerEntry ledgerEntry.transactionId ]


viewEditingEntryRow : Tree.Multitree Account -> Time.Zone -> LedgerEntryForm -> Msg -> Element Msg
viewEditingEntryRow accountsTree timezone ledgerEntry saveMsg =
    let
        editAttrs =
            [ width fill
            , onEnter saveMsg
            ]
    in
    [ viewTimestamp timezone ledgerEntry.timestamp
    , formValueEdit (editAttrs ++ [ focusedOnLoad ]) ledgerEntry.description <|
        ChangeLedgerEntryDescription ledgerEntry.transactionId
    , viewIfSingleSplit ledgerEntry <|
        \{ accountId } ->
            -- TOOD: Smell because accountId could theoretically be "" but we never really pass Nothing so why Maybe.
            accountSelect editAttrs accountsTree (Just accountId) <|
                ChangeLedgerEntryAccount ledgerEntry.transactionId
    , viewIfSingleSplit ledgerEntry <|
        \{ amount } ->
            formValueEdit editAttrs amount <|
                ChangeLedgerEntryOtherSplitAmount ledgerEntry.transactionId
    , viewIfSingleSplit ledgerEntry <|
        \_ ->
            formValueEdit editAttrs ledgerEntry.ownSplitAmount <|
                ChangeLedgerEntryOwnSplitAmount ledgerEntry.transactionId
    ]
        |> viewLedgerRow []


viewIfSingleSplit : LedgerEntryForm -> ({ amount : FormValue Float, accountId : AccountId } -> Element Msg) -> Element Msg
viewIfSingleSplit ledgerEntry present =
    case ledgerEntry.otherSplit of
        SingleSplit split ->
            present split

        MultipleSplits ->
            text "Split transaction"


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
