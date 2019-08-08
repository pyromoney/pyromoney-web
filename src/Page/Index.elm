module Page.Index exposing (Model, Msg(..), init, view)

import Data.Account exposing (Account, Type(..))
import Element exposing (..)
import Element.Events exposing (onClick)
import Tree


type Msg
    = OpenAccount Account


type alias Model =
    { lastError : String
    }


init : ( Model, Cmd Msg )
init =
    ( { lastError = "" }
    , Cmd.none
    )


view : { a | accountsTree : Tree.Multitree Account } -> Model -> Element Msg
view appState model =
    column
        [ width fill ]
    <|
        [ text model.lastError ]
            ++ viewAccounts 0 appState.accountsTree


viewAccounts : Int -> Tree.Multitree Account -> List (Element Msg)
viewAccounts depth accounts =
    List.concatMap (viewAccount depth) accounts


viewAccount : Int -> Tree.Node Account -> List (Element Msg)
viewAccount depth node =
    let
        (Tree.Node account childNodes) =
            node

        style =
            [ paddingEach { left = 20 * depth, right = 0, top = 0, bottom = 0 }
            , pointer
            , onClick (OpenAccount account)
            ]
    in
    [ row [ width fill ]
        [ column [ width (fillPortion 6) ] [ el style (text account.name) ]
        , column [ width (fillPortion 2) ] [ el [] (text (viewType account)) ]
        , column [ width (fillPortion 2) ] [ el [] (text account.currency) ]
        ]
    ]
        ++ viewAccounts (depth + 1) childNodes


viewType : Account -> String
viewType account =
    case account.type_ of
        Asset ->
            "Asset"

        Cash ->
            "Cash"

        Bank ->
            "Bank"

        Liability ->
            "Liability"

        Income ->
            "Income"

        Expense ->
            "Expense"

        Equity ->
            "Equity"

        Trading ->
            "Trading"
