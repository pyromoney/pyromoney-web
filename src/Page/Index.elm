module Page.Index exposing (Model, Msg, view)

import Data.Account exposing (Account)
import Element exposing (..)
import Tree


type Msg
    = NoOp


type alias Model =
    { accountsTree : Tree.Multitree Account
    , lastError : String
    }


view : Model -> Element Msg
view model =
    column
        [ width fill ]
    <|
        [ text model.lastError ]
            ++ viewAccounts 0 model.accountsTree


viewAccounts : Int -> Tree.Multitree Account -> List (Element Msg)
viewAccounts depth accounts =
    List.concatMap (viewAccount depth) accounts


viewAccount : Int -> Tree.Node Account -> List (Element Msg)
viewAccount depth node =
    let
        (Tree.Node account childNodes) =
            node

        padding =
            [ paddingEach { left = 20 * depth, right = 0, top = 0, bottom = 0 } ]
    in
    [ row [ width fill ]
        [ column [ width (fillPortion 6) ] [ el padding (text account.name) ]
        , column [ width (fillPortion 2) ] [ el [] (text account.type_) ]
        , column [ width (fillPortion 2) ] [ el [] (text account.currency) ]
        ]
    ]
        ++ viewAccounts (depth + 1) childNodes
