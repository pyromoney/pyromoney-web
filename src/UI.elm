module UI exposing (accountSelect)

import Data.Account exposing (Account, AccountId)
import Element exposing (Element, html)
import Html exposing (option, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)
import Tree


accountSelect : Tree.Multitree Account -> Maybe AccountId -> (AccountId -> msg) -> Element msg
accountSelect accountsTree maybeAccountId msg =
    select [ onInput msg ]
        [ option
            [ value "account-id"
            , selected True
            ]
            [ text "Some account" ]
        ]
        |> html
