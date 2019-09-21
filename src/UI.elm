module UI exposing (accountSelect)

import Data.Account exposing (Account, AccountId)
import Element exposing (Element, html)
import Html exposing (option, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)
import Tree


accountSelect : Tree.Multitree Account -> Maybe AccountId -> (AccountId -> msg) -> Element msg
accountSelect accountsTree maybeAccountId msg =
    let
        noSelection =
            accountOption "" "(none)" <| (maybeAccountId == Nothing)
    in
    select [ onInput msg ]
        (noSelection
            :: (accountsTree
                    |> Tree.toList
                    |> List.map
                        (\{ id, name } ->
                            accountOption id name <| Just id == maybeAccountId
                        )
               )
        )
        |> html


accountOption : AccountId -> String -> Bool -> Html.Html msg
accountOption id name isSelected =
    option
        [ value id
        , selected isSelected
        ]
        [ text name ]
