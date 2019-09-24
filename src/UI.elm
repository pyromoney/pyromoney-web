module UI exposing (accountSelect, columnRow, formValueEdit, onEnter, zIndex)

import Data.Account exposing (Account, AccountId)
import Element exposing (Attribute, Element, html, htmlAttribute)
import Element.Input as Input exposing (labelHidden)
import FormValue exposing (FormValue)
import Html exposing (Html, option, select, text)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Tree


accountSelect : List (Attribute msg) -> Tree.Multitree Account -> Maybe AccountId -> (AccountId -> msg) -> Element msg
accountSelect attrs accountsTree maybeAccountId msg =
    let
        accountOption : AccountId -> String -> Bool -> Html msg
        accountOption id name isSelected =
            option
                [ value id
                , selected isSelected
                ]
                [ text name ]

        noSelection =
            accountOption "" "(none)" <| (maybeAccountId == Nothing)
    in
    select
        [ onInput msg
        , style "width" "100%"
        ]
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
        |> Element.el attrs


formValueEdit : List (Attribute msg) -> FormValue a -> (String -> msg) -> Element msg
formValueEdit attrs value msg =
    Input.text attrs
        { onChange = msg
        , text = FormValue.toString value
        , placeholder = Nothing
        , label = labelHidden ""
        }


columnRow : List (Attribute msg) -> List Int -> List (Element msg) -> Element msg
columnRow attrs widths cols =
    let
        col w content =
            Element.column [ Element.width (Element.fillPortion w) ] [ content ]
    in
    Element.row ([ Element.width Element.fill ] ++ attrs) <|
        List.map2 col widths cols


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


zIndex : Int -> Element.Attribute msg
zIndex i =
    style "z-index" (String.fromInt i)
        |> htmlAttribute
