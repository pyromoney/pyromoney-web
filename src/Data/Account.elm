module Data.Account exposing (Account, Type(..), decodeAccount)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


type Type
    = Asset
    | Cash
    | Bank
    | Liability
    | Income
    | Expense
    | Equity
    | Trading


type alias AccountId =
    String


type alias Account =
    { id : AccountId
    , parentId : Maybe AccountId
    , name : String
    , type_ : Type
    , currency : String
    , hidden : Bool
    , virtual : Bool
    }


decodeAccount : Decode.Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "id" Decode.string
        |> required "parent_id" (Decode.nullable Decode.string)
        |> required "name" Decode.string
        |> required "type" decodeType
        |> required "currency" Decode.string
        |> required "hidden" Decode.bool
        |> required "virtual" Decode.bool


decodeType : Decode.Decoder Type
decodeType =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "asset" ->
                        Decode.succeed Asset

                    "cash" ->
                        Decode.succeed Cash

                    "bank" ->
                        Decode.succeed Bank

                    "liability" ->
                        Decode.succeed Liability

                    "income" ->
                        Decode.succeed Income

                    "expense" ->
                        Decode.succeed Expense

                    "equity" ->
                        Decode.succeed Equity

                    "trading" ->
                        Decode.succeed Trading

                    somethingElse ->
                        Decode.fail <| "Unknown account type: " ++ somethingElse
            )
