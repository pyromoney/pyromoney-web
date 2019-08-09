module Data.Transaction exposing (Split, Transaction, decodeTransaction)

import Data.Account exposing (Account)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Time


type alias Transaction =
    { id : TransactionId
    , description : String
    , timestamp : Time.Posix
    , splits : List Split
    }


type alias Split =
    { id : SplitId
    , account : Account
    , description : String
    , amount : Float
    }


type alias TransactionId =
    String


type alias SplitId =
    String


type alias AccountId =
    String


decodeTransaction : Dict AccountId Account -> Decode.Decoder Transaction
decodeTransaction accountsDict =
    Decode.succeed Transaction
        |> required "id" Decode.string
        |> optional "description" Decode.string ""
        |> required "timestamp" decodeTimestamp
        |> required "splits" (Decode.list <| decodeSplit accountsDict)


decodeSplit : Dict AccountId Account -> Decode.Decoder Split
decodeSplit accountsDict =
    Decode.succeed Split
        |> required "id" Decode.string
        |> required "account_id" (decodeAccount accountsDict)
        |> optional "description" Decode.string ""
        |> required "amount" decodeAmount


decodeAccount : Dict AccountId Account -> Decode.Decoder Account
decodeAccount accountsDict =
    Decode.string
        |> Decode.andThen
            (\accountId ->
                case Dict.get accountId accountsDict of
                    Just account ->
                        Decode.succeed account

                    Nothing ->
                        Decode.fail <| "Unknown account " ++ accountId
            )


decodeTimestamp : Decode.Decoder Time.Posix
decodeTimestamp =
    Decode.string
        |> Decode.andThen
            (\val ->
                case Iso8601.toTime val of
                    Ok time ->
                        Decode.succeed time

                    Err _ ->
                        Decode.fail <| "Can't parse timestamp " ++ val
            )


decodeAmount : Decode.Decoder Float
decodeAmount =
    Decode.string
        |> Decode.andThen
            (\val ->
                case String.toFloat val of
                    Just f ->
                        Decode.succeed f

                    Nothing ->
                        Decode.fail <| "Can't decode amount " ++ val
            )
