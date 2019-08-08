module Data.Transaction exposing (Split, Transaction, decodeTransaction)

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
    , accountId : AccountId
    , description : String
    , amount : Float
    }


type alias TransactionId =
    String


type alias SplitId =
    String


type alias AccountId =
    String


decodeTransaction : Decode.Decoder Transaction
decodeTransaction =
    Decode.succeed Transaction
        |> required "id" Decode.string
        |> optional "description" Decode.string ""
        |> required "timestamp" decodeTimestamp
        |> required "splits" (Decode.list decodeSplit)


decodeSplit : Decode.Decoder Split
decodeSplit =
    Decode.succeed Split
        |> required "id" Decode.string
        |> required "account_id" Decode.string
        |> optional "description" Decode.string ""
        |> required "amount" decodeAmount


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
