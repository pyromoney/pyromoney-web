module Data.Transaction exposing (LedgerEntry, Split, Transaction, TransactionId, decodeLedgerEntry, decodeSplit, decodeTransaction)

import Data.Account exposing (Account)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Time



{-

   A transaction is an exchange between at least two accounts.
   A single transaction always consists of at least two parts, a from and a to account.

   These parts are called Splits.

   A transaction with only two splits is called a simple transaction.
   A transaction with three or more accounts is called a split transaction.
   Though we don't distinguish them in our data models.

   For convenience there is a LedgerEntry type which is basically a transaction
   in context of the specific account. It's `split` attribute is a split belonging
   to the selected account and the `otherSplits` is a list of splits that belong
   to other accounts.

   If decoder fails to find any of the specified accounts the whole JSON is considered invalid.
-}


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


type alias LedgerEntry =
    { transaction : Transaction
    , split : Split
    , otherSplits : List Split
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


decodeLedgerEntry : Dict AccountId Account -> Account -> Decode.Decoder LedgerEntry
decodeLedgerEntry accountsDict currentAccount =
    decodeTransaction accountsDict
        |> Decode.andThen
            (\transaction ->
                let
                    isCurrent =
                        \split -> split.account.id == currentAccount.id

                    currentSplits =
                        List.filter isCurrent transaction.splits

                    otherSplits =
                        List.filter (not << isCurrent) transaction.splits
                in
                case ( currentSplits, otherSplits ) of
                    ( [], _ ) ->
                        Decode.fail <| "Missing current split in " ++ transaction.id

                    ( _, [] ) ->
                        Decode.fail <| "Missing other splits in " ++ transaction.id

                    ( [ split ], _ ) ->
                        Decode.succeed
                            { transaction = transaction
                            , split = split
                            , otherSplits = otherSplits
                            }

                    ( _, _ ) ->
                        Decode.fail <| "Too many matching current splits in " ++ transaction.id
            )


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
