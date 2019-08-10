module Data.TransactionTest exposing (decodeLedgerEntrySuite, decodeSplitSuite, decodeTransactionSuite)

import Data.Account exposing (Type(..))
import Data.Transaction exposing (decodeLedgerEntry, decodeSplit, decodeTransaction)
import Dict
import Expect
import Json.Decode
import Test exposing (..)
import Time


assetsAccount =
    { id = "account-1"
    , parentId = Nothing
    , name = "Assets"
    , type_ = Asset
    , currency = "PLN"
    , hidden = False
    , virtual = False
    }


cashAccount =
    { id = "account-2"
    , parentId = Just "account-1"
    , name = "Cash"
    , type_ = Cash
    , currency = "PLN"
    , hidden = False
    , virtual = False
    }


bankAccount =
    { id = "account-3"
    , parentId = Just "account-1"
    , name = "Bank"
    , type_ = Bank
    , currency = "PLN"
    , hidden = False
    , virtual = False
    }


accountsDict =
    Dict.fromList
        [ ( assetsAccount.id, assetsAccount )
        , ( cashAccount.id, cashAccount )
        , ( bankAccount.id, bankAccount )
        ]


transactionJson =
    """
    { "id": "transaction-1"
    , "description": "Example"
    , "timestamp": "2019-08-10T17:08:25.144166Z"
    , "splits":
        [
            { "id": "split-1"
            , "account_id": "account-1"
            , "description": null
            , "amount": "-100.50"
            },
            { "id": "split-2"
            , "account_id": "account-2"
            , "description": null
            , "amount": "100.50"
            }
        ]
    }
    """


decodeTransactionSuite : Test
decodeTransactionSuite =
    describe "decodeTransaction"
        [ test "Decodes transaction and its splits" <|
            \_ ->
                let
                    result =
                        Json.Decode.decodeString (decodeTransaction accountsDict) transactionJson
                in
                Expect.equal result
                    (Ok
                        { id = "transaction-1"
                        , description = "Example"
                        , timestamp = Time.millisToPosix 1565456905144
                        , splits =
                            [ { id = "split-1"
                              , account = assetsAccount
                              , description = ""
                              , amount = -100.5
                              }
                            , { id = "split-2"
                              , account = cashAccount
                              , description = ""
                              , amount = 100.5
                              }
                            ]
                        }
                    )
        ]


decodeSplitSuite : Test
decodeSplitSuite =
    describe "decodeSplit"
        [ test "Decodes split with preloaded account" <|
            \_ ->
                let
                    json =
                        """
                        { "id": "split-1"
                        , "account_id": "account-1"
                        , "description": null
                        , "amount": "-100.50"
                        }
                        """

                    result =
                        Json.Decode.decodeString (decodeSplit accountsDict) json
                in
                Expect.equal result
                    (Ok
                        { id = "split-1"
                        , account = assetsAccount
                        , description = ""
                        , amount = -100.5
                        }
                    )
        , test "Fails to decode a split with not preloaded account" <|
            \_ ->
                let
                    json =
                        """
                        { "id": "split-1"
                        , "account_id": "account-0"
                        , "description": null
                        , "amount": "-100.50"
                        }
                        """

                    result =
                        Json.Decode.decodeString (decodeSplit accountsDict) json
                in
                Expect.err result
        ]


decodeLedgerEntrySuite : Test
decodeLedgerEntrySuite =
    describe "decodeLedgerEntry"
        [ test "Decodes ledger entry" <|
            \_ ->
                let
                    result =
                        Json.Decode.decodeString (decodeLedgerEntry accountsDict cashAccount) transactionJson
                in
                Expect.equal result
                    (Ok
                        { transaction =
                            { id = "transaction-1"
                            , description = "Example"
                            , timestamp = Time.millisToPosix 1565456905144
                            , splits =
                                [ { id = "split-1"
                                  , account = assetsAccount
                                  , description = ""
                                  , amount = -100.5
                                  }
                                , { id = "split-2"
                                  , account = cashAccount
                                  , description = ""
                                  , amount = 100.5
                                  }
                                ]
                            }
                        , split =
                            { id = "split-2"
                            , account = cashAccount
                            , description = ""
                            , amount = 100.5
                            }
                        , otherSplits =
                            [ { id = "split-1"
                              , account = assetsAccount
                              , description = ""
                              , amount = -100.5
                              }
                            ]
                        }
                    )
        , test "Fails to decode ledger entry in a context of wrong account" <|
            \_ ->
                let
                    result =
                        Json.Decode.decodeString (decodeLedgerEntry accountsDict bankAccount) transactionJson
                in
                Expect.err result
        ]
