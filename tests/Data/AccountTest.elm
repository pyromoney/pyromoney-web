module Data.AccountTest exposing (all)

import Data.Account exposing (Type(..), decodeAccount)
import Expect
import Json.Decode
import Test exposing (..)


all : Test
all =
    describe "decodeAccount"
        [ test "Decodes account with parent" <|
            \_ ->
                let
                    json =
                        """
                        { "id": "827d51e1-6018-4076-b1f4-16cd839e013a"
                        , "parent_id": "64f1500b-9c45-4a26-97b5-e7f62eee1a2a"
                        , "name": "Cash"
                        , "type": "cash"
                        , "currency": "PLN"
                        , "hidden": false
                        , "virtual": false
                        }
                        """

                    result =
                        Json.Decode.decodeString decodeAccount json
                in
                Expect.equal result
                    (Ok
                        { id = "827d51e1-6018-4076-b1f4-16cd839e013a"
                        , parentId = Just "64f1500b-9c45-4a26-97b5-e7f62eee1a2a"
                        , name = "Cash"
                        , type_ = Cash
                        , currency = "PLN"
                        , hidden = False
                        , virtual = False
                        }
                    )
        , test "Decodes account without parent" <|
            \_ ->
                let
                    json =
                        """
                        { "id": "827d51e1-6018-4076-b1f4-16cd839e013a"
                        , "parent_id": null
                        , "name": "Cash"
                        , "type": "cash"
                        , "currency": "PLN"
                        , "hidden": false
                        , "virtual": false
                        }
                        """

                    result =
                        Json.Decode.decodeString decodeAccount json
                in
                Expect.equal result
                    (Ok
                        { id = "827d51e1-6018-4076-b1f4-16cd839e013a"
                        , parentId = Nothing
                        , name = "Cash"
                        , type_ = Cash
                        , currency = "PLN"
                        , hidden = False
                        , virtual = False
                        }
                    )
        , test "Fails to decode an account with invalid type" <|
            \_ ->
                let
                    json =
                        """
                        { "id": "827d51e1-6018-4076-b1f4-16cd839e013a"
                        , "parent_id": "64f1500b-9c45-4a26-97b5-e7f62eee1a2a"
                        , "name": "Cash"
                        , "type": "unknown"
                        , "currency": "PLN"
                        , "hidden": false
                        , "virtual": false
                        }
                        """

                    result =
                        Json.Decode.decodeString decodeAccount json
                in
                Expect.err result
        ]
