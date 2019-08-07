module Data.Account exposing (Account)


type alias AccountId =
    String


type alias Account =
    { id : AccountId
    , parentId : Maybe AccountId
    , name : String
    , type_ : String
    , currency : String
    , hidden : Bool
    , virtual : Bool
    }
