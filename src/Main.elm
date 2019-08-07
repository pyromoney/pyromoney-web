module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data.Account exposing (Account)
import Element
import Html
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Page.Index
import Tree
import Utils



---- MODEL ----


type Msg
    = RequestAccounts
    | ReceiveAccounts (Result Http.Error (List Account))
    | Index Page.Index.Msg


type alias ServerUrl =
    String


type alias Model =
    { serverUrl : ServerUrl
    , accounts : List Account
    , accountsTree : Tree.Multitree Account
    , lastError : String
    }


type alias Flags =
    { serverUrl : ServerUrl
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { serverUrl = flags.serverUrl
      , accounts = []
      , accountsTree = []
      , lastError = ""
      }
    , fetchAccounts flags.serverUrl
    )


decodeAccounts : Decode.Decoder (List Account)
decodeAccounts =
    Decode.field "data" (Decode.list decodeAccount)


decodeAccount : Decode.Decoder Account
decodeAccount =
    Decode.succeed Account
        |> required "id" Decode.string
        |> required "parent_id" (Decode.nullable Decode.string)
        |> required "name" Decode.string
        |> required "type" Decode.string
        |> required "currency" Decode.string
        |> required "hidden" Decode.bool
        |> required "virtual" Decode.bool


fetchAccounts : ServerUrl -> Cmd Msg
fetchAccounts serverUrl =
    Http.get
        { url = serverUrl ++ "/accounts"
        , expect = Http.expectJson ReceiveAccounts decodeAccounts
        }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestAccounts ->
            ( model, fetchAccounts model.serverUrl )

        ReceiveAccounts (Ok accounts) ->
            let
                rootsFilter account =
                    case account.parentId of
                        Just _ ->
                            False

                        Nothing ->
                            True

                childrenFilter parentAccount account =
                    case account.parentId of
                        Just id ->
                            id == parentAccount.id

                        Nothing ->
                            False

                accountsTree =
                    Tree.toTree rootsFilter childrenFilter accounts
            in
            ( { model | accounts = accounts, accountsTree = accountsTree }, Cmd.none )

        ReceiveAccounts (Err error) ->
            ( { model | lastError = Utils.httpErrorString error }, Cmd.none )

        Index _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    let
        pageModel =
            { accountsTree = model.accountsTree
            , lastError = model.lastError
            }

        pageView =
            Page.Index.view pageModel |> Element.map Index
    in
    Element.layout [] pageView



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
