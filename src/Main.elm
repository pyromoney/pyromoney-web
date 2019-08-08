module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data.Account exposing (Account)
import Element
import Html
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Page.Account
import Page.Index
import Tree
import Utils



---- MODEL ----


type Page
    = IndexPage Page.Index.Model
    | AccountPage Page.Account.Model


type Msg
    = RequestAccounts
    | ReceiveAccounts (Result Http.Error (List Account))
    | IndexMsg Page.Index.Msg
    | AccountMsg Page.Account.Msg


type alias ServerUrl =
    String


type alias AppState =
    { serverUrl : ServerUrl
    , accounts : List Account
    , accountsTree : Tree.Multitree Account
    , lastError : String
    }


type alias Model =
    { appState : AppState
    , currentPage : Page
    }


type alias Flags =
    { serverUrl : ServerUrl
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        appState =
            { serverUrl = flags.serverUrl
            , accounts = []
            , accountsTree = []
            , lastError = ""
            }

        ( pageModel, pageCmd ) =
            Page.Index.init

        fetchAccountsCmd =
            fetchAccounts flags.serverUrl

        mappedPageCmd =
            Cmd.map IndexMsg pageCmd
    in
    ( { appState = appState
      , currentPage = IndexPage pageModel
      }
    , Cmd.batch [ fetchAccountsCmd, mappedPageCmd ]
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
update msg ({ appState } as model) =
    case msg of
        RequestAccounts ->
            ( model, fetchAccounts appState.serverUrl )

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

                newAppState =
                    { appState | accounts = accounts, accountsTree = accountsTree }
            in
            ( { model | appState = newAppState }, Cmd.none )

        ReceiveAccounts (Err error) ->
            let
                newAppState =
                    { appState | lastError = Utils.httpErrorString error }
            in
            ( { model | appState = newAppState }, Cmd.none )

        IndexMsg pageMsg ->
            case pageMsg of
                Page.Index.OpenAccount account ->
                    let
                        ( pageModel, pageCmd ) =
                            Page.Account.init account
                    in
                    ( { model | currentPage = AccountPage pageModel }, Cmd.map AccountMsg pageCmd )

        AccountMsg _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    let
        appState =
            model.appState

        pageView =
            case model.currentPage of
                IndexPage pageModel ->
                    Page.Index.view appState pageModel |> Element.map IndexMsg

                AccountPage pageModel ->
                    Page.Account.view pageModel |> Element.map AccountMsg
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
