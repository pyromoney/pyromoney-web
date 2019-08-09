module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data.Account exposing (Account, decodeAccount)
import Dict exposing (Dict)
import Element
import Html
import Http
import Json.Decode as Decode
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


type alias AccountId =
    String


type alias AppState =
    { serverUrl : ServerUrl
    , accountsDict : Dict AccountId Account
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
            , accountsDict = Dict.empty
            , accountsTree = Tree.empty
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


fetchAccounts : ServerUrl -> Cmd Msg
fetchAccounts serverUrl =
    let
        decoder =
            Decode.field "data" (Decode.list decodeAccount)
    in
    Http.get
        { url = serverUrl ++ "/accounts"
        , expect = Http.expectJson ReceiveAccounts decoder
        }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ appState, currentPage } as model) =
    case ( msg, currentPage ) of
        ( RequestAccounts, _ ) ->
            ( model, fetchAccounts appState.serverUrl )

        ( ReceiveAccounts (Ok accounts), _ ) ->
            let
                accountsDict =
                    accounts
                        |> List.map (\a -> ( a.id, a ))
                        |> Dict.fromList

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
                    { appState | accountsDict = accountsDict, accountsTree = accountsTree }
            in
            ( { model | appState = newAppState }, Cmd.none )

        ( ReceiveAccounts (Err error), _ ) ->
            let
                newAppState =
                    { appState | lastError = Utils.httpErrorString error }
            in
            ( { model | appState = newAppState }, Cmd.none )

        ( IndexMsg (Page.Index.OpenAccount account), IndexPage _ ) ->
            let
                ( newPageModel, newPageCmd ) =
                    Page.Account.init appState account
            in
            ( { model | currentPage = AccountPage newPageModel }
            , Cmd.map AccountMsg newPageCmd
            )

        ( AccountMsg pageMsg, AccountPage pageModel ) ->
            let
                ( newPageModel, newPageCmd ) =
                    Page.Account.update appState pageMsg pageModel
            in
            ( { model | currentPage = AccountPage newPageModel }
            , Cmd.map AccountMsg newPageCmd
            )

        ( _, _ ) ->
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
                    Page.Account.view appState pageModel |> Element.map AccountMsg
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
