module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data.Account exposing (Account, decodeAccount)
import Dict exposing (Dict)
import Element
import Html
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Loadable exposing (Loadable(..))
import Page.Account
import Page.Index
import Task
import Tree
import Utils



---- MODEL ----


type Page
    = IndexPage Page.Index.Model
    | AccountPage Page.Account.Model


type Msg
    = ReceiveAccounts (Result Http.Error (List Account))
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
    }


type NextPage
    = NextIndexPage
    | NextAccountPage AccountId


type alias Model =
    Loadable
        { appState : AppState
        , currentPage : Page
        }
        NextPage


type alias Flags =
    { serverUrl : ServerUrl
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Loading NextIndexPage
    , fetchAccounts flags.serverUrl
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
update msg model =
    case model of
        Loaded ({ appState, currentPage } as loadedModel) ->
            case ( msg, currentPage ) of
                ( IndexMsg (Page.Index.OpenAccount account), IndexPage _ ) ->
                    let
                        ( newPageModel, newPageCmd ) =
                            Page.Account.init appState account
                    in
                    ( Loaded { loadedModel | currentPage = AccountPage newPageModel }
                    , Cmd.map AccountMsg newPageCmd
                    )

                ( AccountMsg pageMsg, AccountPage pageModel ) ->
                    let
                        ( newPageModel, newPageCmd ) =
                            Page.Account.update appState pageMsg pageModel
                    in
                    ( Loaded { loadedModel | currentPage = AccountPage newPageModel }
                    , Cmd.map AccountMsg newPageCmd
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        Loading nextPage ->
            case msg of
                ReceiveAccounts (Ok accounts) ->
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
                            { serverUrl = "http://localhost:4000" -- TODO
                            , accountsDict = accountsDict
                            , accountsTree = accountsTree
                            }
                    in
                    nextPage |> resolveNextPage newAppState

                ReceiveAccounts (Err error) ->
                    ( Failure <| Utils.httpErrorString error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Failure _ ->
            ( model, Cmd.none )


resolveNextPage : AppState -> NextPage -> ( Model, Cmd Msg )
resolveNextPage appState nextPage =
    case nextPage of
        NextIndexPage ->
            let
                ( pageModel, cmd ) =
                    Page.Index.init
            in
            ( Loaded { appState = appState, currentPage = IndexPage pageModel }, Cmd.map IndexMsg cmd )

        NextAccountPage accountId ->
            appState.accountsDict
                |> Dict.get accountId
                |> Maybe.map
                    (\account ->
                        let
                            ( pageModel, cmd ) =
                                Page.Account.init appState account
                        in
                        ( Loaded { appState = appState, currentPage = AccountPage pageModel }, Cmd.map AccountMsg cmd )
                    )
                |> Maybe.withDefault ( Failure "No such account", Cmd.none )


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    (case model of
        Loading _ ->
            Element.text "Loading..."

        Loaded loadedModel ->
            case loadedModel.currentPage of
                IndexPage pageModel ->
                    Page.Index.view loadedModel.appState pageModel |> Element.map IndexMsg

                AccountPage pageModel ->
                    Page.Account.view loadedModel.appState pageModel |> Element.map AccountMsg

        Failure error ->
            Element.text error
    )
        |> Element.layout []



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
