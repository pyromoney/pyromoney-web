module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Data.Account exposing (Account, decodeAccount)
import Dict exposing (Dict)
import Element
import Html
import Http
import Json.Decode as Decode
import Loadable exposing (Loadable(..))
import Page.Account
import Page.Index
import Routes exposing (Route(..))
import Tree
import Url exposing (Url)
import Utils



---- MODEL ----


type Page
    = IndexPage Page.Index.Model
    | AccountPage Page.Account.Model


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | ReceiveAccounts (Result Http.Error (List Account))
    | IndexMsg Page.Index.Msg
    | AccountMsg Page.Account.Msg


type alias ServerUrl =
    String


type alias AccountId =
    String


type alias AppState =
    { accountsDict : Dict AccountId Account
    , accountsTree : Tree.Multitree Account
    }


type alias Config =
    { navKey : Nav.Key
    , serverUrl : ServerUrl
    }


type alias State =
    Loadable
        { appState : AppState
        , currentPage : Page
        }
        Route


type alias Model =
    { config : Config
    , state : State
    }


type alias Flags =
    { serverUrl : ServerUrl
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { config =
            { navKey = navKey
            , serverUrl = flags.serverUrl
            }
      , state =
            Loading (Routes.parseUrl url)
      }
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
update msg ({ config, state } as model) =
    case state of
        Loaded ({ appState, currentPage } as loadedModel) ->
            case ( msg, currentPage ) of
                ( OnUrlRequest urlRequest, _ ) ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model
                            , Nav.pushUrl config.navKey (Url.toString url)
                            )

                        Browser.External url ->
                            ( model
                            , Nav.load url
                            )

                ( OnUrlChange url, _ ) ->
                    let
                        newRoute =
                            Routes.parseUrl url
                    in
                    resolveRoute config appState newRoute

                ( IndexMsg (Page.Index.OpenAccount account), IndexPage _ ) ->
                    let
                        ( newPageModel, newPageCmd ) =
                            Page.Account.init config appState account

                        newState =
                            Loaded { loadedModel | currentPage = AccountPage newPageModel }
                    in
                    ( { config = config, state = newState }
                    , Cmd.map AccountMsg newPageCmd
                    )

                ( AccountMsg pageMsg, AccountPage pageModel ) ->
                    let
                        ( newPageModel, newPageCmd ) =
                            Page.Account.update config appState pageMsg pageModel

                        newState =
                            Loaded { loadedModel | currentPage = AccountPage newPageModel }
                    in
                    ( { config = config, state = newState }
                    , Cmd.map AccountMsg newPageCmd
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        Loading route ->
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
                            { accountsDict = accountsDict
                            , accountsTree = accountsTree
                            }
                    in
                    resolveRoute config newAppState route

                ReceiveAccounts (Err error) ->
                    let
                        newState =
                            Failure <| Utils.httpErrorString error
                    in
                    ( { config = config, state = newState }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Failure _ ->
            ( model, Cmd.none )


resolveRoute : Config -> AppState -> Route -> ( Model, Cmd Msg )
resolveRoute config appState route =
    case route of
        IndexRoute ->
            let
                ( pageModel, cmd ) =
                    Page.Index.init

                newState =
                    Loaded { appState = appState, currentPage = IndexPage pageModel }
            in
            ( { config = config, state = newState }, Cmd.map IndexMsg cmd )

        AccountRoute accountId ->
            appState.accountsDict
                |> Dict.get accountId
                |> Maybe.map
                    (\account ->
                        let
                            ( pageModel, cmd ) =
                                Page.Account.init config appState account

                            newState =
                                Loaded { appState = appState, currentPage = AccountPage pageModel }
                        in
                        ( { config = config, state = newState }, Cmd.map AccountMsg cmd )
                    )
                |> Maybe.withDefault ( { config = config, state = Failure "No such account" }, Cmd.none )

        NotFoundRoute ->
            ( { config = config
              , state = Failure "Not found"
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Pyromoney"
    , body = [ viewCurrentPage model ]
    }


viewCurrentPage : Model -> Html.Html Msg
viewCurrentPage { state } =
    (case state of
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
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
