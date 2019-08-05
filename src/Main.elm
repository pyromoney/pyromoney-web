module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (..)
import Html
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Utils



---- MODEL ----


type Msg
    = RequestAccounts
    | ReceiveAccounts (Result Http.Error (List Account))


type alias ServerUrl =
    String


type alias Account =
    { id : String
    , parentId : Maybe String
    , name : String
    , type_ : String
    , currency : String
    , hidden : Bool
    , virtual : Bool
    }


type TreeNode a
    = TreeNode a (List (TreeNode a))


type alias Tree a =
    List (TreeNode a)


type alias Model =
    { serverUrl : ServerUrl
    , accounts : List Account
    , accountsTree : Tree Account
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


subtree : List Account -> Account -> TreeNode Account
subtree allAccounts parent =
    let
        filterChildren =
            \a ->
                case a.parentId of
                    Just id ->
                        id == parent.id

                    Nothing ->
                        False

        childNodes =
            allAccounts
                |> List.filter filterChildren
                |> List.map (subtree allAccounts)
    in
    TreeNode parent childNodes


toAccountTree : List Account -> Tree Account
toAccountTree accounts =
    let
        isRoot account =
            case account.parentId of
                Just _ ->
                    False

                Nothing ->
                    True

        roots =
            List.filter isRoot accounts
    in
    List.map (subtree accounts) roots



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestAccounts ->
            ( model, fetchAccounts model.serverUrl )

        ReceiveAccounts (Ok accounts) ->
            ( { model | accounts = accounts, accountsTree = toAccountTree accounts }, Cmd.none )

        ReceiveAccounts (Err error) ->
            ( { model | lastError = Utils.httpErrorString error }, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    layout [] <|
        column
            [ width fill ]
        <|
            [ text model.lastError ]
                ++ viewAccounts 0 model.accountsTree


viewAccounts : Int -> Tree Account -> List (Element Msg)
viewAccounts depth accounts =
    List.concatMap (viewAccount depth) accounts


viewAccount : Int -> TreeNode Account -> List (Element Msg)
viewAccount depth node =
    let
        (TreeNode account childNodes) =
            node

        padding =
            [ paddingEach { left = 20 * depth, right = 0, top = 0, bottom = 0 } ]
    in
    [ row [ width fill ]
        [ column [ width (fillPortion 6) ] [ el padding (text account.name) ]
        , column [ width (fillPortion 2) ] [ el [] (text account.type_) ]
        , column [ width (fillPortion 2) ] [ el [] (text account.currency) ]
        ]
    ]
        ++ viewAccounts (depth + 1) childNodes



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
