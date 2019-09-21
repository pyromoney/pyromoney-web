module Routes exposing (Route(..), accountPath, indexPath, parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type alias AccountId =
    String


type Route
    = IndexRoute
    | AccountRoute AccountId
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map IndexRoute top
        , map AccountRoute (s "accounts" </> string)
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


pathFor : Route -> String
pathFor route =
    case route of
        IndexRoute ->
            "/"

        AccountRoute id ->
            "/accounts/" ++ id

        NotFoundRoute ->
            "/404"


indexPath : String
indexPath =
    pathFor IndexRoute


accountPath : AccountId -> String
accountPath id =
    pathFor (AccountRoute id)
