module Utils exposing (httpErrorString, midnight, send)

{-| Assorted helper functions.
-}

import Date exposing (Date)
import Http
import Task
import Time exposing (Month(..), utc)
import Time.Extra as Time


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


midnight : Time.Zone -> Date -> Time.Posix
midnight timezone date =
    Time.Parts (Date.year date) (Date.month date) (Date.day date) 0 0 0 0 |> Time.partsToPosix timezone
