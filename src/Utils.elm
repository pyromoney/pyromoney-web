module Utils exposing (httpErrorString, send)

{-| Assorted helper functions.
-}

import Http
import Task


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
