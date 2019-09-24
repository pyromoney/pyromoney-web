module FormValue exposing (FormValue(..), fromFloat, fromString, parseFloat, toFloat, toString)


type FormValue a
    = Invalid String
    | Valid a String


toString : FormValue a -> String
toString value =
    case value of
        Invalid str ->
            str

        Valid _ str ->
            str


fromString : String -> FormValue String
fromString str =
    Valid str str


fromFloat : Float -> FormValue Float
fromFloat f =
    Valid f (String.fromFloat f)


toFloat : FormValue Float -> Maybe Float
toFloat value =
    case value of
        Invalid _ ->
            Nothing

        Valid f _ ->
            Just f


parseFloat : String -> FormValue Float
parseFloat str =
    String.toFloat str
        |> Maybe.map (\f -> Valid f str)
        |> Maybe.withDefault (Invalid str)
