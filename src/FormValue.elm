module FormValue exposing (FormValue(..), fromFloat, fromString, toFloat, toString)


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
