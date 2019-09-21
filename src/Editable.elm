module Editable exposing
    ( Editable(..), State(..)
    , fromNew, fromSaved, fromEditing
    , edit, cancel, save, targetState
    , target, mapTarget
    , map, mapState
    , isNew, isEditing, isSaved
    )

{-| This module implements editing modes for any content.


# Types

@docs Editable, State


# Construction

@docs fromNew, fromSaved, fromEditing


# State transitions

@docs edit, cancel, save, targetState


# Editing target

@docs target, mapTarget


# General mapping functions

@docs map, mapState


# Predicates

@docs isNew, isEditing, isSaved

-}


{-| Supported states:

  - `New` - a new unsaved content being edited,
  - `Editing a` - existing content being edited along with intermediate
    copy of the content to make modification to until saved,
  - `Saved` - saved, unmodified content.

-}
type State a
    = New
    | Editing a
    | Saved


{-| Editable content along with its editing state.
-}
type Editable a
    = Editable (State a) a



-- TODO: Missing docstrings.


fromNew : a -> Editable a
fromNew =
    Editable New


fromSaved : a -> Editable a
fromSaved =
    Editable Saved


fromEditing : a -> Editable a
fromEditing p =
    Editable (Editing p) p


edit : Editable a -> Editable a
edit ((Editable _ content) as editable) =
    editable
        |> mapState (always <| Editing content)


save : Editable a -> Editable a
save editable =
    editable
        |> map (always <| target editable)
        |> mapState (always <| Saved)


cancel : Editable a -> Editable a
cancel =
    mapState (always Saved)


{-| Extract editing target.

In Editing state intermediate content is used so as to not affect the displayed
content until user commits the changes.

-}
target : Editable a -> a
target (Editable state content) =
    case state of
        New ->
            content

        Editing intermediateContent ->
            intermediateContent

        Saved ->
            content


targetState : Editable a -> State a
targetState (Editable state _) =
    state


{-| Map over editing target.

Analoguously to the target function, transforms intermediate content in Editing
state, regular content otherwise. Makes it possible to modify Saved Page.

-}
mapTarget : (a -> a) -> Editable a -> Editable a
mapTarget f (Editable state content) =
    case state of
        New ->
            Editable New (f content)

        Editing intermediateContent ->
            Editable (Editing (f intermediateContent)) content

        Saved ->
            Editable Saved (f content)


map : (a -> a) -> Editable a -> Editable a
map f (Editable state content) =
    Editable state (f content)


mapState : (State a -> State a) -> Editable a -> Editable a
mapState f (Editable state content) =
    Editable (f state) content


isNew : Editable a -> Bool
isNew (Editable state _) =
    case state of
        New ->
            True

        _ ->
            False


isEditing : Editable a -> Bool
isEditing (Editable state _) =
    case state of
        Editing _ ->
            True

        _ ->
            False


isSaved : Editable a -> Bool
isSaved (Editable state _) =
    case state of
        Saved ->
            True

        _ ->
            False
