module Tree exposing (Multitree, Node(..), empty, toList, toTree)


type Node a
    = Node a (List (Node a))


type alias Multitree a =
    List (Node a)


subtree : (a -> a -> Bool) -> List a -> a -> Node a
subtree childrenFilter allItems parentItem =
    let
        childNodes =
            allItems
                |> List.filter (childrenFilter parentItem)
                |> List.map (subtree childrenFilter allItems)
    in
    Node parentItem childNodes


toTree : (a -> Bool) -> (a -> a -> Bool) -> List a -> Multitree a
toTree rootsFilter childrenFilter items =
    let
        roots =
            List.filter rootsFilter items
    in
    List.map (subtree childrenFilter items) roots


empty : Multitree a
empty =
    []


nodeToList : Node a -> List a
nodeToList (Node val children) =
    val :: List.concatMap nodeToList children


toList : Multitree a -> List a
toList nodes =
    List.concatMap nodeToList nodes
