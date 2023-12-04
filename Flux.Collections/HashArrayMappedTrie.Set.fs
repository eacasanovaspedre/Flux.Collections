namespace rec Flux.Collections

open System.Collections.Generic
open Flux.Collections.Internals.Hamt
open Flux.Collections.Internals.HamtSet

type HamtSet<'E when 'E: equality> =
    private
    | Empty of Comparer: IEqualityComparer<'E>
    | Trie of Root: Node<'E> * Count: int * Comparer: IEqualityComparer<'E>

    interface IEnumerable<'E> with
        member _.GetEnumerator() : IEnumerator<'E> = Seq.empty.GetEnumerator ()
        member this.GetEnumerator() : System.Collections.IEnumerator = Enumerable.enumerator this

module HamtSet =

    (*
    For now these constructors cannot be used because the set operations (union, difference, intersection) would be much
    more expensive, since two sets could have different equality comparers and the hashes would not necessarily be
    identical for the same element, locating those elements on different path in the trie for different sets
    
    let emptyStructural<'E when 'E: equality> =
        HamtSet<'E>.Empty KeyEqualityComparison.selectStructuralEqualityComparer

    let emptyWith comparer = HamtSet<'E>.Empty comparer
    *)
    let empty<'E when 'E: equality> =
        HamtSet<'E>.Empty KeyEqualityComparison.selectNonStructuralEqualityComparer

    let isEmpty set =
        match set with
        | Empty _ -> true
        | _ -> false

    let count set =
        match set with
        | Empty _ -> 0
        | Trie (_, count, _) -> count

    let inline size set = count set

    let put element set =
        match set with
        | Empty eqComparer -> Trie (Leaf element, 1, eqComparer)
        | Trie (root, count, eqComparer) ->
            let hash = Key.uhash eqComparer element
            let prefix = Prefix.fullPrefixFromHash hash

            match Node.put eqComparer element hash prefix root with
            | Node.PutOutcome.AddedTo newRoot -> Trie (newRoot, count + 1, eqComparer)
            | Node.PutOutcome.AlreadyExisted -> set

    let inline add element set = put element set

    let contains element set =
        match set with
        | Empty _ -> false
        | Trie (root, _, eqComparer) ->
            let hash = Key.uhash eqComparer element
            Node.contains eqComparer element hash (Prefix.fullPrefixFromHash hash) root

    let remove key set =
        match set with
        | Empty _ -> set
        | Trie (root, count, eqComparer) ->
            let hash = Key.uhash eqComparer key

            match Node.remove eqComparer key hash (Prefix.fullPrefixFromHash hash) root with
            | Node.RemoveOutcome.NotFound -> set
            | Node.RemoveOutcome.RemovedFrom node -> Trie (node, count - 1, eqComparer)
            | Node.RemoveOutcome.NothingLeft -> Empty eqComparer

    let filter predicate set =
        match set with
        | Empty _ -> set
        | Trie (root, count, eqComparer) ->
            match Node.filter predicate root with
            | Node.NothingRemoved -> set
            | Node.AllRemoved _ -> Empty eqComparer
            | Node.NodeLeft (node, removedCount) -> Trie (node, count - removedCount, eqComparer)

    let exists predicate set =
        match set with
        | Empty _ -> false
        | Trie (root, _, _) -> Node.exists predicate root

    let forall predicate set =
        match set with
        | Empty _ -> true
        | Trie (root, _, _) -> Node.forall predicate root

    let iter action set =
        match set with
        | Empty _ -> ()
        | Trie (root, _, _) -> Node.iter action root

    let map mapper set =
        match set with
        | Empty _ -> set
        | Trie (root, count, eqComparer) -> Trie (Node.map mapper root, count, eqComparer)

    let partition predicate set =
        match set with
        | Empty eqComparer -> set, Empty eqComparer
        | Trie (root, count, eqComparer) ->
            match Node.partition predicate root with
            | Node.NodeAccepted -> set, Empty eqComparer
            | Node.NodeRejected _ -> Empty eqComparer, set
            | Node.NodeSplit (acceptedPart, rejectedPart, rejectedEntryCount) ->
                Trie (acceptedPart, count - rejectedEntryCount, eqComparer),
                Trie (rejectedPart, rejectedEntryCount, eqComparer)

    /// Folds over the bindings in the Hamt.
    /// Given that this an unordered collection, fold might not return the expected results if some order of application
    /// is expected. The function 'foldBack' is not provided for the same reason.
    let fold folder initialState set =
        match set with
        | Empty _ -> initialState
        | Trie (root, _, _) -> Node.fold folder initialState root

    let maybePick picker set =
        match set with
        | Empty _ -> None
        | Trie (root, _, _) -> Node.maybePick picker root

    let pick picker set =
        match set with
        | Empty _ ->
            EntryNotFoundException (EntryNotFound $"Cannot 'pick' an item from an Empty Hamt.")
            |> raise
        | Trie (root, _, _) ->
            match Node.maybePick picker root with
            | Some x -> x
            | None ->
                EntryNotFoundException (
                    EntryNotFound $"The supplied picker returned 'None' for every entry in this Hamt."
                )
                |> raise

    let union set1 set2 =
        match set1, set2 with
        | Empty eqComparer, Empty _ -> Empty eqComparer
        | Empty _, another
        | another, Empty _ -> another
        | Trie (root1, _, eqComparer), Trie (root2, _, _) ->
            let struct (newRoot, count) =
                Node.SameComparer.union eqComparer SimplePrefix.RootLength root1 root2

            Trie (newRoot, count, eqComparer)
            
    let intersection set1 set2 =
        match set1, set2 with
        | Empty eqComparer, _
        | _, Empty eqComparer -> Empty eqComparer
        | Trie (root1, _, eqComparer), Trie (root2, _, _) ->
            match Node.SameComparer.intersection eqComparer SimplePrefix.RootLength root1 root2 with
            | ValueSome (newRoot, count) -> Trie (newRoot, count, eqComparer)
            | ValueNone -> Empty eqComparer

    let toSeq set =
        match set with
        | Empty _ -> Seq.empty
        | Trie (root, _, _) -> Node.toSeq root
