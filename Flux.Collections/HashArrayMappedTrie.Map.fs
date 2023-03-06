namespace rec Flux.Collections

open System.Collections.Generic
open Flux.Collections.Internals.Hamt
open Flux.Collections.Internals.HamtMap
open Flux.Collections.Internals.HamtMap.Node

/// Hash Array Mapped Trie
type HamtMap<'K, 'V when 'K: equality> =
    private
    | Empty of Comparer: IEqualityComparer<'K>
    | Trie of Root: Node<'K, 'V> * Count: int * Comparer: IEqualityComparer<'K>

    interface IEnumerable<KeyValuePair<'K, 'V>> with
        member this.GetEnumerator() : IEnumerator<KeyValuePair<'K, 'V>> =
            this |> HamtMap.toSeq |> Enumerable.enumerator

        member this.GetEnumerator() : System.Collections.IEnumerator =
            Enumerable.enumerator (this :> IEnumerable<_>)

    interface IReadOnlyCollection<KeyValuePair<'K, 'V>> with
        member this.Count = HamtMap.count this

    interface IReadOnlyDictionary<'K, 'V> with

        member this.ContainsKey key = HamtMap.containsKey key this

        member this.Item
            with get key = HamtMap.find key this

        member this.Keys = HamtMap.keys this

        member this.TryGetValue(key: 'K, value: byref<'V>) : bool =
            match HamtMap.maybeFind key this with
            | Some v ->
                value <- v
                true
            | None -> false

        member this.Values = this |> HamtMap.keys |> Seq.map (fun k -> HamtMap.find k this)

module HamtMap =

    module private Helper =

        let ofSeq emptyHamt (entries: ('K * 'V) seq) =
            let rec loop (enumerator: _ IEnumerator) hamt =
                if enumerator.MoveNext () then
                    let k, v = enumerator.Current
                    loop enumerator (put k v hamt)
                else
                    hamt

            loop (entries.GetEnumerator ()) emptyHamt

    let emptyStructural<'K, 'V when 'K: equality> =
        HamtMap<'K, 'V>.Empty KeyEqualityComparison.selectStructuralEqualityComparer

    let empty<'K, 'V when 'K: equality> =
        HamtMap<'K, 'V>.Empty KeyEqualityComparison.selectNonStructuralEqualityComparer<'K>

    let emptyWith comparer = HamtMap<'K, 'V>.Empty comparer

    let isEmpty hamt =
        match hamt with
        | Empty _ -> true
        | _ -> false

    let count =
        function
        | Empty _ -> 0
        | Trie (_, count, _) -> count
        
    let inline size hamt = count hamt

    let put key value hamt =
        match hamt with
        | Empty eqComparer -> Trie (Leaf (KeyValuePair (key, value)), 1, eqComparer)
        | Trie (root, count, eqComparer) ->
            let hash = Key.uhash eqComparer key
            let prefix = Prefix.fullPrefixFromHash hash

            match Node.put eqComparer (KeyValuePair (key, value)) hash prefix root with
            | PutOutcome.AddedTo newRoot -> Trie (newRoot, count + 1, eqComparer)
            | PutOutcome.ReplacedIn newRoot -> Trie (newRoot, count, eqComparer)

    let inline add key value hamt = put key value hamt

    let containsKey key hamt =
        match hamt with
        | Empty _ -> false
        | Trie (root, _, eqComparer) ->
            let hash = Key.uhash eqComparer key
            Node.containsKey eqComparer key hash (Prefix.fullPrefixFromHash hash) root

    let maybeFind key hamt =
        match hamt with
        | Empty _ -> None
        | Trie (root, _, eqComparer) ->
            let hash = Key.uhash eqComparer key
            Node.maybeFind eqComparer key hash (Prefix.fullPrefixFromHash hash) root

    let find key hamt=
        match hamt with
        | Empty _ -> KeyNotFoundException.throw key
        | Trie (root, _, eqComparer) ->
            let hash = Key.uhash eqComparer key
            Node.find eqComparer key hash (Prefix.fullPrefixFromHash hash) root

    let remove key hamt =
        match hamt with
        | Empty _ -> hamt
        | Trie (root, count, eqComparer) ->
            let hash = Key.uhash eqComparer key

            match Node.remove eqComparer key hash (Prefix.fullPrefixFromHash hash) root with
            | RemoveOutcome.NotFound -> hamt
            | RemoveOutcome.RemovedFrom node -> Trie (node, count - 1, eqComparer)
            | RemoveOutcome.NothingLeft -> Empty eqComparer

    let filter predicate hamt =
        match hamt with
        | Empty _ -> hamt
        | Trie (root, count, eqComparer) ->
            match Node.filter predicate root with
            | NothingRemoved -> hamt
            | AllRemoved _ -> Empty eqComparer
            | NodeLeft (node, removedCount) -> Trie (node, count - removedCount, eqComparer)

    let exists predicate hamt =
        match hamt with
        | Empty _ -> false
        | Trie (root, _, _) -> Node.exists predicate root

    let forall predicate hamt =
        match hamt with
        | Empty _ -> true
        | Trie (root, _, _) -> Node.forall predicate root

    let iter action hamt =
        match hamt with
        | Empty _ -> ()
        | Trie (root, _, _) -> Node.iter action root

    let map mapper hamt =
        match hamt with
        | Empty _ -> hamt
        | Trie (root, count, eqComparer) -> Trie (Node.map mapper root, count, eqComparer)

    let partition predicate hamt =
        match hamt with
        | Empty eqComparer -> hamt, Empty eqComparer
        | Trie (root, count, eqComparer) ->
            match Node.partition predicate root with
            | NodeAccepted -> hamt, Empty eqComparer
            | NodeRejected _ -> Empty eqComparer, hamt
            | NodeSplit (acceptedPart, rejectedPart, rejectedEntryCount) ->
                Trie (acceptedPart, count - rejectedEntryCount, eqComparer),
                Trie (rejectedPart, rejectedEntryCount, eqComparer)

    /// Folds over the bindings in the Hamt.
    /// Given that this an unordered collection, fold might not return the expected results if some order of application
    /// is expected. The function 'foldBack' is not provided for the same reason.
    let fold folder initialState hamt =
        match hamt with
        | Empty _ -> initialState
        | Trie (root, _, _) -> Node.fold folder initialState root

    let maybePick picker hamt =
        match hamt with
        | Empty _ -> None
        | Trie (root, _, _) -> Node.maybePick picker root

    let pick picker hamt =
        match hamt with
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

    let change key changer hamt =
        match hamt with
        | Empty eqComparer ->
            match changer None with
            | Some value -> Trie (Leaf (KeyValuePair (key, value)), 1, eqComparer)
            | None -> hamt
        | Trie (root, count, eqComparer) ->
            let hash = Key.uhash eqComparer key
            let prefix = Prefix.fullPrefixFromHash hash

            match Node.change changer eqComparer key hash prefix root with
            | AddedTo newRoot -> Trie (newRoot, count + 1, eqComparer)
            | ReplacedIn newRoot -> Trie (newRoot, count, eqComparer)
            | NothingChanged -> hamt
            | RemovedFrom newRoot -> Trie (newRoot, count - 1, eqComparer)
            | NothingLeft -> Empty eqComparer

    let toSeq hamt =
        match hamt with
        | Empty _ -> Seq.empty
        | Trie (root, _, _) -> Node.toSeq root

    let toSeqOfPairs hamt =
        match hamt with
        | Empty _ -> Seq.empty
        | Trie (root, _, _) -> Node.toSeqOfPairs root

    let keys hamt =
        match hamt with
        | Empty _ -> Seq.empty
        | Trie (root, _, _) -> Node.keys root

    let ofSeq entries = Helper.ofSeq empty entries

    let ofSeqStructural entries = Helper.ofSeq emptyStructural entries

    let ofSeqWith eqComparer entries =
        Helper.ofSeq (emptyWith eqComparer) entries

    module Lens =

        let inline _key k = find k, add k

        let inline _keyMaybe k =
            maybeFind k,
            fun x h ->
                match x with
                | Some v -> add k v h
                | None -> remove k h
