namespace rec Flux.Collections

open Flux.Collections.Internals.Hamt
open System.Collections.Generic

type Hamt<'K, 'V when 'K: equality> =
    private
    | Empty of Comparer: IEqualityComparer<'K> voption
    | Trie of Root: Node<'K, 'V> * Count: int * Comparer: IEqualityComparer<'K> voption

    interface IReadOnlyDictionary<'K, 'V> with

        member this.GetEnumerator() : IEnumerator<KeyValuePair<'K, 'V>> =
            this
            |> Hamt.toSeq
            |> Seq.map (fun entry -> KeyValuePair(KVEntry.key entry, KVEntry.value entry))
            |> Enumerable.enumerator

        member this.GetEnumerator() : System.Collections.IEnumerator =
            upcast (Enumerable.enumerator (this :> IEnumerable<_>))

        member this.ContainsKey key = Hamt.containsKey key this

        member this.Count = Hamt.count this

        member this.Item
            with get key = Hamt.find key this

        member this.Keys = Hamt.keys this

        member this.TryGetValue(key: 'K, value: byref<'V>) : bool =
            match Hamt.maybeFind key this with
            | Some v ->
                value <- v
                true
            | None -> false

        member this.Values =
            this
            |> Hamt.keys
            |> Seq.map (fun k -> Hamt.find k this)

module Hamt =

    let empty<'K, 'V when 'K: equality> = 
        if typeof<'K>.IsValueType then
            Hamt<'K, 'V>.Empty ValueOption.None
        else
            Hamt<'K, 'V>.Empty (ValueOption.Some EqualityComparer.Default)

    let emptyWith comparer = 
        Empty (ValueOption.Some comparer)

    let isEmpty hamt =
        match hamt with
        | Empty _ -> true
        | _ -> false

    let count =
        function
        | Empty _ -> 0
        | Trie (_, count, _) -> count

    let add key value =
        function
        | Empty eqComparer -> Trie(Leaf(KVEntry(key, value)), 1, eqComparer)
        | Trie (root, count, eqComparerOpt) ->
            let newNode, outcome =
                match eqComparerOpt with
                | ValueSome eqComparer ->
                    let hash = Key.uhash eqComparer key
                    Node.add eqComparer (KVEntry(key, value)) hash (Prefix.fullPrefixFromHash hash) root
                | ValueNone ->
                    let hash = Key.uhash' key
                    Node.add' (KVEntry(key, value)) hash (Prefix.fullPrefixFromHash hash) root
            match newNode, outcome with
            | newRoot, Added -> Trie(newRoot, count + 1, eqComparerOpt)
            | newRoot, Replaced -> Trie(newRoot, count, eqComparerOpt)

    let containsKey key =
        function
        | Empty _ -> false
        | Trie (root, _, ValueSome eqComparer) ->
            let hash = Key.uhash eqComparer key
            Node.containsKey eqComparer key hash (Prefix.fullPrefixFromHash hash) root
        | Trie (root, _, ValueNone) ->
            let hash = Key.uhash' key
            Node.containsKey' key hash (Prefix.fullPrefixFromHash hash) root

    let maybeFind key =
        function
        | Empty _ -> None
        | Trie (root, _, ValueSome eqComparer) ->
            let hash = Key.uhash eqComparer key
            Node.maybeFind eqComparer key hash (Prefix.fullPrefixFromHash hash) root
        | Trie (root, _, ValueNone) ->
            let hash = Key.uhash' key
            Node.maybeFind' key hash (Prefix.fullPrefixFromHash hash) root

    let find key =
        function
        | Empty _ -> KeyNotFoundException.throw key
        | Trie (root, _, ValueSome eqComparer) ->
            let hash = Key.uhash eqComparer key
            Node.find eqComparer key hash (Prefix.fullPrefixFromHash hash) root
        | Trie (root, _, ValueNone) ->
            let hash = Key.uhash' key
            Node.find' key hash (Prefix.fullPrefixFromHash hash) root

    let remove key hamt =
        match hamt with
        | Empty _ -> hamt
        | Trie (root, count, eqComparerOpt) ->
            let outcome =
                match eqComparerOpt with
                | ValueSome eqComparer ->
                    let hash = Key.uhash eqComparer key
                    Node.remove eqComparer key hash (Prefix.fullPrefixFromHash hash) root
                | ValueNone ->
                    let hash = Key.uhash' key
                    Node.remove' key hash (Prefix.fullPrefixFromHash hash) root
            match outcome with
            | NotFound -> hamt
            | Removed node -> Trie(node, count - 1, eqComparerOpt)
            | NothingLeft -> Empty eqComparerOpt
        

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

    let findAndSet k f h = //this group of functions can be optimized by doing it in the node level
        let x = find k h
        add k (f x) h

    let maybeFindAndSet k f h =
        match maybeFind k h with
        | Some value -> add k (f value) h |> Some
        | None -> None

    let findAndSetSafe k f h =
        match maybeFind k h with
        | Some value -> add k (f value) h
        | None -> h

    let findAndRemove k h = //this group of functions can be optimized by doing it in the node level
        let v = find k h
        v, remove k h

    let maybeFindAndRemove k h =
        match maybeFind k h with
        | Some value -> (value, remove value h) |> Some
        | None -> None

    let findAndRemoveSafe k h =
        match maybeFind k h with
        | Some value -> (Some value, remove value h)
        | None -> None, h

    module Lens =

        let inline _key k = find k, add k

        let inline _keyMaybe k =
            maybeFind k,
            fun x h ->
                match x with
                | Some v -> add k v h
                | None -> remove k h
