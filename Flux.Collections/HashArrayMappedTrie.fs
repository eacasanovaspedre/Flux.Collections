namespace rec Flux.Collections

open Flux
open Flux.Bit
open System.Collections.Generic
open System.ComponentModel

#if X64
type BitmapHolder = UInt64W
#else
type BitmapHolder = UInt32W
#endif

type private Node<'K, 'T> =
    | Leaf of KVEntry<'K, 'T>
    | LeafWithCollisions of KVEntry<'K, 'T> list
    | Branch of Bitmap<BitmapHolder> * Node<'K, 'T> array

[<Struct>]
type private Prefix = Prefix of bits: uint32 * length: int<bit>

type private AddOutcome =
    | Added
    | Replaced

type private RemoveOutcome<'K, 'T> =
    | NothingLeft
    | Removed of Node<'K, 'T>
    | NotFound

[<Struct>]
type KeyNotFound<'K> = KeyNotFound of 'K

exception KeyNotFoundException of Message: string * Key: obj KeyNotFound

type Hamt<'K, 'V when 'K: equality> =
    private
    | Empty
    | Trie of root: Node<'K, 'V> * count: int

    interface IReadOnlyDictionary<'K, 'V> with

        member this.GetEnumerator(): IEnumerator<KeyValuePair<'K, 'V>> =
            this
            |> Hamt.toSeq
            |> Seq.map (fun entry -> KeyValuePair(KVEntry.key entry, KVEntry.value entry))
            |> Enumerable.enumerator

        member this.GetEnumerator(): System.Collections.IEnumerator =
            upcast (Enumerable.enumerator (this :> IEnumerable<_>))

        member this.ContainsKey key = Hamt.containsKey key this

        member this.Count = Hamt.count this

        member this.Item
            with get key = Hamt.find key this

        member this.Keys = Hamt.keys this

        member this.TryGetValue(key: 'K, value: byref<'V>): bool =
            match Hamt.maybeFind key this with
            | Some v -> value <- v; true
            | None -> false

        member this.Values = this |> Hamt.keys |> Seq.map (fun k -> Hamt.find k this)

module Hamt =
    open Prefix
    open KVEntry

    module private Prefix =

        [<Literal>]
        let HashSize = 32<bit>

        let inline uhash key =
            key
            |> hash
            |> uint32

        let inline currentLevelPrefixFromHash currentLength hash =
            Prefix(rshift hash (HashSize - currentLength), currentLength)

        let inline fullPrefixFromHash hash = currentLevelPrefixFromHash HashSize hash

        let inline prefixBits (Prefix(bits, _)) = bits

        let inline length (Prefix(_, length)) = length

    module private CollisionHelpers =

        let collisionHash entries =
            entries
            |> List.head
            |> key
            |> uhash

        let insertOrReplace entry entries =
            entries
            |> List.tryFindIndex (fun e -> key e = key entry)
            |> Option.map (fun index ->
                match List.splitAt index entries with
                | before, _ :: after -> before @ entry :: after, Replaced
                | _ -> failwith "Never")
            |> Option.defaultWith (fun () -> entry :: entries, Added)

        let removeIfExists targetKey entries =
            entries
            |> List.tryFindIndex (fun e -> key e = targetKey)
            |> Option.map (fun index ->
                match List.splitAt index entries with
                | before, _ :: after -> before @ after
                | _ -> failwith "Never")
            |> Option.defaultValue entries

    module private Node =

        open CollisionHelpers
        open Bitmap

        let inline childBitIndex prefix = (prefixBits prefix) &&& bitIndexMask<BitmapHolder>() |> asBits

        let inline containsChild childBitIndex bitmap = Bitmap.isBitOn childBitIndex bitmap

        let childArrayIndex childBitIndex bitmap =
            bitmap
            |> Bitmap.onlyLowerBits childBitIndex
            |> Bitmap.countBitsOn
            |> int

        let inline nextLayerPrefix (Prefix(bits, length)) =
            let shift = min length (bitIndexBits<BitmapHolder>())
            Prefix(rshift bits shift, length - shift)

        let rec add entry entryHash prefix node =
            match node with
            | Leaf oldEntry when key entry = key oldEntry -> Leaf entry, Replaced
            | Leaf oldEntry ->
                let oldHash =
                    oldEntry
                    |> key
                    |> uhash
                if entryHash = oldHash then
                    LeafWithCollisions [ entry; oldEntry ], AddOutcome.Added
                else
                    let oldPrefix = currentLevelPrefixFromHash (length prefix) oldHash
                    Branch(Bitmap.bit (childBitIndex oldPrefix), [| node |]) |> add entry entryHash prefix
            | LeafWithCollisions entries ->
                let collisionHash = collisionHash entries
                if entryHash = collisionHash then
                    let (newEntries, outcome) = insertOrReplace entry entries
                    LeafWithCollisions newEntries, outcome
                else
                    let collisionPrefix = currentLevelPrefixFromHash (length prefix) collisionHash
                    let collisionBitIndex = childBitIndex collisionPrefix
                    Branch(Bitmap.bit collisionBitIndex, [| node |]) |> add entry entryHash prefix
            | Branch(bitmap, children) ->
                let bitIndex = childBitIndex prefix
                let arrayIndex = childArrayIndex bitIndex bitmap
                if containsChild bitIndex bitmap then
                    let (newChild, outcome) = add entry entryHash (nextLayerPrefix prefix) children.[arrayIndex]
                    Branch(bitmap, Array.put newChild arrayIndex children), outcome
                else
                    Branch(setBit bitIndex bitmap, Array.insert (Leaf entry) arrayIndex children), Added

        let rec containsKey targetKey targetHash prefix =
            function
            | Leaf(KVEntry(key, _)) when key = targetKey -> true
            | Leaf _ -> false
            | LeafWithCollisions entries when collisionHash entries = targetHash ->
                List.exists (fun (KVEntry(key, _)) -> key = targetKey) entries
            | LeafWithCollisions _ -> false
            | Branch(bitmap, children) ->
                let bitIndex = childBitIndex prefix
                if containsChild bitIndex bitmap then
                    containsKey targetKey targetHash (nextLayerPrefix prefix)
                        children.[childArrayIndex bitIndex bitmap]
                else
                    false

        let rec maybeFind targetKey targetHash prefix node =
            match node with
            | Leaf(KVEntry(key, value)) when key = targetKey -> Some value
            | Leaf _ -> None
            | LeafWithCollisions entries when collisionHash entries = targetHash ->
                List.tryFind (fun entry -> key entry = targetKey) entries |> Option.map value
            | LeafWithCollisions _ -> None
            | Branch(bitmap, children) ->
                let bitIndex = childBitIndex prefix
                if containsChild bitIndex bitmap
                then maybeFind targetKey targetHash (nextLayerPrefix prefix) children.[childArrayIndex bitIndex bitmap]
                else None

        let rec remove targetKey targetHash prefix node =
            match node with
            | Leaf(KVEntry(key, _)) when key = targetKey -> NothingLeft
            | Leaf _ -> NotFound
            | LeafWithCollisions entries when collisionHash entries = targetHash ->
                match removeIfExists targetKey entries with
                | newEntries when LanguagePrimitives.PhysicalEquality entries newEntries -> NotFound
                | [ single ] -> Removed(Leaf single)
                | moreThanOne -> Removed(LeafWithCollisions moreThanOne)
            | LeafWithCollisions _ -> NotFound
            | Branch(bitmap, children) ->
                let bitIndex = childBitIndex prefix
                if containsChild bitIndex bitmap then
                    let childArrayIndex = childArrayIndex bitIndex bitmap
                    match remove targetKey targetHash (nextLayerPrefix prefix) (Array.item childArrayIndex children) with
                    | NotFound -> NotFound
                    | Removed child -> Removed(Branch(bitmap, Array.put child childArrayIndex children))
                    | NothingLeft ->
                        let newBitmap = clearBit bitIndex bitmap
                        if Array.isEmpty children
                        then NothingLeft
                        elif Array.length children = 1
                        then Removed(Array.head children)
                        else Removed(Branch(newBitmap, Array.remove childArrayIndex children))
                else
                    NotFound

        let rec toSeq =
            function
            | Leaf entry -> Seq.singleton entry
            | LeafWithCollisions entries -> upcast entries
            | Branch(_, children) -> Seq.collect toSeq children

        let rec toSeqPairs =
            function
            | Leaf entry -> entry |> asPair |> Seq.singleton
            | LeafWithCollisions entries -> entries |> Seq.map asPair
            | Branch(_, children) -> Seq.collect toSeqPairs children

        let rec keys =
            function
            | Leaf entry -> Seq.singleton (key entry)
            | LeafWithCollisions entries -> Seq.map key entries
            | Branch(_, children) -> Seq.collect keys children

    let empty = Empty

    let isEmpty hamt =
        match hamt with
        | Empty -> true
        | _ -> false

    let count =
        function
        | Empty -> 0
        | Trie(_, count) -> count

    let add key value =
        function
        | Empty -> Trie(Leaf(KVEntry(key, value)), 1)
        | Trie(root, count) ->
            let hash = uhash key
            match Node.add (KVEntry(key, value)) hash (fullPrefixFromHash hash) root with
            | newRoot, Added -> Trie(newRoot, count + 1)
            | newRoot, Replaced -> Trie(newRoot, count)

    let containsKey key =
        function
        | Empty -> false
        | Trie(root, _) ->
            let hash = uhash key
            Node.containsKey key hash (fullPrefixFromHash hash) root

    let maybeFind key =
        function
        | Empty -> None
        | Trie(root, _) ->
            let hash = uhash key
            Node.maybeFind key hash (fullPrefixFromHash hash) root

    let find key hamt =
        match maybeFind key hamt with
        | Some value -> value
        | None -> KeyNotFoundException("Key not found in Hamt", KeyNotFound (upcast key)) |> raise

    let remove key hamt =
        match hamt with
        | Empty -> Empty
        | Trie(root, count) ->
            let hash = uhash key
            match Node.remove key hash (fullPrefixFromHash hash) root with
            | NotFound -> hamt
            | Removed node -> Trie(node, count - 1)
            | NothingLeft -> Empty

    let toSeq hamt =
        match hamt with
        | Empty -> Seq.empty
        | Trie(root, _) -> Node.toSeq root

    let toSeqPairs hamt =
        match hamt with
        | Empty -> Seq.empty
        | Trie(root, _) -> Node.toSeqPairs root

    let keys hamt =
        match hamt with
        | Empty -> Seq.empty
        | Trie(root, _) -> Node.keys root

    let findAndSet k f h =
        let x = find k h
        add k (f x) h

    let maybeFindAndSet k f h =
        h
        |> maybeFind k
        |> Option.map (fun x -> add k (f x) h)

    let maybeFindAndSet' k f h =
        h
        |> maybeFind k
        |> Option.map (fun x -> add k (f x) h)
        |> Option.defaultValue h

    let findAndRemove k h =
        let v = Hamt.find k h
        v, Hamt.remove k h

    let maybeFindAndRemove k h =
        Hamt.maybeFind k h
        |> Option.map (fun v -> v, Hamt.remove k h)

    let maybeFindAndRemove' k h =
        Hamt.maybeFind k h
        |> Option.map (fun v -> Some v, Hamt.remove k h)
        |> Option.defaultValue (None, h)
    module Lens =

        let inline _key k = find k, fun v -> add k v

        let inline _keyMaybe k = maybeFind k, fun x h -> match x with | Some v -> add k v h | None -> remove k h
