namespace Flux.Collections

[<Struct>]
type KeyNotFound<'K> = KeyNotFound of 'K

exception KeyNotFoundException of Msg: string * Key: obj KeyNotFound with
    override this.Message = this.Msg

    static member inline throw key =
        KeyNotFoundException("Key not found in Hamt", KeyNotFound(upcast key))
        |> raise

namespace Flux.Collections.Internals

open Flux
open Flux.Collections
open Flux.Bit

//Some functions have an almost identical copy with an ' appended to the name to indicate that it's a version that won't
//receive a comparer as an argument.
//TODO: Consider the possibility of keeping the two versions only on the low level functions, like Key.equals and
//TODO: Key.uhash, receiving the optional comparer and doing there the discrimination.   

module internal Hamt =

    #if X64
    type BitmapHolder = Bitmap64
    #else
    type BitmapHolder = Bitmap32
    #endif

    type Node<'K, 'T> =
        | Leaf of KVEntry<'K, 'T>
        | LeafWithCollisions of KVEntry<'K, 'T> list
        | Branch of Bitmap<BitmapHolder> * Node<'K, 'T> array

    [<Struct>]
    type Prefix = Prefix of bits: uint32 * length: int<bit>

    [<Struct>]
    type AddOutcome =
        | Added
        | Replaced

    type RemoveOutcome<'K, 'T> =
        | NothingLeft
        | Removed of Node<'K, 'T>
        | NotFound

    module Key =
        open System.Collections.Generic
        
        [<Literal>]
        let HashSize = 32<bit>
        
        let inline uhash (eqComparer: IEqualityComparer<_>) key =
            key
            |> eqComparer.GetHashCode
            |> uint32

        let inline uhash' key =
            key
            |> EqualityComparer.Default.GetHashCode
            |> uint32

        let inline equals (eqComparer: IEqualityComparer<_>) k1 k2 =
            eqComparer.Equals (k1, k2)

        let inline equals' k1 k2 =
            EqualityComparer.Default.Equals (k1, k2)

    module Prefix =

        let inline currentLevelPrefixFromHash currentLength hash =
            Prefix (rshift hash (Key.HashSize - currentLength), currentLength)

        let inline fullPrefixFromHash hash =
            currentLevelPrefixFromHash Key.HashSize hash

        let inline prefixBits (Prefix (bits, _)) = bits

        let inline length (Prefix (_, length)) = length

    module private CollisionHelpers =

        let inline collisionHash eqComparer entries =
            entries |> List.head |> KVEntry.key |> Key.uhash eqComparer

        let inline collisionHash' entries =
            entries |> List.head |> KVEntry.key |> Key.uhash'

        let insertOrReplace eqComparer entry entries =
            let rec loop before =
                function
                | KVEntry (key, _) :: after when Key.equals eqComparer key (KVEntry.key entry) -> (List.rev before) @ entry :: after, Replaced
                | x :: xs -> loop (x :: before) xs
                | [] -> entry :: before, Added

            loop [] entries

        let insertOrReplace' entry entries =
            let rec loop before =
                function
                | KVEntry (key, _) :: after when Key.equals' key (KVEntry.key entry) -> (List.rev before) @ entry :: after, Replaced
                | x :: xs -> loop (x :: before) xs
                | [] -> entry :: before, Added

            loop [] entries

        let removeIfExists eqComparer targetKey entries =
            let rec loop before =
                function
                | KVEntry (key, _) :: after when Key.equals eqComparer key targetKey -> (List.rev before) @ after
                | x :: xs -> loop (x :: before) xs
                | [] -> entries

            loop [] entries

        let removeIfExists' targetKey entries =
            let rec loop before =
                function
                | KVEntry (key, _) :: after when Key.equals' key targetKey -> (List.rev before) @ after
                | x :: xs -> loop (x :: before) xs
                | [] -> entries

            loop [] entries

    module Node =

        open Prefix
        open CollisionHelpers

        let inline childBitIndex prefix =
            (prefixBits prefix)
            &&& Bitmap.bitIndexMask<BitmapHolder> ()
            |> asBits

        let inline containsChild childBitIndex bitmap = Bitmap.isBitOn childBitIndex bitmap

        let childArrayIndex childBitIndex bitmap =
            bitmap
            |> Bitmap.bitsLowerThan childBitIndex
            |> Bitmap.countBitsOn
            |> int

        let inline nextLayerPrefix (Prefix (bits, length)) =
            let shift = min length (Bitmap.bitIndexBits<BitmapHolder> ())
            Prefix(rshift bits shift, length - shift)

        let rec add eqComparer entry entryHash prefix node =
            match node with
            | Leaf oldEntry when Key.equals eqComparer (KVEntry.key entry) (KVEntry.key oldEntry) -> Leaf entry, Replaced
            | Leaf oldEntry ->
                let oldHash = Key.uhash eqComparer (KVEntry.key oldEntry)

                if entryHash = oldHash then
                    LeafWithCollisions [ entry; oldEntry ], AddOutcome.Added
                else
                    let oldPrefix = currentLevelPrefixFromHash (length prefix) oldHash

                    Branch(Bitmap.bit (childBitIndex oldPrefix), [| node |])
                    |> add eqComparer entry entryHash prefix
            | LeafWithCollisions entries ->
                let collisionHash = collisionHash eqComparer entries

                if entryHash = collisionHash then
                    let newEntries, outcome = insertOrReplace eqComparer entry entries
                    LeafWithCollisions newEntries, outcome
                else
                    let collisionPrefix = currentLevelPrefixFromHash (length prefix) collisionHash
                    let collisionBitIndex = childBitIndex collisionPrefix

                    Branch (Bitmap.bit collisionBitIndex, [| node |])
                    |> add eqComparer entry entryHash prefix
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix
                let arrayIndex = childArrayIndex bitIndex bitmap

                if containsChild bitIndex bitmap then
                    let newChild, outcome =
                        add eqComparer entry entryHash (nextLayerPrefix prefix) children[arrayIndex]

                    Branch(bitmap, Array.put newChild arrayIndex children), outcome
                else
                    Branch(Bitmap.setBit bitIndex bitmap, Array.insert (Leaf entry) arrayIndex children), Added
                    
        let rec add' entry entryHash prefix node =
            match node with
            | Leaf oldEntry when Key.equals' (KVEntry.key entry) (KVEntry.key oldEntry) -> Leaf entry, Replaced
            | Leaf oldEntry ->
                let oldHash = Key.uhash' (KVEntry.key oldEntry)

                if entryHash = oldHash then
                    LeafWithCollisions [ entry; oldEntry ], AddOutcome.Added
                else
                    let oldPrefix = currentLevelPrefixFromHash (length prefix) oldHash

                    Branch(Bitmap.bit (childBitIndex oldPrefix), [| node |])
                    |> add' entry entryHash prefix
            | LeafWithCollisions entries ->
                let collisionHash = collisionHash' entries

                if entryHash = collisionHash then
                    let newEntries, outcome = insertOrReplace' entry entries
                    LeafWithCollisions newEntries, outcome
                else
                    let collisionPrefix = currentLevelPrefixFromHash (length prefix) collisionHash
                    let collisionBitIndex = childBitIndex collisionPrefix

                    Branch (Bitmap.bit collisionBitIndex, [| node |])
                    |> add' entry entryHash prefix
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix
                let arrayIndex = childArrayIndex bitIndex bitmap

                if containsChild bitIndex bitmap then
                    let newChild, outcome =
                        add' entry entryHash (nextLayerPrefix prefix) children[arrayIndex]

                    Branch(bitmap, Array.put newChild arrayIndex children), outcome
                else
                    Branch(Bitmap.setBit bitIndex bitmap, Array.insert (Leaf entry) arrayIndex children), Added

        let rec containsKey eqComparer targetKey targetHash prefix =
            function
            | Leaf (KVEntry (key, _)) when Key.equals eqComparer key targetKey -> true
            | Leaf _ -> false
            | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->
                let rec existsIn =
                    function
                    | KVEntry (key, _) :: _ when Key.equals eqComparer key targetKey -> true
                    | _ :: xs -> existsIn xs
                    | [] -> false

                existsIn entries
            | LeafWithCollisions _ -> false
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                containsChild bitIndex bitmap
                && containsKey eqComparer targetKey targetHash (nextLayerPrefix prefix) children[childArrayIndex bitIndex bitmap]
                
        let rec containsKey' targetKey targetHash prefix =
            function
            | Leaf (KVEntry (key, _)) when Key.equals' key targetKey -> true
            | Leaf _ -> false
            | LeafWithCollisions entries when collisionHash' entries = targetHash ->
                let rec existsIn =
                    function
                    | KVEntry (key, _) :: _ when Key.equals' key targetKey -> true
                    | _ :: xs -> existsIn xs
                    | [] -> false

                existsIn entries
            | LeafWithCollisions _ -> false
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                containsChild bitIndex bitmap
                && containsKey' targetKey targetHash (nextLayerPrefix prefix) children[childArrayIndex bitIndex bitmap]

        let rec find eqComparer targetKey targetHash prefix node =
            match node with
            | Leaf (KVEntry (key, value)) when Key.equals eqComparer key targetKey -> value
            | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->
                let rec findValue =
                    function
                    | KVEntry (key, value) :: _ when Key.equals eqComparer key targetKey -> value
                    | _ :: xs -> findValue xs
                    | [] -> KeyNotFoundException.throw targetKey

                findValue entries
            | Leaf _
            | LeafWithCollisions _ -> KeyNotFoundException.throw targetKey
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                if containsChild bitIndex bitmap then
                    find eqComparer targetKey targetHash (nextLayerPrefix prefix) children[childArrayIndex bitIndex bitmap]
                else
                    KeyNotFoundException.throw targetKey
                    
        let rec find' targetKey targetHash prefix node =
            match node with
            | Leaf (KVEntry (key, value)) when Key.equals' key targetKey -> value
            | LeafWithCollisions entries when collisionHash' entries = targetHash ->
                let rec findValue =
                    function
                    | KVEntry (key, value) :: _ when Key.equals' key targetKey -> value
                    | _ :: xs -> findValue xs
                    | [] -> KeyNotFoundException.throw targetKey

                findValue entries
            | Leaf _
            | LeafWithCollisions _ -> KeyNotFoundException.throw targetKey
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                if containsChild bitIndex bitmap then
                    find' targetKey targetHash (nextLayerPrefix prefix) children[childArrayIndex bitIndex bitmap]
                else
                    KeyNotFoundException.throw targetKey

        let rec maybeFind eqComparer targetKey targetHash prefix node =
            match node with
            | Leaf (KVEntry (key, value)) when Key.equals eqComparer key targetKey -> Some value
            | Leaf _ -> None
            | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->
                let rec maybeFindValue =
                    function
                    | KVEntry (key, value) :: _ when Key.equals eqComparer key targetKey -> Some value
                    | _ :: xs -> maybeFindValue xs
                    | [] -> None

                maybeFindValue entries
            | LeafWithCollisions _ -> None
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                if containsChild bitIndex bitmap then
                    maybeFind eqComparer targetKey targetHash (nextLayerPrefix prefix) children[childArrayIndex bitIndex bitmap]
                else
                    None
                    
        let rec maybeFind' targetKey targetHash prefix node =
            match node with
            | Leaf (KVEntry (key, value)) when Key.equals' key targetKey -> Some value
            | Leaf _ -> None
            | LeafWithCollisions entries when collisionHash' entries = targetHash ->
                let rec maybeFindValue =
                    function
                    | KVEntry (key, value) :: _ when Key.equals' key targetKey -> Some value
                    | _ :: xs -> maybeFindValue xs
                    | [] -> None

                maybeFindValue entries
            | LeafWithCollisions _ -> None
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                if containsChild bitIndex bitmap then
                    maybeFind' targetKey targetHash (nextLayerPrefix prefix) children[childArrayIndex bitIndex bitmap]
                else
                    None

        let rec remove eqComparer targetKey targetHash prefix node =
            match node with
            | Leaf (KVEntry (key, _)) when Key.equals eqComparer key targetKey -> NothingLeft
            | Leaf _ -> NotFound
            | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->
                match removeIfExists eqComparer targetKey entries with
                | newEntries when LanguagePrimitives.PhysicalEquality entries newEntries -> NotFound
                | [ single ] -> Removed(Leaf single)
                | moreThanOne -> Removed(LeafWithCollisions moreThanOne)
            | LeafWithCollisions _ -> NotFound
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                if containsChild bitIndex bitmap then
                    let childArrayIndex = childArrayIndex bitIndex bitmap

                    match remove eqComparer targetKey targetHash (nextLayerPrefix prefix) (Array.item childArrayIndex children)
                        with
                    | NotFound -> NotFound
                    | Removed child -> Removed(Branch(bitmap, Array.put child childArrayIndex children))
                    | NothingLeft ->
                        let newBitmap = Bitmap.clearBit bitIndex bitmap

                        if Array.isEmpty children then
                            NothingLeft
                        elif Array.length children = 1 then
                            Removed(Array.head children)
                        else
                            Removed(Branch(newBitmap, Array.remove childArrayIndex children))
                else
                    NotFound
                    
        let rec remove' targetKey targetHash prefix node =
            match node with
            | Leaf (KVEntry (key, _)) when Key.equals' key targetKey -> NothingLeft
            | Leaf _ -> NotFound
            | LeafWithCollisions entries when collisionHash' entries = targetHash ->
                match removeIfExists' targetKey entries with
                | newEntries when LanguagePrimitives.PhysicalEquality entries newEntries -> NotFound
                | [ single ] -> Removed(Leaf single)
                | moreThanOne -> Removed(LeafWithCollisions moreThanOne)
            | LeafWithCollisions _ -> NotFound
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                if containsChild bitIndex bitmap then
                    let childArrayIndex = childArrayIndex bitIndex bitmap

                    match remove' targetKey targetHash (nextLayerPrefix prefix) (Array.item childArrayIndex children)
                        with
                    | NotFound -> NotFound
                    | Removed child -> Removed(Branch(bitmap, Array.put child childArrayIndex children))
                    | NothingLeft ->
                        let newBitmap = Bitmap.clearBit bitIndex bitmap

                        if Array.isEmpty children then
                            NothingLeft
                        elif Array.length children = 1 then
                            Removed(Array.head children)
                        else
                            Removed(Branch(newBitmap, Array.remove childArrayIndex children))
                else
                    NotFound

        let rec toSeq =
            function
            | Leaf entry -> Seq.singleton entry
            | LeafWithCollisions entries -> upcast entries
            | Branch (_, children) -> Seq.collect toSeq children

        let rec toSeqOfPairs =
            function
            | Leaf entry -> entry |> KVEntry.asPair |> Seq.singleton
            | LeafWithCollisions entries -> entries |> Seq.map KVEntry.asPair
            | Branch (_, children) -> Seq.collect toSeqOfPairs children

        let rec keys =
            function
            | Leaf entry -> Seq.singleton (KVEntry.key entry)
            | LeafWithCollisions entries -> Seq.map KVEntry.key entries
            | Branch (_, children) -> Seq.collect keys children