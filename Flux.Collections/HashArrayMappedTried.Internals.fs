namespace Flux.Collections

[<Struct>]
type KeyNotFound<'K> = KeyNotFound of Key: 'K

exception KeyNotFoundException of Msg: string * KeyNotFound: obj KeyNotFound with
    override this.Message = this.Msg

    static member inline throw key =
        KeyNotFoundException ("Key not found in Hamt", KeyNotFound (upcast key))
        |> raise

[<Struct>]
type EntryNotFound = EntryNotFound of Msg: string

exception EntryNotFoundException of EntryNotFound: EntryNotFound with
    override this.Message =
        match this.EntryNotFound with
        | EntryNotFound msg -> msg

namespace Flux.Collections.Internals

open Flux
open Flux.Collections
open Flux.Bit

module internal Hamt =

#if X64
    type BitmapHolder = Bitmap64
#else
    type BitmapHolder = Bitmap32
#endif

    type Node<'K, 'T> =
        | Leaf of Entry: KVEntry<'K, 'T>
        | LeafWithCollisions of Entries: KVEntry<'K, 'T> list
        | Branch of Bitmap: Bitmap<BitmapHolder> * Children: Node<'K, 'T> array

    [<Struct>]
    type Prefix = Prefix of bits: uint32 * length: int<bit>

    module Key =
        open System.Collections.Generic

        [<Literal>]
        let HashSize = 32<bit>

        let inline uhash (eqComparer: #IEqualityComparer<_>) key = key |> eqComparer.GetHashCode |> uint32

        let inline equals (eqComparer: #IEqualityComparer<_>) k1 k2 = eqComparer.Equals (k1, k2)

    module Prefix =

        let inline currentLevelPrefixFromHash currentLength hash =
            Prefix (rshift hash (Key.HashSize - currentLength), currentLength)

        let inline fullPrefixFromHash hash =
            currentLevelPrefixFromHash Key.HashSize hash

        let inline bits (Prefix (bits, _)) = bits

        let inline length (Prefix (_, length)) = length

    module private Collision =

        let inline collisionHash eqComparer entries =
            entries |> List.head |> KVEntry.key |> Key.uhash eqComparer

    module Node =

        open Prefix
        open Collision

        /// Returns the index of the child to which the prefix points to
        /// if prefix is xx...x00101 it points to the child at index 5 (101)
        let inline childBitIndex prefix =
            (bits prefix) &&& Bitmap.bitIndexMask<BitmapHolder> () |> asBits

        /// Returns true if the bit to which childBitIndex (index of the child) points to is on
        let inline containsChild childBitIndex bitmap = Bitmap.isBitOn childBitIndex bitmap

        /// Returns the index in the array to which childBitIndex points to. The array contains only elements for which
        /// the bitmap has bits on. If the bitmap has only 3 bits on, the children array would have 3 elements.
        /// For a bitmap 0..001000001, only the bits 0 and 6 are on, so the children array would have items.
        /// childArrayIndex 6 0..001000001 would return 1
        /// It's important to first ensure that the target bit is on in the bitmap by calling containsChild before using
        /// this function
        let childArrayIndex childBitIndex bitmap =
            bitmap |> Bitmap.bitsLowerThan childBitIndex |> Bitmap.countBitsOn |> int

        let inline nextLayerPrefix (Prefix (bits, length)) =
            let shift = min length (Bitmap.bitIndexBits<BitmapHolder> ())
            Prefix (rshift bits shift, length - shift)

        type PutOutcome =
            | Added
            | Replaced

        let rec put eqComparer entry entryHash prefix node =
            match node with
            | Leaf oldEntry when Key.equals eqComparer (KVEntry.key entry) (KVEntry.key oldEntry) ->
                struct (Leaf entry, Replaced)
            | Leaf oldEntry ->
                let oldHash = Key.uhash eqComparer (KVEntry.key oldEntry)

                if entryHash = oldHash then
                    LeafWithCollisions [ entry; oldEntry ], Added
                else
                    let oldPrefix = currentLevelPrefixFromHash (length prefix) oldHash
                    let branch = Branch (Bitmap.bit (childBitIndex oldPrefix), [| node |])

                    put eqComparer entry entryHash prefix branch
            | LeafWithCollisions entries ->
                let collisionHash = collisionHash eqComparer entries

                if entryHash = collisionHash then
                    let rec putOnList before =
                        function
                        | KVEntry (key, _) :: after when Key.equals eqComparer key (KVEntry.key entry) ->
                            struct ((List.rev before) @ entry :: after, Replaced)
                        | x :: xs -> putOnList (x :: before) xs
                        | [] -> struct (entry :: entries, Added) //should I care about the order

                    let struct (newEntries, outcome) = putOnList [] entries
                    LeafWithCollisions newEntries, outcome
                else
                    let collisionPrefix = currentLevelPrefixFromHash (length prefix) collisionHash
                    let collisionBitIndex = childBitIndex collisionPrefix

                    Branch (Bitmap.bit collisionBitIndex, [| node |])
                    |> put eqComparer entry entryHash prefix
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix
                let arrayIndex = childArrayIndex bitIndex bitmap

                if containsChild bitIndex bitmap then
                    let struct (newChild, outcome) =
                        put eqComparer entry entryHash (nextLayerPrefix prefix) children[arrayIndex]

                    struct (Branch (bitmap, Array.put newChild arrayIndex children), outcome)
                else
                    struct (Branch (Bitmap.setBit bitIndex bitmap, Array.insert (Leaf entry) arrayIndex children), Added)

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
                && containsKey
                    eqComparer
                    targetKey
                    targetHash
                    (nextLayerPrefix prefix)
                    children[childArrayIndex bitIndex bitmap]

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
                    find
                        eqComparer
                        targetKey
                        targetHash
                        (nextLayerPrefix prefix)
                        children[childArrayIndex bitIndex bitmap]
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
                    maybeFind
                        eqComparer
                        targetKey
                        targetHash
                        (nextLayerPrefix prefix)
                        children[childArrayIndex bitIndex bitmap]
                else
                    None

        type RemoveOutcome<'K, 'T> =
            | NothingLeft
            | RemovedAndLeftNode of Node<'K, 'T>
            | NotFound

        let rec remove eqComparer targetKey targetHash prefix node =
            match node with
            | Leaf (KVEntry (key, _)) when Key.equals eqComparer key targetKey -> NothingLeft
            | Leaf _ -> NotFound
            | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->

                let rec removeFromList before =
                    function
                    | KVEntry (key, _) :: after when Key.equals eqComparer key targetKey ->
                        match (List.rev before) @ after with
                        | [ single ] -> RemovedAndLeftNode (Leaf single)
                        | moreThanOne -> RemovedAndLeftNode (LeafWithCollisions moreThanOne)
                    | x :: xs -> removeFromList (x :: before) xs
                    | [] -> NotFound

                removeFromList [] entries
            | LeafWithCollisions _ -> NotFound
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix

                if containsChild bitIndex bitmap then
                    let arrayIndex = childArrayIndex bitIndex bitmap
                    let nextLayerPrefix = nextLayerPrefix prefix

                    let outcome =
                        remove eqComparer targetKey targetHash nextLayerPrefix children[arrayIndex]

                    match outcome with
                    | NotFound -> NotFound
                    | RemovedAndLeftNode child ->
                        RemovedAndLeftNode (Branch (bitmap, Array.put child arrayIndex children))
                    | NothingLeft ->
                        if Array.length children = 1 then
                            NothingLeft
                        // This next commented code is invalid, because it could raise a branch a level up but it's
                        // bitmap would be the same, based on a prefix a level lower, thus rendering all children
                        // impossible to find. Consider the adding a new Branch (DeeperBranch) case that represents a
                        // branch that was raised
                        // elif Array.length children = 2 then
                        //     RemovedAndLeftNode children[if childArrayIndex = 0 then 1 else 0]
                        else
                            let newBitmap = Bitmap.clearBit bitIndex bitmap
                            RemovedAndLeftNode (Branch (newBitmap, Array.remove arrayIndex children))
                else
                    NotFound

        type FilterOutcome<'K, 'T> =
            | NothingRemoved
            | AllRemoved of RemovedCount: int
            | NodeLeft of Node: Node<'K, 'T> * RemovedCount: int

        let rec filter predicate =
            function
            | Leaf (KVEntry (key, value)) when predicate key value -> NothingRemoved
            | Leaf _ -> AllRemoved 1
            | LeafWithCollisions entries ->
                let rec filterFromList toKeep modified =
                    function
                    | entry :: xs when predicate (KVEntry.key entry) (KVEntry.value entry) ->
                        filterFromList (entry :: toKeep) modified xs
                    | _ :: xs -> filterFromList toKeep true xs
                    | [] when modified -> List.rev toKeep
                    | [] -> entries

                match filterFromList [] false entries with
                | newEntries when LanguagePrimitives.PhysicalEquality entries newEntries -> NothingRemoved
                | [ single ] -> NodeLeft (Leaf single, entries.Length - 1)
                | moreThanOneLeft ->
                    NodeLeft (LeafWithCollisions moreThanOneLeft, entries.Length - moreThanOneLeft.Length)
            | Branch (bitmap, children) -> filterBranch predicate bitmap children

        and filterBranch predicate bitmap children =
            let rec loopOverChildren stepBitmap finalBitmap totalRemovedCount chosenNodes chosenCount index =
                if index < children.Length then
                    let nextIndex = index + 1
                    let currentBit = Bitmap.getLeastSignificantDigitOn stepBitmap
                    let nextStepBitmap = Bitmap.difference stepBitmap currentBit
                    let child = children[index]
                    let outcome = filter predicate child

                    match outcome with
                    | NothingRemoved ->
                        let nextChosenNodes = child :: chosenNodes
                        let nextChosenCount = chosenCount + 1

                        loopOverChildren
                            nextStepBitmap
                            finalBitmap
                            totalRemovedCount
                            nextChosenNodes
                            nextChosenCount
                            nextIndex
                    | NodeLeft (node, removedCount) ->
                        let nextTotalRemovedCount = totalRemovedCount + removedCount
                        let nextChosenNodes = node :: chosenNodes
                        let nextChosenCount = chosenCount + 1

                        loopOverChildren
                            nextStepBitmap
                            finalBitmap
                            nextTotalRemovedCount
                            nextChosenNodes
                            nextChosenCount
                            nextIndex
                    | AllRemoved removedCount ->
                        let nextFinalBitmap = Bitmap.difference finalBitmap currentBit
                        let nextTotalRemovedCount = (totalRemovedCount + removedCount)

                        loopOverChildren
                            nextStepBitmap
                            nextFinalBitmap
                            nextTotalRemovedCount
                            chosenNodes
                            chosenCount
                            nextIndex
                elif chosenCount = 0 then
                    AllRemoved totalRemovedCount
                elif totalRemovedCount = 0 then
                    NothingRemoved
                else
                    let children = Array.zeroCreate chosenCount

                    let rec toArray index list =
                        if index >= 0 then
                            children[index] <- List.head list
                            toArray (index - 1) (List.tail list)
                        else
                            children

                    let nodeLeft = Branch (finalBitmap, toArray (chosenCount - 1) chosenNodes)
                    NodeLeft (nodeLeft, totalRemovedCount)

            loopOverChildren bitmap bitmap 0 [] 0 0

        let rec exists predicate =
            function
            | Leaf (KVEntry (key, value)) -> predicate key value
            | LeafWithCollisions entries ->
                let rec existsInList predicate =
                    function
                    | KVEntry (key, value) :: xs -> predicate key value || existsInList predicate xs
                    | [] -> false

                existsInList predicate entries
            | Branch (_, children) ->
                let rec loopOverChildren index =
                    if index < children.Length then
                        let child = children[index]
                        exists predicate child || loopOverChildren (index + 1)
                    else
                        false

                loopOverChildren 0

        let rec forall predicate =
            function
            | Leaf (KVEntry (key, value)) -> predicate key value
            | LeafWithCollisions entries ->
                let rec forallInList predicate =
                    function
                    | KVEntry (key, value) :: xs -> predicate key value && forallInList predicate xs
                    | [] -> true

                forallInList predicate entries
            | Branch (_, children) ->
                let rec loopOverChildren index =
                    if index < children.Length then
                        let child = children[index]
                        forall predicate child && loopOverChildren (index + 1)
                    else
                        true

                loopOverChildren 0

        let rec iter action =
            function
            | Leaf (KVEntry (key, value)) -> action key value
            | LeafWithCollisions entries ->
                let rec iterList action =
                    function
                    | KVEntry (key, value) :: xs ->
                        action key value
                        iterList action xs
                    | [] -> ()

                iterList action entries
            | Branch (_, children) ->
                let rec loopOverChildren index =
                    if index < children.Length then
                        let child = children[index]
                        iter action child
                        loopOverChildren (index + 1)
                    else
                        ()

                loopOverChildren 0

        let rec map mapper =
            function
            | Leaf (KVEntry (key, value)) -> Leaf (KVEntry (key, mapper key value))
            | LeafWithCollisions entries ->
                LeafWithCollisions (List.map (fun (KVEntry (k, v)) -> KVEntry (k, mapper k v)) entries)
            | Branch (bitmap, children) -> Branch (bitmap, Array.map (fun node -> map mapper node) children)

        type PartitionOutcome<'K, 'T> =
            | NodeAccepted
            | NodeRejected of RejectedEntriesCount: int
            | NodeSplit of AcceptedPart: Node<'K, 'T> * RejectedPart: Node<'K, 'T> * RejectedEntriesCount: int

        let rec partition predicate node =
            match node with
            | Leaf (KVEntry (key, value)) when predicate key value -> NodeAccepted
            | Leaf _ -> NodeRejected 1
            | LeafWithCollisions entries ->
                let accepted, rejected =
                    List.partition (fun entry -> predicate (KVEntry.key entry) (KVEntry.value entry)) entries

                let rejectedCount = List.length rejected
                NodeSplit (LeafWithCollisions accepted, LeafWithCollisions rejected, rejectedCount)
            | Branch (bitmap, children) -> partitionBranch predicate bitmap children

        and partitionBranch predicate bitmap children =
            let rec loopOverChildren
                stepBitmap
                acceptedBitmap
                rejectedBitmap
                rejectedEntriesCount
                acceptedChildren
                rejectedChildren
                acceptedChildrenCount
                rejectedChildrenCount
                index
                =
                if index < children.Length then
                    let nextIndex = index + 1
                    let currentBit = Bitmap.getLeastSignificantDigitOn stepBitmap
                    let nextStepBitmap = Bitmap.difference stepBitmap currentBit
                    let child = children[index]
                    let outcome = partition predicate child

                    match outcome with
                    | NodeAccepted ->
                        loopOverChildren
                            nextStepBitmap
                            acceptedBitmap
                            (Bitmap.difference rejectedBitmap currentBit)
                            rejectedEntriesCount
                            (child :: acceptedChildren)
                            rejectedChildren
                            (acceptedChildrenCount + 1)
                            rejectedChildrenCount
                            nextIndex
                    | NodeRejected childRejectedEntriesCount ->
                        loopOverChildren
                            nextStepBitmap
                            (Bitmap.difference acceptedBitmap currentBit)
                            rejectedBitmap
                            (rejectedEntriesCount + childRejectedEntriesCount)
                            acceptedChildren
                            (child :: rejectedChildren)
                            acceptedChildrenCount
                            (rejectedChildrenCount + 1)
                            nextIndex
                    | NodeSplit (acceptedNode, rejectedNode, childRejectedEntriesCount) ->
                        loopOverChildren
                            nextStepBitmap
                            acceptedBitmap
                            rejectedBitmap
                            (rejectedEntriesCount + childRejectedEntriesCount)
                            (acceptedNode :: acceptedChildren)
                            (rejectedNode :: rejectedChildren)
                            (acceptedChildrenCount + 1)
                            (rejectedChildrenCount + 1)
                            nextIndex
                elif rejectedChildrenCount = 0 then
                    NodeAccepted
                elif acceptedChildrenCount = 0 then
                    NodeRejected rejectedEntriesCount
                else
                    let acceptedChildrenArray =
                        Array.Unsafe.ofListWithKnownSize acceptedChildrenCount acceptedChildren

                    let acceptedPart = Branch (acceptedBitmap, acceptedChildrenArray)

                    let rejectedChildrenArray =
                        Array.Unsafe.ofListWithKnownSize rejectedChildrenCount rejectedChildren

                    let rejectedPart = Branch (rejectedBitmap, rejectedChildrenArray)
                    NodeSplit (acceptedPart, rejectedPart, rejectedEntriesCount)

            loopOverChildren bitmap bitmap bitmap 0 [] [] 0 0 0

        let rec maybePick picker =
            function
            | Leaf (KVEntry (key, value)) -> picker key value
            | LeafWithCollisions entries ->
                let rec maybePickFromList =
                    function
                    | KVEntry (key, value) :: xs ->
                        match picker key value with
                        | Some x -> Some x
                        | None -> maybePickFromList xs
                    | [] -> None

                maybePickFromList entries
            | Branch (_, children) ->
                let rec loopOverChildren index =
                    if index < children.Length then
                        let child = children[index]

                        match maybePick picker child with
                        | Some x -> Some x
                        | None -> loopOverChildren (index + 1)
                    else
                        None

                loopOverChildren 0

        let rec fold folder state =
            function
            | Leaf (KVEntry (key, value)) -> folder state key value
            | LeafWithCollisions entries ->
                let rec loopOverEntries state =
                    function
                    | KVEntry (key, value) :: xs -> loopOverEntries (folder state key value) xs
                    | [] -> state

                loopOverEntries state entries
            | Branch (_, children) ->
                let rec loopOverChildren state index =
                    if index < children.Length then
                        loopOverChildren (fold folder state children[index]) (index + 1)
                    else
                        state

                loopOverChildren state 0

        type ChangeOutcome<'K, 'T> =
            | Added of Node<'K, 'T>
            | Replaced of Node<'K, 'T>
            | NothingChanged
            | NothingLeft
            | RemovedAndLeftNode of Node<'K, 'T>

        let rec change changer eqComparer targetKey targetHash prefix =
            function
            | Leaf (KVEntry (key, value)) when Key.equals eqComparer key targetKey ->
                match changer (Some value) with
                | Some newValue -> KVEntry (targetKey, newValue) |> Leaf |> Replaced
                | None -> NothingLeft
            | Leaf oldEntry as node ->
                match changer None with
                | Some value ->
                    let entry = KVEntry (targetKey, value)
                    let oldHash = Key.uhash eqComparer (KVEntry.key oldEntry)

                    if targetHash = oldHash then
                        [ entry; oldEntry ] |> LeafWithCollisions |> Added
                    else
                        let oldPrefix = currentLevelPrefixFromHash (length prefix) oldHash
                        let branch = Branch (Bitmap.bit (childBitIndex oldPrefix), [| node |])
                        let struct (branchWithEntry, _) = put eqComparer entry targetHash prefix branch
                        Added branchWithEntry
                | None -> NothingChanged
            | LeafWithCollisions entries as node ->
                let collisionHash = collisionHash eqComparer entries

                if targetHash = collisionHash then
                    let rec changeInList before =
                        function
                        | KVEntry (key, value) :: after when Key.equals eqComparer key targetKey ->
                            match changer (Some value) with
                            | Some newValue ->
                                (List.rev before) @ (KVEntry (targetKey, newValue)) :: after
                                |> LeafWithCollisions
                                |> Replaced
                            | None ->
                                match (List.rev before) @ after with
                                | [ single ] -> RemovedAndLeftNode (Leaf single)
                                | moreThanOne -> RemovedAndLeftNode (LeafWithCollisions moreThanOne)
                        | x :: xs -> changeInList (x :: before) xs
                        | [] ->
                            match changer None with
                            | Some value -> Added (LeafWithCollisions ((KVEntry (targetKey, value)) :: entries))
                            | None -> NothingChanged

                    changeInList [] entries
                else
                    match changer None with
                    | Some value ->
                        let collisionPrefix = currentLevelPrefixFromHash (length prefix) collisionHash
                        let collisionBitIndex = childBitIndex collisionPrefix

                        let branch = Branch (Bitmap.bit collisionBitIndex, [| node |])

                        let struct (branchWithEntry, _) =
                            put eqComparer (KVEntry (targetKey, value)) targetHash prefix branch

                        Added branchWithEntry
                    | None -> NothingChanged
            | Branch (bitmap, children) ->
                let bitIndex = childBitIndex prefix
                let arrayIndex = childArrayIndex bitIndex bitmap

                if containsChild bitIndex bitmap then
                    let outcome =
                        change changer eqComparer targetKey targetHash (nextLayerPrefix prefix) children[arrayIndex]

                    match outcome with
                    | Added newChild -> Added (Branch (bitmap, Array.put newChild arrayIndex children))
                    | Replaced newChild -> Replaced (Branch (bitmap, Array.put newChild arrayIndex children))
                    | NothingLeft ->
                        if children.Length = 1 then
                            NothingLeft
                        else
                            let newBitmap = Bitmap.clearBit bitIndex bitmap
                            RemovedAndLeftNode (Branch (newBitmap, Array.remove arrayIndex children))
                    | NothingChanged -> NothingChanged
                    | RemovedAndLeftNode child ->
                        RemovedAndLeftNode (Branch (bitmap, Array.put child arrayIndex children))
                else
                    match changer None with
                    | Some value ->
                        Added (
                            Branch (
                                Bitmap.setBit bitIndex bitmap,
                                Array.insert (Leaf (KVEntry (targetKey, value))) arrayIndex children
                            )
                        )
                    | None -> NothingChanged

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
