module internal Flux.Collections.Internals.HamtMap

open System.Collections.Generic
open Flux
open Flux.Collections
open Flux.Collections.Internals.Hamt
open Flux.Collections.KeyValuePair.ActivePattern

type Node<'K, 'T> =
    | Leaf of Entry: KeyValuePair<'K, 'T>
    | LeafWithCollisions of Entries: KeyValuePair<'K, 'T> list
    | Branch of Bitmap: BitmapHolder * Children: Node<'K, 'T> array

module private Collision =

    let inline collisionHash eqComparer entries =
        entries |> List.head |> KeyValuePair.key |> Key.uhash eqComparer

module Node =

    open Collision

    type PutOutcome<'K, 'T> =
        | AddedTo of Node<'K, 'T>
        | ReplacedIn of Node<'K, 'T>

    let rec put eqComparer entry entryHash prefix node =
        match node with
        | Leaf oldEntry when Key.equals eqComparer (KeyValuePair.key entry) (KeyValuePair.key oldEntry) ->
            ReplacedIn (Leaf entry)
        | Leaf oldEntry ->
            let oldHash = Key.uhash eqComparer (KeyValuePair.key oldEntry)

            if entryHash = oldHash then
                AddedTo (LeafWithCollisions [ entry; oldEntry ])
            else
                let oldPrefix = Prefix.currentLevelPrefixFromHash (Prefix.length prefix) oldHash
                let branch = Branch (Bitmap.bit (Branch.childBitIndex oldPrefix), [| node |])

                put eqComparer entry entryHash prefix branch
        | LeafWithCollisions entries ->
            let collisionHash = collisionHash eqComparer entries

            if entryHash = collisionHash then
                let rec putOnList before =
                    function
                    | KeyValuePair (key, _) :: after when Key.equals eqComparer key (KeyValuePair.key entry) ->
                        ReplacedIn (LeafWithCollisions ((List.rev before) @ entry :: after))
                    | x :: xs -> putOnList (x :: before) xs
                    | [] -> AddedTo (LeafWithCollisions (entry :: entries)) //should I care about the order

                putOnList [] entries
            else
                let collisionPrefix =
                    Prefix.currentLevelPrefixFromHash (Prefix.length prefix) collisionHash

                let collisionBitIndex = Branch.childBitIndex collisionPrefix
                let branch = Branch (Bitmap.bit collisionBitIndex, [| node |])

                put eqComparer entry entryHash prefix branch
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix
            let arrayIndex = Branch.childArrayIndex bitIndex bitmap

            if Branch.containsChild bitIndex bitmap then
                let outcome =
                    put eqComparer entry entryHash (Prefix.nextLayerPrefix prefix) children[arrayIndex]

                match outcome with
                | AddedTo newChild -> AddedTo (Branch (bitmap, Array.put newChild arrayIndex children))
                | ReplacedIn newChild -> ReplacedIn (Branch (bitmap, Array.put newChild arrayIndex children))
            else
                AddedTo (Branch (Bitmap.setBit bitIndex bitmap, Array.insert (Leaf entry) arrayIndex children))

    let rec containsKey eqComparer targetKey targetHash prefix =
        function
        | Leaf (KeyValuePair (key, _)) when Key.equals eqComparer key targetKey -> true
        | Leaf _ -> false
        | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->
            let rec existsIn =
                function
                | KeyValuePair (key, _) :: _ when Key.equals eqComparer key targetKey -> true
                | _ :: xs -> existsIn xs
                | [] -> false

            existsIn entries
        | LeafWithCollisions _ -> false
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix

            Branch.containsChild bitIndex bitmap
            && containsKey
                eqComparer
                targetKey
                targetHash
                (Prefix.nextLayerPrefix prefix)
                children[Branch.childArrayIndex bitIndex bitmap]

    let rec find eqComparer targetKey targetHash prefix node =
        match node with
        | Leaf (KeyValuePair (key, value)) when Key.equals eqComparer key targetKey -> value
        | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->
            let rec findValue =
                function
                | KeyValuePair (key, value) :: _ when Key.equals eqComparer key targetKey -> value
                | _ :: xs -> findValue xs
                | [] -> KeyNotFoundException.throw targetKey

            findValue entries
        | Leaf _
        | LeafWithCollisions _ -> KeyNotFoundException.throw targetKey
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix

            if Branch.containsChild bitIndex bitmap then
                find
                    eqComparer
                    targetKey
                    targetHash
                    (Prefix.nextLayerPrefix prefix)
                    children[Branch.childArrayIndex bitIndex bitmap]
            else
                KeyNotFoundException.throw targetKey

    let rec maybeFind eqComparer targetKey targetHash prefix node =
        match node with
        | Leaf (KeyValuePair (key, value)) when Key.equals eqComparer key targetKey -> Some value
        | Leaf _ -> None
        | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->
            let rec maybeFindValue =
                function
                | KeyValuePair (key, value) :: _ when Key.equals eqComparer key targetKey -> Some value
                | _ :: xs -> maybeFindValue xs
                | [] -> None

            maybeFindValue entries
        | LeafWithCollisions _ -> None
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix

            if Branch.containsChild bitIndex bitmap then
                maybeFind
                    eqComparer
                    targetKey
                    targetHash
                    (Prefix.nextLayerPrefix prefix)
                    children[Branch.childArrayIndex bitIndex bitmap]
            else
                None

    type RemoveOutcome<'K, 'T> =
        | NothingLeft
        | RemovedFrom of Node<'K, 'T>
        | NotFound

    let rec remove eqComparer targetKey targetHash prefix node =
        match node with
        | Leaf (KeyValuePair (key, _)) when Key.equals eqComparer key targetKey -> NothingLeft
        | Leaf _ -> NotFound
        | LeafWithCollisions entries when collisionHash eqComparer entries = targetHash ->

            let rec removeFromList before =
                function
                | KeyValuePair (key, _) :: after when Key.equals eqComparer key targetKey ->
                    match before, after with
                    | [ single ], []
                    | [], [ single ] -> RemovedFrom (Leaf single)
                    | _ -> RemovedFrom (LeafWithCollisions ((List.rev before) @ after))
                | x :: xs -> removeFromList (x :: before) xs
                | [] -> NotFound

            removeFromList [] entries
        | LeafWithCollisions _ -> NotFound
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix

            if Branch.containsChild bitIndex bitmap then
                let arrayIndex = Branch.childArrayIndex bitIndex bitmap
                let nextLayerPrefix = Prefix.nextLayerPrefix prefix

                let outcome =
                    remove eqComparer targetKey targetHash nextLayerPrefix children[arrayIndex]

                match outcome with
                | NotFound -> NotFound
                | RemovedFrom newChild -> RemovedFrom (Branch (bitmap, Array.put newChild arrayIndex children))
                | NothingLeft ->
                    if Array.length children = 1 then
                        NothingLeft
                    // This next commented code is invalid, because it could raise a branch a level up but it's
                    // bitmap would be the same, based on a prefix a level lower, thus rendering all children
                    // impossible to find. Consider the adding a new Branch (DeeperBranch) case that represents a
                    // branch that was raised
                    // elif Array.length children = 2 then
                    //     RemovedAndLeftNode children[if Branch.childArrayIndex = 0 then 1 else 0]
                    else
                        let newBitmap = Bitmap.clearBit bitIndex bitmap
                        RemovedFrom (Branch (newBitmap, Array.remove arrayIndex children))
            else
                NotFound

    type FilterOutcome<'K, 'T> =
        | NothingRemoved
        | AllRemoved of RemovedCount: int
        | NodeLeft of Node: Node<'K, 'T> * RemovedCount: int

    let rec filter predicate =
        function
        | Leaf (KeyValuePair (key, value)) when predicate key value -> NothingRemoved
        | Leaf _ -> AllRemoved 1
        | LeafWithCollisions entries ->
            let rec filterFromList discardedCount toKeep = //fix this
                function
                | entry :: xs when predicate (KeyValuePair.key entry) (KeyValuePair.value entry) ->
                    filterFromList discardedCount (entry :: toKeep) xs
                | _ :: xs -> filterFromList (discardedCount + 1) toKeep xs
                | [] when discardedCount > 0 ->
                    match toKeep with
                    | [] -> AllRemoved discardedCount
                    | [ single ] -> NodeLeft (Leaf single, entries.Length - 1)
                    | _ -> NodeLeft (LeafWithCollisions (List.rev toKeep), discardedCount)
                | [] -> NothingRemoved

            filterFromList 0 [] entries
        | Branch (bitmap, children) -> filterBranch predicate bitmap children

    and filterBranch predicate bitmap children =
        let rec loopOverChildren stepBitmap finalBitmap totalRemovedCount chosenNodes chosenCount index =
            if index < children.Length then
                let nextIndex = index + 1
                let currentBit = Bitmap.getLeastSignificantBitOn stepBitmap
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
        | Leaf (KeyValuePair (key, value)) -> predicate key value
        | LeafWithCollisions entries ->
            let rec existsInList =
                function
                | KeyValuePair (key, value) :: xs -> predicate key value || existsInList xs
                | [] -> false

            existsInList entries
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
        | Leaf (KeyValuePair (key, value)) -> predicate key value
        | LeafWithCollisions entries ->
            let rec forallInList =
                function
                | KeyValuePair (key, value) :: xs -> predicate key value && forallInList xs
                | [] -> true

            forallInList entries
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
        | Leaf (KeyValuePair (key, value)) -> action key value
        | LeafWithCollisions entries ->
            let rec iterList =
                function
                | KeyValuePair (key, value) :: xs ->
                    action key value
                    iterList xs
                | [] -> ()

            iterList entries
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
        | Leaf (KeyValuePair (key, value)) -> Leaf (KeyValuePair (key, mapper key value))
        | LeafWithCollisions entries ->
            LeafWithCollisions (List.map (fun (KeyValuePair (k, v)) -> KeyValuePair (k, mapper k v)) entries)
        | Branch (bitmap, children) -> Branch (bitmap, Array.map (fun node -> map mapper node) children)

    type PartitionOutcome<'K, 'T> =
        | NodeAccepted
        | NodeRejected of RejectedEntriesCount: int
        | NodeSplit of AcceptedPart: Node<'K, 'T> * RejectedPart: Node<'K, 'T> * RejectedEntriesCount: int

    let rec partition predicate =
        function
        | Leaf (KeyValuePair (key, value)) when predicate key value -> NodeAccepted
        | Leaf _ -> NodeRejected 1
        | LeafWithCollisions entries ->
            let accepted, rejected =
                List.partition (fun entry -> predicate (KeyValuePair.key entry) (KeyValuePair.value entry)) entries

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
                let currentBit = Bitmap.getLeastSignificantBitOn stepBitmap
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
        | Leaf (KeyValuePair (key, value)) -> picker key value
        | LeafWithCollisions entries ->
            let rec maybePickFromList =
                function
                | KeyValuePair (key, value) :: xs ->
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
        | Leaf (KeyValuePair (key, value)) -> folder state key value
        | LeafWithCollisions entries ->
            let rec loopOverEntries state =
                function
                | KeyValuePair (key, value) :: xs -> loopOverEntries (folder state key value) xs
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
        | AddedTo of Node<'K, 'T>
        | ReplacedIn of Node<'K, 'T>
        | NothingChanged
        | NothingLeft
        | RemovedFrom of Node<'K, 'T>

    let rec change changer eqComparer targetKey targetHash prefix =
        function
        | Leaf (KeyValuePair (key, value)) when Key.equals eqComparer key targetKey ->
            match changer (Some value) with
            | Some newValue -> KeyValuePair (targetKey, newValue) |> Leaf |> ReplacedIn
            | None -> NothingLeft
        | Leaf oldEntry as node ->
            match changer None with
            | Some value ->
                let entry = KeyValuePair (targetKey, value)
                let oldHash = Key.uhash eqComparer (KeyValuePair.key oldEntry)

                if targetHash = oldHash then
                    [ entry; oldEntry ] |> LeafWithCollisions |> AddedTo
                else
                    let oldPrefix = Prefix.currentLevelPrefixFromHash (Prefix.length prefix) oldHash
                    let branch = Branch (Bitmap.bit (Branch.childBitIndex oldPrefix), [| node |])

                    match put eqComparer entry targetHash prefix branch with
                    | PutOutcome.AddedTo branchWithEntry -> AddedTo branchWithEntry
                    | PutOutcome.ReplacedIn _ -> impossibleCodeBranch ()
            | None -> NothingChanged
        | LeafWithCollisions entries as node ->
            let collisionHash = collisionHash eqComparer entries

            if targetHash = collisionHash then
                let rec changeInList before =
                    function
                    | KeyValuePair (key, value) :: after when Key.equals eqComparer key targetKey ->
                        match changer (Some value) with
                        | Some newValue ->
                            (List.rev before) @ (KeyValuePair (targetKey, newValue)) :: after
                            |> LeafWithCollisions
                            |> ReplacedIn
                        | None ->
                            match before, after with
                            | [], [ single ]
                            | [ single ], [] -> (Leaf single)
                            | _ -> LeafWithCollisions ((List.rev before) @ after)
                            |> RemovedFrom
                    | x :: xs -> changeInList (x :: before) xs
                    | [] ->
                        match changer None with
                        | Some value -> AddedTo (LeafWithCollisions ((KeyValuePair (targetKey, value)) :: entries))
                        | None -> NothingChanged

                changeInList [] entries
            else
                match changer None with
                | Some value ->
                    let collisionPrefix =
                        Prefix.currentLevelPrefixFromHash (Prefix.length prefix) collisionHash

                    let collisionBitIndex = Branch.childBitIndex collisionPrefix
                    let branch = Branch (Bitmap.bit collisionBitIndex, [| node |])

                    let putOutcome =
                        put eqComparer (KeyValuePair (targetKey, value)) targetHash prefix branch

                    match putOutcome with
                    | PutOutcome.AddedTo newBranch -> AddedTo newBranch
                    | PutOutcome.ReplacedIn _ -> impossibleCodeBranch ()

                | None -> NothingChanged
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix
            let arrayIndex = Branch.childArrayIndex bitIndex bitmap

            if Branch.containsChild bitIndex bitmap then
                let outcome =
                    change changer eqComparer targetKey targetHash (Prefix.nextLayerPrefix prefix) children[arrayIndex]

                match outcome with
                | AddedTo newChild -> AddedTo (Branch (bitmap, Array.put newChild arrayIndex children))
                | ReplacedIn newChild -> ReplacedIn (Branch (bitmap, Array.put newChild arrayIndex children))
                | NothingLeft ->
                    if children.Length = 1 then
                        NothingLeft
                    else
                        let newBitmap = Bitmap.clearBit bitIndex bitmap
                        RemovedFrom (Branch (newBitmap, Array.remove arrayIndex children))
                | NothingChanged -> NothingChanged
                | RemovedFrom child -> RemovedFrom (Branch (bitmap, Array.put child arrayIndex children))
            else
                match changer None with
                | Some value ->
                    AddedTo (
                        Branch (
                            Bitmap.setBit bitIndex bitmap,
                            Array.insert (Leaf (KeyValuePair (targetKey, value))) arrayIndex children
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
        | Leaf entry -> entry |> KeyValuePair.asTuple |> Seq.singleton
        | LeafWithCollisions entries -> entries |> Seq.map KeyValuePair.asTuple
        | Branch (_, children) -> Seq.collect toSeqOfPairs children

    let rec keys =
        function
        | Leaf entry -> Seq.singleton (KeyValuePair.key entry)
        | LeafWithCollisions entries -> Seq.map KeyValuePair.key entries
        | Branch (_, children) -> Seq.collect keys children
