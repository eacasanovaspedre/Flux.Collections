module internal Flux.Collections.Internals.HamtSet

open Flux
open Flux.Collections
open Flux.Collections.Internals.Hamt
open Flux.Bit

type Node<'E> =
    | Leaf of Element: 'E
    | LeafWithCollisions of Elements: 'E list
    | Branch of Bitmap: BitmapHolder * Children: Node<'E> array
    
module private Collision =

    let inline collisionHash eqComparer elements =
        elements |> List.head |> Key.uhash eqComparer

module Node =

    open Collision

    type PutOutcome<'E> =
        | AddedTo of 'E
        | AlreadyExisted

    let rec put eqComparer element elementHash prefix node =
        match node with
        | Leaf oldElement when Key.equals eqComparer element oldElement -> AlreadyExisted
        | Leaf oldElement ->
            let oldHash = Key.uhash eqComparer oldElement

            if elementHash = oldHash then
                LeafWithCollisions [ element; oldElement ] |> AddedTo
            else
                let oldPrefix = Prefix.currentLevelPrefixFromHash (Prefix.length prefix) oldHash
                let branch = Branch (Bitmap.bit (Branch.childBitIndex oldPrefix), [| node |])

                put eqComparer element elementHash prefix branch
        | LeafWithCollisions elements ->
            let collisionHash = collisionHash eqComparer elements

            if elementHash = collisionHash then
                let rec putOnList before =
                    function
                    | oldElement :: _ when Key.equals eqComparer oldElement element -> AlreadyExisted
                    | x :: xs -> putOnList (x :: before) xs
                    | [] -> AddedTo (LeafWithCollisions (element :: elements)) //should I care about the order

                putOnList [] elements
            else
                let collisionPrefix =
                    Prefix.currentLevelPrefixFromHash (Prefix.length prefix) collisionHash

                let collisionBitIndex = Branch.childBitIndex collisionPrefix
                let branch = Branch (Bitmap.bit collisionBitIndex, [| node |])

                put eqComparer element elementHash prefix branch
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix
            let arrayIndex = Branch.childArrayIndex bitIndex bitmap

            if Branch.containsChild bitIndex bitmap then
                let outcome =
                    put eqComparer element elementHash (Prefix.nextLayerPrefix prefix) children[arrayIndex]

                match outcome with
                | AddedTo newChild -> AddedTo (Branch (bitmap, Array.put newChild arrayIndex children))
                | AlreadyExisted -> AlreadyExisted
            else
                AddedTo (Branch (Bitmap.setBit bitIndex bitmap, Array.insert (Leaf element) arrayIndex children))

    let rec contains eqComparer element elementHash prefix =
        function
        | Leaf oldElement when Key.equals eqComparer oldElement element -> true
        | Leaf _ -> false
        | LeafWithCollisions entries when collisionHash eqComparer entries = elementHash ->
            let rec existsIn =
                function
                | oldElement :: _ when Key.equals eqComparer oldElement element -> true
                | _ :: xs -> existsIn xs
                | [] -> false

            existsIn entries
        | LeafWithCollisions _ -> false
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix

            Branch.containsChild bitIndex bitmap
            && contains
                eqComparer
                element
                elementHash
                (Prefix.nextLayerPrefix prefix)
                children[Branch.childArrayIndex bitIndex bitmap]

    type RemoveOutcome<'E> =
        | NothingLeft
        | RemovedFrom of Node<'E>
        | NotFound

    let rec remove eqComparer element elementHash prefix node =
        match node with
        | Leaf oldElement when Key.equals eqComparer oldElement element -> NothingLeft
        | Leaf _ -> NotFound
        | LeafWithCollisions elements when collisionHash eqComparer elements = elementHash ->

            let rec removeFromList before =
                function
                | oldElement :: after when Key.equals eqComparer oldElement element ->
                    match before, after with
                    | [], [ single ]
                    | [ single ], [] -> RemovedFrom (Leaf single)
                    | _ -> RemovedFrom (LeafWithCollisions ((List.rev before) @ after))
                | x :: xs -> removeFromList (x :: before) xs
                | [] -> NotFound

            removeFromList [] elements
        | LeafWithCollisions _ -> NotFound
        | Branch (bitmap, children) ->
            let bitIndex = Branch.childBitIndex prefix

            if Branch.containsChild bitIndex bitmap then
                let arrayIndex = Branch.childArrayIndex bitIndex bitmap
                let nextLayerPrefix = Prefix.nextLayerPrefix prefix

                let outcome =
                    remove eqComparer element elementHash nextLayerPrefix children[arrayIndex]

                match outcome with
                | NotFound -> NotFound
                | RemovedFrom newChild -> RemovedFrom (Branch (bitmap, Array.put newChild arrayIndex children))
                | NothingLeft ->
                    if children.Length = 1 then
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

    type FilterOutcome<'E> =
        | NothingRemoved
        | AllRemoved of RemovedCount: int
        | NodeLeft of Node: Node<'E> * RemovedCount: int

    let rec filter predicate =
        function
        | Leaf element when predicate element -> NothingRemoved
        | Leaf _ -> AllRemoved 1
        | LeafWithCollisions elements ->
            let rec filterFromList discardedCount toKeep =
                function
                | element :: xs when predicate element -> filterFromList discardedCount (element :: toKeep) xs
                | _ :: xs -> filterFromList (discardedCount + 1) toKeep xs
                | [] when discardedCount > 0 ->
                    match toKeep with
                    | [] -> AllRemoved discardedCount
                    | [ single ] -> NodeLeft (Leaf single, discardedCount)
                    | _ -> NodeLeft (LeafWithCollisions (List.rev toKeep), discardedCount)
                | [] -> NothingRemoved

            filterFromList 0 [] elements
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
        | Leaf element -> predicate element
        | LeafWithCollisions elements ->
            let rec existsInList =
                function
                | element :: xs -> predicate element || existsInList xs
                | [] -> false

            existsInList elements
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
        | Leaf element -> predicate element
        | LeafWithCollisions elements ->
            let rec forallInList =
                function
                | element :: xs -> predicate element && forallInList xs
                | [] -> true

            forallInList elements
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
        | Leaf element -> action element
        | LeafWithCollisions elements ->
            let rec iterList =
                function
                | element :: xs ->
                    action element
                    iterList xs
                | [] -> ()

            iterList elements
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
        | Leaf element -> Leaf (mapper element)
        | LeafWithCollisions elements -> LeafWithCollisions (List.map mapper elements)
        | Branch (bitmap, children) -> Branch (bitmap, Array.map (fun node -> map mapper node) children)

    type PartitionOutcome<'E> =
        | NodeAccepted
        | NodeRejected of RejectedEntriesCount: int
        | NodeSplit of AcceptedPart: Node<'E> * RejectedPart: Node<'E> * RejectedEntriesCount: int

    let rec partition predicate =
        function
        | Leaf element when predicate element -> NodeAccepted
        | Leaf _ -> NodeRejected 1
        | LeafWithCollisions elements ->
            let accepted, rejected = List.partition predicate elements
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
        | Leaf element -> picker element
        | LeafWithCollisions elements ->
            let rec maybePickFromList =
                function
                | element :: xs ->
                    match picker element with
                    | Some x -> Some x
                    | None -> maybePickFromList xs
                | [] -> None

            maybePickFromList elements
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
        | Leaf element -> folder state element
        | LeafWithCollisions elements ->
            let rec loopOverEntries state =
                function
                | element :: xs -> loopOverEntries (folder state element) xs
                | [] -> state

            loopOverEntries state elements
        | Branch (_, children) ->
            let rec loopOverChildren state index =
                if index < children.Length then
                    loopOverChildren (fold folder state children[index]) (index + 1)
                else
                    state

            loopOverChildren state 0

    let rec toSeq =
        function
        | Leaf element -> Seq.singleton element
        | LeafWithCollisions elements -> elements
        | Branch (_, children) -> Seq.collect toSeq children

    let rec simpleMerge eqComparer nodeDest =
        function
        | Leaf element ->
            let hash = Key.uhash eqComparer element
            let outcome = put eqComparer element hash (Prefix.fullPrefixFromHash hash) nodeDest

            match outcome with
            | AddedTo node -> node, 1
            | AlreadyExisted -> nodeDest, 0
        | LeafWithCollisions elements ->
            let hash = collisionHash eqComparer elements
            let prefix = Prefix.fullPrefixFromHash hash

            let rec putAll sizeIncrease node =
                function
                | x :: xs ->
                    let outcome = put eqComparer x hash prefix nodeDest

                    match outcome with
                    | AddedTo newNode -> putAll (sizeIncrease + 1) newNode xs
                    | AlreadyExisted -> putAll sizeIncrease node xs
                | [] -> node, sizeIncrease

            putAll 0 nodeDest elements
        | Branch (_, children) ->
            let rec mergeAll sizeIncrease node index =
                if index < children.Length then
                    let child = children[index]
                    let newNode, increase = simpleMerge eqComparer node child
                    mergeAll (sizeIncrease + increase) newNode (index + 1)
                else
                    node, sizeIncrease

            mergeAll 0 nodeDest 0

    module private Helpers =

        let rec countEntries =
            function
            | Leaf _ -> 1
            | LeafWithCollisions elements -> List.length elements
            | Branch (_, children) ->
                let mutable size = 0

                for child in children do
                    size <- size + countEntries child

                size

        let inline mergeDistinct eqComparer elements1 elements2 : struct (_ * _) =
            let rec loop finalLength final loopList list : struct (_ * _) =
                match list with
                | [] -> LeafWithCollisions final, finalLength
                | h :: hs ->
                    match loopList with
                    | x :: _ when Key.equals eqComparer x h -> loop finalLength final elements1 hs
                    | _ :: xs -> loop finalLength final xs list
                    | [] -> loop (finalLength + 1) (h :: final) elements1 hs

            loop (List.length elements1) elements1 elements1 elements2

        let inline mergeCollisionWithNode
            eqComparer
            collisionNode
            elements
            elementsHash
            elementsLength
            initialPrefix
            initialNode
            : struct (_ * _) =
            let rec loopIn prefix node =
                match node with
                | Leaf element ->
                    let elementHash = Key.uhash eqComparer element

                    if elementHash = elementsHash then
                        let rec putOnList before =
                            function
                            | oldElement :: _ when Key.equals eqComparer oldElement element ->
                                struct (collisionNode, elementsLength)
                            | x :: xs -> putOnList (x :: before) xs
                            | [] -> struct (LeafWithCollisions (element :: elements), elementsLength + 1) //should I care about the order

                        putOnList [] elements
                    else
                        let elementPrefix =
                            Prefix.currentLevelPrefixFromHash (Prefix.length prefix) elementHash

                        let elementBitIndex = Branch.childBitIndex elementPrefix
                        let branch = Branch (Bitmap.bit elementBitIndex, [| node |])

                        loopIn prefix branch
                | LeafWithCollisions oldElements ->
                    let oldElementsHash = collisionHash eqComparer oldElements

                    if elementsHash = oldElementsHash then
                        mergeDistinct eqComparer elements oldElements
                    else
                        let oldElementsPrefix =
                            Prefix.currentLevelPrefixFromHash (Prefix.length prefix) elementsHash

                        let oldElementsBitIndex = Branch.childBitIndex oldElementsPrefix
                        let branch = Branch (Bitmap.bit oldElementsBitIndex, [| node |])

                        loopIn prefix branch
                | Branch (bitmap, children) ->
                    let bitIndex = Branch.childBitIndex prefix
                    let arrayIndex = Branch.childArrayIndex bitIndex bitmap

                    let rec countEntriesInChildren newChildCount index count =
                        if index < children.Length then
                            if index <> arrayIndex then
                                countEntriesInChildren newChildCount (index + 1) (count + countEntries children[index])
                            else
                                countEntriesInChildren newChildCount (index + 1) (count + newChildCount)
                        else
                            count

                    if Branch.containsChild bitIndex bitmap then
                        let struct (newChild, newChildCount) =
                            loopIn (Prefix.nextLayerPrefix prefix) children[arrayIndex]

                        Branch (bitmap, Array.put newChild arrayIndex children),
                        countEntriesInChildren newChildCount 0 0
                    else
                        Branch (Bitmap.setBit bitIndex bitmap, Array.insert collisionNode arrayIndex children),
                        countEntriesInChildren elementsLength 0 0

            loopIn initialPrefix initialNode

    module SameComparer =

        let rec union eqComparer prefixLength node1 node2 : struct (_ * _) =
            match node1, node2 with
            | Leaf e1, Leaf e2 when Key.equals eqComparer e1 e2 -> node1, 1
            | Leaf e1, Leaf e2 ->
                let hash1 = Key.uhash eqComparer e1
                let hash2 = Key.uhash eqComparer e2

                if hash1 = hash2 then
                    LeafWithCollisions [ e2; e1 ], 2
                else
                    let prefix1 = SimplePrefix.fromHash prefixLength hash1
                    let branch = Branch (Bitmap.bit (SimpleBranch.childBitIndex prefix1), [| node1 |])
                    let prefix2 = Prefix.currentLevelPrefixFromHash prefixLength hash2

                    let outcome = put eqComparer e2 hash2 prefix2 branch

                    match outcome with
                    | PutOutcome.AddedTo newBranch -> struct (newBranch, 2)
                    | PutOutcome.AlreadyExisted -> impossibleCodeBranch ()
            | LeafWithCollisions elements as collisionNode, anotherNode
            | anotherNode, (LeafWithCollisions elements as collisionNode) ->
                let elementsLength = List.length elements
                let elementsHash = collisionHash eqComparer elements
                let elementsPrefix = Prefix.currentLevelPrefixFromHash prefixLength elementsHash

                Helpers.mergeCollisionWithNode
                    eqComparer
                    collisionNode
                    elements
                    elementsHash
                    elementsLength
                    elementsPrefix
                    anotherNode
            | Leaf element, branch
            | branch, Leaf element ->
                let branchSize = Helpers.countEntries branch
                let elementHash = Key.uhash eqComparer element
                let elementPrefix = Prefix.currentLevelPrefixFromHash prefixLength elementHash
                let outcome = put eqComparer element elementHash elementPrefix branch

                match outcome with
                | AddedTo newBranch -> newBranch, branchSize + 1
                | AlreadyExisted -> branch, branchSize
            | Branch (bitmap1, children1), Branch (bitmap2, children2) ->
                let newBitmap = Bitmap.union bitmap1 bitmap2
                let newChildren = Array.zeroCreate (Bitmap.countBitsOn newBitmap |> asPlainInt)

                let rec mergeChildren bitmap1 bitmap2 bit1 bit2 index index1 index2 count =
                    if index < newChildren.Length then
                        let index' = index + 1

                        if bit1 = bit2 then
                            let child1 = children1[index1]
                            let child2 = children2[index2]
                            let nextLayerLength = SimplePrefix.nextLayerLength prefixLength

                            let struct (newChild, newChildSize) =
                                union eqComparer nextLayerLength child1 child2

                            newChildren[index] <- newChild
                            let bitmap1' = Bitmap.difference bitmap1 bit1
                            let bitmap2' = Bitmap.difference bitmap2 bit2
                            let bit1' = Bitmap.getLeastSignificantBitOn bitmap1'
                            let bit2' = Bitmap.getLeastSignificantBitOn bitmap2'
                            let index1' = index1 + 1
                            let index2' = index2 + 1
                            let count' = count + newChildSize
                            mergeChildren bitmap1' bitmap2' bit1' bit2' index' index1' index2' count'
                        elif
                            Bitmap.areAllBitsOff bit2
                            || (Bitmap.hasAtLeastOneBitOn bit1 && Bitmap.lessThanAsNumber bit1 bit2)
                        then
                            let child1 = children1[index1]
                            newChildren[index] <- child1
                            let bitmap1' = Bitmap.difference bitmap1 bit1
                            let bit1' = Bitmap.getLeastSignificantBitOn bitmap1'
                            let index1' = index1 + 1
                            let count' = count + Helpers.countEntries child1
                            mergeChildren bitmap1' bitmap2 bit1' bit2 index' index1' index2 count'
                        else
                            let child2 = children2[index2]
                            newChildren[index] <- child2
                            let bitmap2' = Bitmap.difference bitmap2 bit2
                            let bit2' = Bitmap.getLeastSignificantBitOn bitmap2
                            let index2' = index2 + 1
                            let count' = count + Helpers.countEntries child2
                            mergeChildren bitmap1 bitmap2' bit1 bit2' index' index1 index2' count'
                    else
                        count

                let bit1' = Bitmap.getLeastSignificantBitOn bitmap1
                let bit2' = Bitmap.getLeastSignificantBitOn bitmap2
                let count = mergeChildren bitmap1 bitmap2 bit1' bit2' 0 0 0 0

                Branch (newBitmap, newChildren), count

        let rec intersection eqComparer prefixLength node1 node2 =
            match node1, node2 with
            | Leaf e1, Leaf e2 when Key.equals eqComparer e1 e2 -> ValueSome (node1, 1)
            | Leaf _, Leaf _ -> ValueNone
            | Leaf element as leafNode, LeafWithCollisions elements
            | LeafWithCollisions elements, (Leaf element as leafNode) ->
                let rec findInElements = function
                    | h::_ when Key.equals eqComparer h element -> ValueSome (leafNode, 1)
                    | _::t -> findInElements t
                    | [] -> ValueNone
                findInElements elements
            | LeafWithCollisions elements1, LeafWithCollisions elements2 ->
                let rec keepCommon toKeep toKeepLength list1 list2 currentElement: struct (_*_) =
                    match list1 with
                    | x1::xs1 when Key.equals eqComparer x1 currentElement ->
                        match list2 with
                        | x2::xs2 -> keepCommon (currentElement :: toKeep) (toKeepLength + 1) elements1 xs2 x2
                        | [] -> toKeep, toKeepLength
                    | _::xs1 -> keepCommon toKeep toKeepLength xs1 list2 currentElement
                    | [] ->
                        match list2 with
                        | x2::xs2 -> keepCommon toKeep toKeepLength elements1 xs2 x2
                        | [] -> toKeep, toKeepLength
                let struct (toKeep, count) = keepCommon [] 0 elements1 (List.tail elements2) (List.head elements2)
                if count = 0 then ValueNone else ValueSome (LeafWithCollisions toKeep, count)
            | (Leaf e1 as leaf), Branch(bitmap, children)
            | Branch(bitmap, children), (Leaf e1 as leaf) ->
                let prefix = SimplePrefix.fromHash prefixLength (Key.uhash eqComparer e1)
                let bitIndex = SimpleBranch.childBitIndex prefix
                let arrayIndex = Branch.childArrayIndex bitIndex bitmap
                let nextLength = SimplePrefix.nextLayerLength prefixLength
                intersection eqComparer nextLength leaf children[arrayIndex]
            | LeafWithCollisions elements as collision, Branch (bitmap, children)
            | Branch (bitmap, children), (LeafWithCollisions elements as collision) ->
                let prefix = SimplePrefix.fromHash prefixLength (collisionHash eqComparer elements)
                let bitIndex = SimpleBranch.childBitIndex prefix
                let arrayIndex = Branch.childArrayIndex bitIndex bitmap
                let nextLength = SimplePrefix.nextLayerLength prefixLength
                intersection eqComparer nextLength collision children[arrayIndex]
            | Branch (bitmap1, children1), Branch (bitmap2, children2) ->
                let rec intersectBranches inBitmap outBitmap children childCount totalSize =
                    if Bitmap.hasAtLeastOneBitOn inBitmap then
                        let struct (nextInBitmap, leastSignificantBit) = Bitmap.clearLeastSignificantBitAndReturn inBitmap
                        let index1 = Branch.childArrayIndexOfBit leastSignificantBit bitmap1
                        let index2 = Branch.childArrayIndexOfBit leastSignificantBit bitmap2
                        let child1 = children1[index1]
                        let child2 = children2[index2]
                        let nextLength = SimplePrefix.nextLayerLength prefixLength
                        match intersection eqComparer nextLength child1 child2 with
                        | ValueNone ->
                            let nextOutBitmap = outBitmap
                            let nextChildCount = childCount
                            let nextChildren = children
                            let nextTotalSize = totalSize
                            intersectBranches nextInBitmap nextOutBitmap nextChildren nextChildCount nextTotalSize
                        | ValueSome (newChild, childSize) ->
                            let nextOutBitmap = Bitmap.union outBitmap leastSignificantBit
                            let nextChildCount = childCount + 1
                            let nextChildren = newChild::children
                            let nextTotalSize = totalSize + childSize
                            intersectBranches nextInBitmap nextOutBitmap nextChildren nextChildCount nextTotalSize
                    elif Bitmap.hasAtLeastOneBitOn outBitmap then
                        ValueSome (Branch (outBitmap, Array.Unsafe.ofListWithKnownSizeRev childCount children), totalSize)
                    else
                        ValueNone
                let intersectedBitmap = Bitmap.intersection bitmap1 bitmap2
                intersectBranches intersectedBitmap Bitmap.noBit [] 0 0 
