//I prefer this over recursive namespaces. If one day this becomes a compile error I will be forced to change it. I hate recursive namespaces, it breaks F# linear dependencies.
#nowarn "60"#nowarn "69" // Interface implementations in augmentations are now deprecated. Interface implementations should be given on the initial declaration of a type.
namespace Flux.Collections


exception IncompatibleMergeException of string

exception PatternMatchedBugException of string * string

[<AutoOpen>]
module private Helpers =
    let inline bug pattern = raise (PatternMatchedBugException("This pattern was never meant to be matched", pattern))

type private 'T SBHTree = Node of 'T * 'T list * 'T SBHTree list

type private 'T SBHTreeRoot = Root of int * 'T SBHTree

module private SBHTree =
    let item (Node(x, _, _)) = x

    let link descending (Node(x, auxX, childrenX) as treeX) (Node(y, auxY, childrenY) as treeY) =
        if x <= y <> descending then Node(x, auxX, treeY :: childrenX) else Node(y, auxY, treeX :: childrenY)

    let skewLink descending v treeX treeY =
        let (Node(w, aux, children)) = link descending treeX treeY
        if v <= w <> descending then Node(v, w :: aux, children) else Node(w, v :: aux, children)

module private SBHTreeRoot =
    open SBHTree

    let inline getTree (Root(_, tree)) = tree

    let insert descending value =
        function
        | Root(rank1, tree1) :: Root(rank2, tree2) :: roots when rank1 = rank2 ->
            Root(rank1 + 1, skewLink descending value tree1 tree2) :: roots
        | roots -> Root(0, Node(value, [], [])) :: roots

    let rec insRoot descending (Root(rank, tree) as root) roots =
        match roots with
        | [] -> [ root ]
        | Root(rank', _) :: _ when rank < rank' -> root :: roots
        | Root(_, tree') :: roots' -> insRoot descending (Root(rank + 1, link descending tree tree')) roots'

    let rec mergeRoots descending roots1 roots2 =
        match roots1, roots2 with
        | roots, []
        | [], roots -> roots
        | Root(rankX, treeX) :: roots1', Root(rankY, _) :: _ when rankX < rankY ->
            Root(rankX, treeX) :: mergeRoots descending roots1' roots2
        | Root(rankX, _) :: _, Root(rankY, treeY) :: roots2' when rankX > rankY ->
            Root(rankY, treeY) :: mergeRoots descending roots1 roots2'
        | Root(rank, treeX) :: roots1', Root(_, treeY) :: roots2' ->
            insRoot descending (Root(rank + 1, link descending treeX treeY)) (mergeRoots descending roots1' roots2')

    let normalize descending =
        function
        | [] -> []
        | root :: roots -> insRoot descending root roots

    let rec extractMinRoot descending =
        function
        | [ p ] -> (p, [])
        | (Root(_, tree) as root) :: roots ->
            let (Root(_, tree') as root', roots') = extractMinRoot descending roots
            if (SBHTree.item tree <= SBHTree.item tree') <> descending
            then root, roots
            else root', root :: roots'
        | [] -> bug "[]"

    let rec findMinRootItem descending =
        function
        | [ Root(_, node) ] -> item node
        | Root(_, node) :: roots' ->
            let this = item node
            let other = findMinRootItem descending roots'
            if this <= other <> descending then this else other
        | [] -> bug "[]"

    let uncons descending roots =
        //find the root with the minimum value a return it along with the remaining roots
        let (Root(rank, Node(x, aux, children)), roots') = extractMinRoot descending roots
        //reverse the children and set their ranks based on the parent's rank
        let (_, reversed) =
            children |> List.fold (fun (rank, trees) tree -> rank - 1, Root(rank - 1, tree) :: trees) (rank, [])
        //merge the reversed children with the remaining trees
        let merged = mergeRoots descending reversed (normalize descending roots')
        //reinsert all "auxiliary" elements
        let newRoots = aux |> List.fold (fun roots value -> insert descending value roots) merged
        x, newRoots

    let rec toListOrdered descending roots =
        let rec treeToList acc =
            function
            | (Node(x, aux, children) :: ts) :: tls ->
                let nacc = aux |> List.fold (fun xs x -> x :: xs) (x :: acc)
                treeToList nacc (children :: ts :: tls)
            | [] :: tls -> treeToList acc tls
            | [] -> acc

        let sorted =
            [ (roots |> List.map getTree) ]
            |> treeToList []
            |> List.sort

        if descending then sorted |> List.rev else sorted

//****************************************************************************************************
/// A SkewBinomialHeap is a priority queue where elements are inserted in any order, using "insert" and are
/// extracted in either ascending or descending order using "head", "peek", "tail", "pop" or any of their
/// "maybe" and "try" variants. The main advantage of the SkewBinomialHeap over the BinomialHeap is that it supports
/// insertions in constant time O(1). (Based on "Purely Functional Data Structures" - 1996 by Chris Okasaki)
type SkewBinomialHeap<'T when 'T: comparison> =
    private { Count: int
              Descending: bool
              Roots: 'T SBHTreeRoot list }
//interface IEnumerable<'T>

module SkewBinomialHeap =

    let inline private skewBinomialHeap count descending roots =
        { Count = count
          Descending = descending
          Roots = roots }

    ///O(1) - Returns true if the heap has no elements.
    let isEmpty { Count = count } = count = 0

    ///O(1) - Returns true if the heap has elements.
    let isNotEmpty { Count = count } = count > 0

    ///O(1) - Returns an empty heap.
    let empty descending = skewBinomialHeap 0 descending []

    ///O(log n) - Returns the element at the front. Throws if empty.
    let head heap =
        if isNotEmpty heap
        then SBHTreeRoot.findMinRootItem heap.Descending heap.Roots
        else invalidOp "Empty heap, no head"

    ///O(log n) - Returns Ok x where x is the element at the front.
    ///Returns Error if the collection is empty.
    let tryHead heap =
        if isNotEmpty heap
        then Ok(SBHTreeRoot.findMinRootItem heap.Descending heap.Roots)
        else Error "Empty heap, no head"

    ///O(log n) - Returns Some x where x is the element at the front.
    ///Returns None if the collection is empty.
    let maybeHead heap =
        if isNotEmpty heap
        then Some(SBHTreeRoot.findMinRootItem heap.Descending heap.Roots)
        else None

    ///O(1) - Returns a new heap with the element inserted.
    let insert x { Count = count; Descending = descending; Roots = roots } =
        skewBinomialHeap (count + 1) descending (SBHTreeRoot.insert descending x roots)

    ///O(1) - Returns true if a call to head of tryHead would return the maximum element in the collection.
    /// Returns false if the element at the head is the minimum.
    let isDescending { Descending = descending } = descending

    ///O(1) - Returns the number of elements in the collection.
    let length { Count = count } = count

    ///O(1) - Returns the number of elements in the collection.
    let count heap = length heap

    ///O(log n) - Returns a new heap with the elements of both heaps. The two heaps must have the same isDescending value.
    let merge
        { Count = countL; Descending = descendingL; Roots = rootsL }
        { Count = countR; Descending = descendingR; Roots = rootsR }
        =
        if descendingL = descendingR then
            skewBinomialHeap (countL + countR) descendingL
                (SBHTreeRoot.mergeRoots descendingL (SBHTreeRoot.normalize descendingL rootsL)
                     (SBHTreeRoot.normalize descendingL rootsR))
        else
            "Can not merge two heaps with diferent comparison methods"
            |> IncompatibleMergeException
            |> raise

    ///O(log n) - Returns Some h where h is the merged heap, if both original heaps have the same isDescending value.
    /// Returns None if isDescending is diferent in the heaps supplied.
    let maybeMerge
        { Count = countL; Descending = descendingL; Roots = rootsL }
        { Count = countR; Descending = descendingR; Roots = rootsR }
        =
        if descendingL = descendingR then
            Some
                (skewBinomialHeap (countL + countR) descendingL
                     (SBHTreeRoot.mergeRoots descendingL (SBHTreeRoot.normalize descendingL rootsL)
                          (SBHTreeRoot.normalize descendingL rootsR)))
        else
            None

    ///O(log n) - Returns Ok h where h is the merged heap, if both original heaps have the same isDescending value.
    /// Returns Error if descending is diferent in the heaps supplied.
    let tryMerge
        { Count = countL; Descending = descendingL; Roots = rootsL }
        { Count = countR; Descending = descendingR; Roots = rootsR }
        =
        if descendingL = descendingR then
            Ok
                (skewBinomialHeap (countL + countR) descendingL
                     (SBHTreeRoot.mergeRoots descendingL (SBHTreeRoot.normalize descendingL rootsL)
                          (SBHTreeRoot.normalize descendingL rootsR)))
        else
            Error "Can not merge two heaps with diferent comparison methods"

    ///O(n) - Returns heap from the sequence.
    let ofSeq descending s = s |> Seq.fold (fun heap value -> insert value heap) (empty descending)

    ///O(log n) - Returns a new heap with the front (head) element removed. Throws if empty.
    let tail ({ Count = count; Descending = descending; Roots = roots } as heap) =
        if isNotEmpty heap
        then skewBinomialHeap (count - 1) descending (SBHTreeRoot.uncons descending roots |> snd)
        else invalidOp "Empty heap, no tail"

    ///O(log n) - Returns Some h where h is the heap with the front (head) element removed.
    /// Returns None if the collection is empty.
    let maybeTail ({ Count = count; Descending = descending; Roots = roots } as heap) =
        if isNotEmpty heap
        then Some(skewBinomialHeap (count - 1) descending (SBHTreeRoot.uncons descending roots |> snd))
        else None

    ///O(log n) - Returns Ok h where h is the heap with the front (head) element removed.
    /// Returns Error if the collection is empty.
    let tryTail ({ Count = count; Descending = descending; Roots = roots } as heap) =
        if isNotEmpty heap
        then Ok(skewBinomialHeap (count - 1) descending (SBHTreeRoot.uncons descending roots |> snd))
        else Error "Empty heap, no tail"

    /// O(log n) - Returns the head element and tail. Throws if empty.
    let uncons ({ Count = count; Descending = descending; Roots = roots } as heap) =
        if isNotEmpty heap
        then let (h, t) = SBHTreeRoot.uncons descending roots in (h, skewBinomialHeap (count - 1) descending t)
        else invalidOp "Empty heap, no head and no tail"

    /// O(log n) - Returns Some (h, t) where h is the head and t is the tail.
    /// Returns None if the collection is empty.
    let maybeUncons ({ Count = count; Descending = descending; Roots = roots } as heap) =
        if isNotEmpty heap
        then let (h, t) = SBHTreeRoot.uncons descending roots in Some(h, skewBinomialHeap (count - 1) descending t)
        else None

    /// O(log n) - Returns Ok (h, t) where h is the head and t is the tail.
    /// Returns Error if the collection is empty.
    let tryUncons ({ Count = count; Descending = descending; Roots = roots } as heap) =
        if isNotEmpty heap
        then let (h, t) = SBHTreeRoot.uncons descending roots in Ok(h, skewBinomialHeap (count - 1) descending t)
        else Error "Empty heap, no head and no tail"

    /// O(n * log n) - Returns and ordered list of the elements in the heap.
    let toList { Descending = descending; Roots = roots } = SBHTreeRoot.toListOrdered descending roots

    let rec toSeq heap =
        if isEmpty heap then
            Seq.empty
        else
            let h, t = uncons heap
            seq {
                yield h
                yield! toSeq t
            }

    let toSeq1 heap =
        Seq.unfold maybeUncons heap

    let inline (|Cons|Nil|) heap =
        match maybeUncons heap with
        | Some(h, t) -> Cons(h, t)
        | None -> Nil

// type SkewBinomialHeap<'T when 'T: comparison> with
//     interface IEnumerable<'T> with
//         member this.GetEnumerator(): IEnumerator<_> =
