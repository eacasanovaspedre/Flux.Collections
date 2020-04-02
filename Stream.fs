namespace Flux.Collections

type private 'T Suspended =
    | Value of 'T
    | Generator of (unit -> 'T)
    | Failure of exn

type private 'T Cell = 
    | Cons of 'T * 'T Stream
    | Nil

and 'T Stream = private { mutable DelayedCell: 'T Cell Suspended }

exception ElementNotFoundException of string

module private Cell =
    
    let inline isNil c = match c with | Nil _ -> true | _ -> false
    let inline map f c = match c with | Cons (v, z) -> Cons (f (v, z)) | Nil -> Nil
    let inline map2 f c1 c2 = match c1, c2 with | Cons (v1, z1), Cons (v2, z2) -> Cons (f (v1, z1) (v2, z2)) | _ -> Nil
    let inline map3 f c1 c2 c3 = match c1, c2, c3 with | Cons (v1, z1), Cons (v2, z2), Cons (v3, z3) -> Cons (f (v1, z1) (v2, z2) (v3, z3)) | _ -> Nil
    let inline mapToOption f c = match c with | Cons (v, z) -> Some (f (v, z)) | Nil -> None
    let inline bindToOption f c = match c with | Cons (v, z) -> f (v, z) | Nil -> None

module Stream =

    [<AutoOpen>]
    module private Helpers =

        let inline stream delayedCell = { DelayedCell = delayedCell }

        let cell z =
            let inline delayedCell { DelayedCell = delayedCell } = delayedCell
            let inline mutateCell newDelayedCell (z: 'T Stream) = z.DelayedCell <- newDelayedCell

            match delayedCell z with
            | Value v -> v
            | _ ->
                System.Threading.Monitor.Enter z
                try
                    match delayedCell z with
                    | Generator f ->
                        try
                            let value = f ()
                            mutateCell (Value value) z
                            value
                        with ex ->
                            mutateCell (Failure ex) z
                            reraise ()
                    | Value v -> v
                    | Failure ex -> raise ex
                finally
                    System.Threading.Monitor.Exit z

        let inline fromCell cell = stream (Value cell)

        let inline suspended generator = stream (Generator generator)

        let inline delayed streamGenerator = streamGenerator >> cell |> suspended

    let empty<'T> = stream (Value (Cell<'T>.Nil))

    let isEmpty z = z |> cell |> Cell.isNil

    let inline isNotEmpty z = z |> isEmpty |> not

    let singleton v = fromCell (Cons (v, empty))

    let head z =
        match cell z with
        | Cons (h, _) -> h
        | _           -> invalidOp "Empty LazyList, no head"

    let maybeHead z = (cell >> Cell.mapToOption fst) z

    let tail z =
        match cell z with
        | Cons (_, tail) -> tail
        | _              -> invalidOp "Empty LazyList, no tail."

    let maybeTail z = (cell >> Cell.mapToOption snd) z

    let uncons z =
        match cell z with
        | Cons (h, t) -> h, t
        | _           -> invalidOp "Empty LazyList, no head and no tail."

    let maybeUncons z = (cell >> Cell.mapToOption id) z

    let maybeLast z =
        let rec last' z prev =
            match cell z with
            | Cons (x, z') -> last' z' x
            | Nil          -> prev
        match cell z with
        | Nil          -> None
        | Cons (x, z') -> Some (last' z' x)

    let last z =
        match maybeLast z with
        | Some l -> l
        | None -> invalidOp "Empty LazyList, no last element."

    let count z =
        let rec count a z =
            match cell z with
            | Cons (_, z') -> count (a + 1) z'
            | Nil         -> a
        count 0 z
            
    let length = count

    let init n f =
        let rec initi i =
            suspended <| fun _ ->
                if i = n 
                    then Nil
                    else Cons (f i, initi (i + 1))
        if n < 0 
            then invalidArg "n" "The argument has to be non negative."
            else initi 0

    let initInfinite f =
        let rec initi i =
            suspended <| fun _ -> Cons (f i, initi (i + 1))
        initi 0

    let initWhile f =
        let rec initi i =
            suspended <| fun _ ->
                match f i with
                | Some v -> Cons (v, initi (i + 1))
                | None   -> Nil
        initi 0

    let replicate n v =
        do if n < 0 then invalidArg "n" "The argument has to be non negative."
        let rec replicate' n =
            if n = 0 then empty
            else
                suspended <| fun _ -> Cons (v, replicate' (n - 1))
        replicate' n

    // TODO: Explain in the documentation the problem of calling append over another append creating a chain of appends
    // In [append (append (append A B) C) D], to reach the first cell of A the number of suspensions and evaluations is equal to the number of appends, and so for the second, third ... nth cell of A. Whenever possible it is better to append right associative [append A (append B (append C D))] or use any of the concatenation functions.
    let rec append z t =
        suspended <| fun _ ->
            match cell z with
            | Cons (x, z') -> Cons (x, append z' t)
            | Nil          -> cell t

    let rec concat lazyLists =
        delayed <| fun _ ->
            match cell lazyLists with
            | Cons (z, zz) -> append z (concat zz)
            | Nil          -> empty

    let rec collect f z =
        delayed <| fun _ ->
            match cell z with
            | Cons (x, z') -> append (f x) (collect f z')
            | Nil          -> empty

    let collecti f z =
        let rec collecti' i z =
            delayed <| fun _ ->
                match cell z with
                | Cons (x, z') -> append (f i x) (collecti' (i + 1) z')
                | Nil          -> empty
        collecti' 0 z

    let rec fold f s z =
        match cell z with
        | Cons (x, z') -> fold f (f s x) z'
        | Nil          -> s

    let foldBack f z =
        let rec foldBack' z g =
            match cell z with
            | Cons (x, z') -> foldBack' z' <| f x >> g //fun s -> g (f x s) is faster see reduceBack
            | Nil          -> g
        foldBack' z id

    let rec scan f s z =
        suspended <| fun _ ->
            match cell z with
            | Cons (x, z') -> Cons (s, scan f (f s x) z')
            | Nil          -> Cons (s, empty)

    let scanBack f z s =
        let rec toListRev acc z =
            match cell z with
            | Nil         -> acc
            | Cons(x, xs) -> toListRev (x::acc) xs
        let rec fold last acc = function
            | []    -> acc
            | x::xs -> let next = f x last in fold next (Cons (next, acc |> fromCell)) xs
        suspended <| fun _ -> z |> toListRev [] |> fold s (Cons (s, empty))

    let reduce f z =
        let rec reduce' z prev =
            match cell z with
            | Cons (x, z') -> reduce' z' (f prev x)
            | Nil          -> prev
        match cell z with
        | Cons (x, z') -> reduce' z' x
        | Nil          -> invalidOp "The LazyList is empty, can not reduce."

    let reduceBack f z =
        let rec reduceBack' z left g =
            match cell z with
            | Cons (x, z') -> reduceBack' z' x <| fun right -> g (f left right)
            | Nil          -> g left
        match cell z with
        | Cons (x, z') -> reduceBack' z' x id
        | Nil          -> invalidOp "The LazyList is empty, can not reduceBack."

    let rec unfold f s =
        suspended <| fun _ ->
            match f s with
            | Some (v, s') -> Cons (v, unfold f s')
            | None         -> Nil

    let rec choose f z =
        suspended (nextChosenOrNil f z)
    and private nextChosenOrNil f z _ =
        match cell z with
        | Cons (x, z') ->
            match f x with
            | Some v -> Cons (v, choose f z')
            | None   -> nextChosenOrNil f z' ()
        | Nil -> Nil

    let rec filter f z =
        suspended (nextAccepted f z)
    and private nextAccepted f z _ =
        match cell z with
        | Cons (x, z') when f x -> Cons (x, filter f z')
        | Cons (_, z')          -> nextAccepted f z' ()
        | Nil                  -> Nil

    let rec exists f s =
        match cell s with
        | Cons (x, _) 
            when f x   -> true
        | Cons (_, z') -> exists f z'
        | Nil          -> false

    let rec exists2 f z1 z2 =
        match cell z1, cell z2 with
        | Cons (x1, _), Cons (x2, _) 
            when f x1 x2              -> true
        | Cons (_, z1'), Cons(_, z2') -> exists2 f z1' z2'
        | _                           -> false

    let rec forall f z =
        match cell z with
        | Cons (x, z')
            when f x -> forall f z'
        | Cons _     -> false
        | Nil        -> true

    let rec forall2 f z1 z2 =
        match cell z1, cell z2 with
        | Cons (x1, z1'), Cons (x2, z2')
            when f x1 x2      -> forall2 f z1' z2'
        | Cons _, Cons _      -> false
        | _                   -> true

    let rec iter f z =
        match cell z with
        | Cons (x, z') -> f x; iter f z'
        | Nil          -> ()

    let rec iter2 f z1 z2 =
        match cell z1, cell z2 with
        | Cons (x1, z1'), Cons (x2, z2') -> f x1 x2; iter2 f z1' z2'
        | _                              -> ()

    let iteri f z =
        let rec iteri' f z i =
            match cell z with
            | Cons (x, z') -> f i x; iteri' f z' (i + 1)
            | Nil          -> ()
        iteri' f z 0

    let rec map f z =
        suspended <| fun _ ->
            cell z |> Cell.map (fun (x, z') -> f x, map f z')

    let mapi f z =
        let rec map' z i =
            suspended <| fun _ ->
                cell z |> Cell.map (fun (x, z') -> (f i x, map' z' (i + 1)))
        map' z 0

    let rec map2 f z1 z2 =
        suspended <| fun _ ->
            (cell z1, cell z2) ||> Cell.map2 (fun (x1, z1') (x2, z2') -> (f x1 x2, map2 f z1' z2'))

    let rec zip z1 z2 =
        suspended <| fun _ ->
            (cell z1, cell z2) ||> Cell.map2 (fun (x1, z1') (x2, z2') -> (x1, x2), (zip z1' z2'))

    let rec zip3 z1 z2 z3 =
        suspended <| fun _ ->
            (cell z1, cell z2, cell z3) |||> Cell.map3 (fun (x1, z1') (x2, z2') (x3, z3') -> (x1, x2, x3), (zip3 z1' z2' z3'))

    let rec truncate n z =
        suspended <| fun _ ->
            if n <= 0 then
                Nil
            else 
                match cell z with
                | Cons (x, z') -> Cons (x, truncate (n - 1) z')
                | Nil          -> Nil

    let take n z =
        do if n < 0 then invalidArg "n" "Can not take a negative number of elements."
        let rec take' n z =
            suspended <| fun _ ->
                if n = 0 then Nil
                else
                    match cell z with
                    | Cons (x, z') -> Cons (x, take' (n - 1) z')
                    | Nil          -> invalidArg "n" "Insufficient number of elements. Can no take more elements."
        take' n z

    let takeWhile f z =
        let rec takeWhile' z =
            suspended <| fun _ ->
                match cell z with
                | Cons (x, z') when f x -> Cons (x, takeWhile' z')
                | _                     -> Nil
        takeWhile' z

    let drop n z =
        let rec drop' n z =
            match n, cell z with
            | 1, Cons (_, z') -> z'
            | _, Cons (_, z') -> drop' (n - 1) z'
            | _, Nil          -> empty
        if n <= 0 then z
        else
            delayed <| fun _ -> drop' n z

    let skip n z =
        let rec skip' n z =
            match n, cell z with
            | 1, Cons (_, z') -> z'
            | _, Cons (_, z') -> skip' (n - 1) z'
            | _, Nil          -> invalidArg "n" "Insufficient number of elements."
        if n < 0 
            then invalidArg "n" "Can not skip a negative number of elements."
        elif n = 0
            then z
            else delayed <| fun _ -> skip' n z

    let rec maybeSkip n z =
        if n <= 0 
            then Some z
            else cell z |> Cell.bindToOption (snd >> maybeSkip (n - 1))

    let skipWhile f z =
        let rec skipWhile' z _ =
            match cell z with
            | Cons (x, z') when f x -> skipWhile' z' ()
            | cell                  -> cell
        suspended <| skipWhile' z

    let rev z =
        let rec reverse' z r =
            match cell z with
            | Cons (x, z') -> reverse' z' (Cons (x, r) |> fromCell)
            | Nil          -> r
        reverse' z empty

    let item n = head << skip n

    let rec find f z =
        match cell z with
        | Cons (x, _) when f x -> x
        | Cons (_, z')         -> find f z'
        | Nil                  -> raise (ElementNotFoundException "No element that satisfies the given predicate was found in the collection.")

    let rec maybeFind f z =
        match cell z with
        | Cons (x, _) when f x -> Some x
        | Cons (_, z')         -> maybeFind f z'
        | Nil                  -> None

    let rec pick f z =
        match cell z with
        | Cons (x, z') ->
            match f x with
            | Some v -> v
            | None   -> pick f z'
        | Nil -> raise (ElementNotFoundException "No element that satisfies the given chooser was found in the collection.")

    let rec maybePick f z =
        match cell z with
        | Cons (x, z') ->
            match f x with
            | Some v -> v
            | None   -> maybePick f z'
        | Nil -> None

    let findIndex f z =
        let rec findIndex' i z =
            match cell z with
            | Cons (x, _) when f x -> i
            | Cons (_, z')         -> findIndex' (i + 1) z'
            | Nil                  -> raise (ElementNotFoundException "No element that satisfies the given predicate was found in the collection.")
        findIndex' 0 z

    let maybeFindIndex f z =
        let rec maybeFindIndex' i z =
            match cell z with
            | Cons (x, _) when f x -> Some i
            | Cons (_, z')         -> maybeFindIndex' (i + 1) z'
            | Nil                  -> None
        maybeFindIndex' 0 z

    let findIndex64 f z =
        let rec findIndex64' i z =
            match cell z with
            | Cons (x, _) when f x -> i
            | Cons (_, z')         -> findIndex64' (i + 1L) z'
            | Nil                  -> raise (ElementNotFoundException "No element that satisfies the given predicate was found in the collection.")
        findIndex64' 0L z

    let maybeFindIndex64 f z =
        let rec maybeFindIndex64' i z =
            match cell z with
            | Cons (x, _) when f x -> Some i
            | Cons (_, z')         -> maybeFindIndex64' (i + 1L) z'
            | Nil                  -> None
        maybeFindIndex64' 0L z

    let exactlyOne z =
        match cell z with
        | Cons (x, z') 
            when isEmpty z' -> x
        | Cons _            -> invalidOp "The supplied LazyList contains more than one element."
        | Nil               -> invalidOp "The supplied LazyList is empty."

    let ofArray array =
        let rec ofArray' i =
            suspended <| fun _ ->
                if i < Array.length array
                    then Cons (array.[i], ofArray' (i + 1))
                    else Nil
        ofArray' 0

    let toArray z =
        let array = Array.zeroCreate (count z)
        let rec toArray' i z =
            match cell z with
            | Cons (x, z') -> array.[i] <- x; toArray' (i + 1) z'
            | Nil -> ()
        do toArray' 0 z
        array

    let rec ofList list =
        suspended <| fun _ ->
            match list with
            | x::xs -> Cons (x, ofList xs)
            | []    -> Nil

    // Explain in the documentation that the enumerator will be in use as long as the LazyList produced here is not fully evaluated or recycled by the GC, so, if this enumerable references a mutable collection, that collection must not be changed, otherwise the enumerator could become corrupted.
    // Also the enumerator may not be disposed
    let ofSeq (seq: _ seq) =
        let e = seq.GetEnumerator ()
        let rec create _ =
            suspended <| fun _ ->
                if e.MoveNext () then 
                    Cons (e.Current, create ())
                else
                    do e.Dispose ()
                    Nil
        create ()

    // This version doesn't have the problem of ofSeq, but can't be used with an infinite sequence
    let ofSeqSecure z = (Seq.toList >> ofList) z

    let toSeq z = z |> Seq.unfold (fun z -> cell z |> Cell.mapToOption id)

    let toList z = z |> toSeq |> List.ofSeq

    let internal hashNodes = 15

    let internal computeHash (ec: System.Collections.IEqualityComparer) z =
        let rec computeHash' z i hash =
            if i >= hashNodes then hash
            else
                match cell z with
                | Cons (x, z') ->
                    let t = ec.GetHashCode x
                    computeHash' z' (i + 1) ((hash * 397) ^^^ (t + i + 1)) //created by me, I don't know how good it is, this is now a TODO: research on the best hashing methods
                | Nil -> hash
        computeHash' z 0 0

    let internal areEqual (ec: System.Collections.IEqualityComparer) z1 z2 =
        if obj.ReferenceEquals (z1, z2) then true
        else
            let rec areEqual' z1 z2 =
                match cell z1, cell z2 with
                | Cons (x1, _), Cons (x2, _) 
                    when not (ec.Equals (x1, x2)) -> false
                | Cons (_, z1'), Cons (_, z2')          -> areEqual' z1' z2'
                | Nil, Nil                              -> true
                | _                                     -> false
            areEqual' z1 z2

    let internal compare (c: System.Collections.IComparer) z1 z2  =
        if obj.ReferenceEquals (z1, z2) then 0
        else
            let rec compare' z1 z2 =
                match cell z1, cell z2 with
                | Cons (x1, z1'), Cons (x2, z2') -> 
                    match c.Compare (x1, x2) with
                    | r when r = 0 -> compare' z1' z2'
                    | r            -> r
                | Nil, Nil -> 0
                | Nil, _   -> -1
                | _        -> 1                             
            compare' z1 z2

    let cons x z = Cons (x, z) |> fromCell //if this cell is never used this is a bit slower than using suspended, but a lot faster if used

    let consDelayed x f = (x, delayed f) |> Cons |> fromCell

    let delayedFromPair f = suspended <| fun _ -> let (h, t) = f () in Cons (h, t)

    let delayed g = delayed g

    [<AutoOpen>]
    module ActivePattern =
        //This is down here to avoid using it in any of the functions above.
        let (|Cons|Nil|) (z: 'T Stream) = match cell z with | Cell.Cons (x, z') -> Choice1Of2 (x, z') | Cell.Nil -> Choice2Of2 ()