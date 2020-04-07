module Flux.Collections.Stream

open Expecto
open Expecto.Flip
open FsCheck
open Flux.TestUtils
open Flux.Collections.Stream

let asList laz =
    let rec asList' zal =
        function
        | Cons(x, xs) -> asList' (x :: zal) xs
        | Nil -> List.rev zal
    asList' [] laz

let rec private evaluate z =
    match z with
    | Nil -> ()
    | Cons(_, z') -> do evaluate z'

module Seq =
    let collecti f = Seq.mapi f >> Seq.collect id

module List =
    let collecti f = List.mapi f >> List.collect id

[<Tests>]
let tests =
    ftestList "Stream<_>"
        [ testCase "empty should create an empty stream" <| fun () ->
            match empty with
            | Cons _ -> fail "Stream was not empty"
            | Nil -> pass

          testProperty "singleton should create a stream with exactly the original item" <| fun x ->
              match singleton x with
              | Cons(x', Nil) -> x = x'
              | Cons(_, Cons _) -> failProp
              | Nil -> failProp

          testProperty "cons should add the item to the stream as the new head" <| fun (Finite l) x ->
              match cons x l with
              | Cons(x', l') -> x' = x && l' = l
              | _ -> fail ""

          testProperty "consDelayed should add the item to the stream as the new head" <| fun f x ->
              let ff() =
                  match f() with
                  | Finite l -> l
              match consDelayed x ff with
              | Cons(w, ws) -> w = x && ws = ff()
              | _ -> failProp

          testProperty "isEmpty returns whether the stream contains no elements" <| fun laz ->
              laz
              |> match laz with
                 | Cons _ -> isEmpty >> not
                 | Nil -> isEmpty

          testProperty "isNotEmpty returns whether the stream contains elements" <| fun laz ->
              laz
              |> match laz with
                 | Cons _ -> isNotEmpty
                 | Nil -> isNotEmpty >> not

          testProperty "head should return the first element of the stream" <| fun laz ->
              isNotEmpty laz ==> match laz with
                                 | Cons(h, _) -> head laz = h
                                 | _ -> passProp

          testCase "head should throw when the stream is empty"
          <| fun () ->
              Expect.throwsT<System.InvalidOperationException>
                  "Head on empty stream did not throw exception or the exception was not the expected one" (fun _ ->
                  head empty |> ignore)

          testProperty
              "maybeHead should return 'Some' with the first element of the stream or 'None' if the stream is empty" <| fun laz ->
              match laz, maybeHead laz with
              | Cons(h, _), Some h' -> h' = h
              | Nil, None -> passProp
              | _ -> failProp

          testCase "tail should throw when the stream is empty"
          <| fun () ->
              Expect.throwsT<System.InvalidOperationException>
                  "Tail on empty stream did not throw exception or the exception was not the expected one" (fun _ ->
                  tail empty |> ignore)

          testProperty
              "maybeTail should return 'Some' with the stream with the first element removed or 'None' if the stream is empty" <| fun (Finite laz) ->
              match laz, maybeTail laz with
              | Cons(_, t), Some t' -> t = t'
              | Nil, None -> passProp
              | _ -> failProp

          testProperty
              "uncons should return the head and tail pair if the stream is not empty and it should throw if the stream is empty" <| fun (Finite laz) ->
              match laz with
              | Cons p ->
                  let p' = uncons laz in p = p'
              | _ -> throws<System.InvalidOperationException, _> <| lazy (uncons laz)

          testProperty
              "maybeUncons should return 'Some' with the head-tail pair if the stream is not empty or 'None' if the stream is empty" <| fun (Finite laz) ->
              match laz, maybeUncons laz with
              | Cons p, Some p' -> p = p'
              | Nil, None -> passProp
              | _ -> failProp

          testProperty
              "last should return the last item if the stream is not empty or it should throw if the stream is empty" <| fun (Finite laz) ->
              if isEmpty laz then
                  throws<System.InvalidOperationException, _> <| lazy (last laz)
              else
                  laz
                  |> asList
                  |> List.last = last laz

          testProperty
              "maybeLast should return the last item wrapped in 'Some' if the stream is not empty or it should return 'None' otherwise" <| fun (Finite laz) ->
              laz
              |> asList
              |> List.tryLast = maybeLast laz

          testProperty "count should return the number of items in the stream"
          <| fun (Finite laz) -> Seq.length laz = count laz

          testProperty "length should return the number of items in the stream"
          <| fun (Finite laz) -> Seq.length laz = length laz

          testProperty "init creates a stream with the number of items specified using the item creator"
          <| fun (NonNegativeInt n) f -> asList (init n f) = (List.init n f)

          testProperty "init throws ArgumentException when the count number is negative"
          <| fun (PositiveInt n) f -> throws<System.ArgumentException, _> <| lazy (init (-n) f)

          testProperty "init does not evaluate the function" <| fun (NonNegativeInt n) f ->
              init n <| fun i ->
                  failwith "f"
                  f i
              |> isSuspended

          testProperty
              "initInfinite creates an infinite stream using the item supplier. The first m items are correct"
          <| fun (NonNegativeInt n) f -> infiniteSequenceEqual (initInfinite f) (Seq.initInfinite f)

          testProperty "initInfinite does not evaluate the function" <| fun f ->
              initInfinite <| fun i ->
                  failwith "f"
                  f i
              |> isSuspended

          testProperty
              "initWhile creates a stream with elements supplied by the function as long as its result is 'Some'" <| fun (NonNegativeInt n) f ->
              let f' =
                  function
                  | i when i = n -> None
                  | i -> Some(f i)
              initWhile f'
              |> asList = List.init n f

          testProperty "initWhile does not evaluate the function" <| fun f ->
              initWhile <| fun i ->
                  failwith "f"
                  f i
              |> isSuspended

          testProperty "replicate creates a stream with the supplied item repeated the number of times specified"
          <| fun (NonNegativeInt n) v ->
              replicate n v
              |> asList = List.replicate n v

          testProperty "replicate throws ArgumentException when the count number is negative"
          <| fun (PositiveInt n) v -> throws<System.ArgumentException, _> <| lazy (replicate (-n) v)

          testProperty
              "append creates a stream that contains the elements of the first stream followed by the elements of the second stream"
          <| fun (Finite a) (Finite b) ->
              append a b
              |> asList = List.append (asList a) (asList b)

          testProperty "append works with infinite lists"
          <| fun (Infinite a) b -> infiniteSequenceEqual (append a b) a

          testProperty "append does not evaluate the original list"
          <| fun y z -> (isSuspended z && isSuspended y) ==> (append y z |> isSuspended)

          testProperty "concat creates a stream that contains the elements of each stream in the same other" <| fun (NonNegativeInt n) f ->
              let list = init n (f >> finite)
              concat list
              |> asList =
                  List.concat
                      (Seq.init n
                           (f
                            >> finite
                            >> asList))

          testProperty "concat works with infinite stream" <| fun (PositiveInt n) f ->
              let list = init n (f >> infinite)
              infiniteSequenceEqual (concat list) (f 0 |> infinite)

          testProperty "concat works with infinite stream of stream" <| fun f ->
              let list = initInfinite f
              infiniteSequenceEqual (concat list) (Seq.initInfinite f |> Seq.concat)

          testProperty "concat does not evaluate the original stream of stream"
          <| fun z -> isSuspended z ==> (concat z |> isSuspended)

          testProperty
              "collect creates a stream with the concatenation of the streams resulted from mapping each element of the original list to a stream" <| fun (Finite z) f ->
              collect (f >> finite) z
              |> asList =
                  (asList z
                   |> List.collect
                       (f
                        >> finite
                        >> asList))

          testProperty "collect works with infinite lists"
          <| fun z f -> infiniteSequenceEqual (collect (f >> infinite) z) (Seq.collect (f >> infinite) z)

          testProperty "collect does not evaluate the original list"
          <| fun f s z -> isSuspended z ==> (collect f z |> isSuspended)

          testProperty
              "collecti creates a stream with the concatenation of the streams resulted from mapping each element of the original stream to a stream" <| fun (Finite z) f ->
              collecti (fun i -> f i >> finite) z
              |> asList =
                  (asList z
                   |> List.collecti (fun i ->
                       f i
                       >> finite
                       >> asList))

          testProperty "collecti works with infinite stream"
          <| fun z f ->
              infiniteSequenceEqual (collecti (fun i -> f i >> infinite) z)
                  (z |> Seq.collecti (fun i -> f i >> infinite))

          testProperty "collecti does not evaluate the original stream"
          <| fun f s z -> isSuspended z ==> (collecti f z |> isSuspended)

          testProperty "fold is consistent with Seq's fold" <| fun (Finite z) f s -> fold f s z = Seq.fold f s z

          testProperty "foldBack is consistent with Seq's fold"
          <| fun (Finite z) f s -> foldBack f z s = Seq.foldBack f z s

          testProperty "scan is consistent with List's scan"
          <| fun (Finite z) f s ->
              scan f s z
              |> asList = List.scan f s (asList z)

          testProperty "scan scans infinite stream"
          <| fun (Infinite z) f s -> infiniteSequenceEqual (scan f s z) (Seq.scan f s z)

          testProperty "scan returns an unevaluated stream" <| fun z f s -> scan f s z |> isSuspended

          testProperty "scan does not evaluate the original stream"
          <| fun f s z -> isSuspended z ==> (scan f s z |> isSuspended)

          testProperty "scanBack is consistent with List's scanBack"
          <| fun (Finite z) f s ->
              scanBack f z s
              |> asList = List.scanBack f (asList z) s

          testProperty "scanBack does not evaluate the original stream"
          <| fun f s z -> isSuspended z ==> (scanBack f z s |> isSuspended)

          testProperty "reduce is consistent with List's reduce"
          <| fun (NonEmpty z) f -> reduce f z = List.reduce f (asList z)

          testProperty "reduce throws when the stream is empty"
          <| fun f -> throws<System.InvalidOperationException, _> <| lazy (reduce f empty)

          testProperty "reduceBack is consistent with List's reduceBack"
          <| fun (NonEmpty z) f -> reduceBack f z = List.reduceBack f (asList z)

          testProperty "reduceBack throws when the stream is empty"
          <| fun f -> throws<System.InvalidOperationException, _> <| lazy (reduceBack f empty)

          testProperty "unfold is consistent with List's unfold" <| fun f g s (PositiveInt max) ->
              let f' (index, state) =
                  if index = max then None else Some(f state, (index + 1, g state))
              unfold f' (0, s)
              |> asList = List.unfold f' (0, s)

          testProperty "choose is consistent with List's choose"
          <| fun f (Finite z) ->
              choose f z
              |> asList = List.choose f (asList z)

          testProperty "choose chooses in a infinite stream" <| fun f (Infinite z) -> infiniteSequenceEqual (choose f z) (Seq.choose f z)

          testProperty "choose returns an unevaluated stream" <| fun f z -> choose f z |> isSuspended

          testProperty "choose does not evaluate the original stream"
          <| fun f z -> isSuspended z ==> (choose f z |> isSuspended)

          testProperty "filter is consistent with List's filter"
          <| fun f (Finite z) ->
              filter f z
              |> asList = List.filter f (asList z)

          testProperty "filter filters an infinite stream"
          <| fun f (Infinite z) -> infiniteSequenceEqual (filter f z) (Seq.filter f z)

          testProperty "filter returns an unevaluated stream" <| fun f z -> filter f z |> isSuspended

          testProperty "filter does not evaluate the original stream"
          <| fun f z -> isSuspended z ==> (filter f z |> isSuspended)

          testProperty "exists is consistent with List's exists"
          <| fun f (Finite z) -> exists f z = List.exists f (asList z)

          testProperty "exists2 is consistent with Seq's exists"
          <| fun f (Finite z1) (Finite z2) -> exists2 f z1 z2 = Seq.exists2 f z1 z2

          testProperty "forall is consistent with List's forall"
          <| fun f (Finite z) -> forall f z = List.forall f (asList z)

          testProperty "forall2 is consistent with Seq's forall2"
          <| fun f (Finite z1) (Finite z2) -> forall2 f z1 z2 = Seq.forall2 f z1 z2

          testProperty "iter is consistent with List's iter" <| fun (Finite z) ->
              let mutable s1 = ""
              let mutable s2 = ""
              let f1 a = s1 <- sprintf "%s%A" s1 a
              let f2 a = s2 <- sprintf "%s%A" s2 a
              do iter f1 z
                 List.iter f2 (asList z)
              s1 = s2

          testProperty "iter2 is consistent with Seq's iter2" <| fun (Finite z1) (Finite z2) ->
              let mutable s1 = ""
              let mutable s2 = ""
              let f1 a b = s1 <- sprintf "%s%A%A" s1 a b
              let f2 a b = s2 <- sprintf "%s%A%A" s2 a b
              do iter2 f1 z1 z2
                 Seq.iter2 f2 (asList z1) (asList z2)
              s1 = s2

          testProperty "iteri is consistent with List's iteri" <| fun (Finite z) ->
              let mutable s1 = ""
              let mutable s2 = ""
              let f1 i a = s1 <- sprintf "%s%A%A" s1 i a
              let f2 i a = s2 <- sprintf "%s%A%A" s2 i a
              do iteri f1 z
                 List.iteri f2 (asList z)
              s1 = s2

          testProperty "map is consistent with List's map" <| fun f (Finite z) ->
              map f z
              |> asList = List.map f (asList z)

          testProperty "map works with infinite streams"
          <| fun f (Infinite z) -> infiniteSequenceEqual (map f z) (Seq.map f z)

          testProperty "map returns a suspended stream" <| fun f z -> map f z |> isSuspended

          testProperty "map does not evaluate the original list"
          <| fun f z -> isSuspended z ==> (map f z |> isSuspended)

          testProperty "mapi is consistent with List's map" <| fun f (Finite z) ->
              mapi f z
              |> asList = List.mapi f (asList z)

          testProperty "mapi works with infinite streams"
          <| fun f (Infinite z) -> infiniteSequenceEqual (mapi f z) (Seq.mapi f z)

          testProperty "mapi returns a suspended stream" <| fun f z -> mapi f z |> isSuspended

          testProperty "mapi does not evaluate the original stream"
          <| fun f z -> isSuspended z ==> (mapi f z |> isSuspended)

          testProperty "map2 is consistent with Seq's map2"
          <| fun f (Finite z1) (Finite z2) ->
              map2 f z1 z2
              |> asList = (Seq.map2 f (asList z1) (asList z2) |> Seq.toList)

          testProperty "map2 works with infinite streams"
          <| fun f (Infinite z1) (Infinite z2) ->
              infiniteSequenceEqual (map2 f z1 z2) (Seq.map2 f z1 z2)

          testProperty "map2 returns a suspended list" <| fun f z1 z2 -> map2 f z1 z2 |> isSuspended

          testProperty "map2 does not evaluate the original stream"
          <| fun f z1 z2 -> (isSuspended z1 && isSuspended z2) ==> (map2 f z1 z2 |> isSuspended)

          testProperty "zip is consistent with Seq's zip"
          <| fun (Finite z1) (Finite z2) -> List.ofSeq (zip z1 z2) = List.ofSeq (Seq.zip z1 z2)

          testProperty "zip works with infinite streams"
          <| fun f (Infinite z1) (Infinite z2) -> infiniteSequenceEqual (zip z1 z2) (Seq.zip z1 z2)

          testProperty "zip returns a suspended stream" <| fun f z1 z2 -> zip z1 z2 |> isSuspended

          testProperty "zip does not evaluate the original stream"
          <| fun f z1 z2 -> (isSuspended z1 && isSuspended z2) ==> (zip z1 z2 |> isSuspended)

          testProperty "zip3 is consistent with Seq's zip3"
          <| fun (Finite z1) (Finite z2) (Finite z3) ->
              List.ofSeq (zip3 z1 z2 z3) = List.ofSeq (Seq.zip3 z1 z2 z3)

          testProperty "zip3 works with infinite streams"
          <| fun f (Infinite z1) (Infinite z2) (Finite z3) ->
              infiniteSequenceEqual (zip3 z1 z2 z3) (Seq.zip3 z1 z2 z3)

          testProperty "zip3 returns a suspended stream" <| fun f z1 z2 z3 -> zip3 z1 z2 z3 |> isSuspended

          testProperty "zip3 does not evaluate the original stream"
          <| fun f z1 z2 z3 ->
              (isSuspended z1 && isSuspended z2 && isSuspended z3) ==> (zip3 z1 z2 z3 |> isSuspended)

          testProperty "truncate is consistent with Seq's truncate"
          <| fun z n -> List.ofSeq (truncate n z) = List.ofSeq (Seq.truncate n z)

          testProperty "take throws when asked to take a negative number of items"
          <| fun z (PositiveInt n) -> throws<System.ArgumentException, _> <| lazy (take (-n) z)

          testProperty "take throws when asked to take more elements than exists in the stream"
          <| fun (Sized(z, size)) n ->
              n > size ==> lazy (throws<System.ArgumentException, _> <| lazy (take n z |> evaluate))

          testProperty "take is consistent with Seq's take when there are enough elements to be taken"
          <| fun (Sized(z, size)) (NonNegativeInt n) ->
              n <= size ==> lazy (List.ofSeq (take n z) = List.ofSeq (Seq.take n z))

          testProperty "takeWhile is consistent with Seq's takeWhile"
          <| fun f z -> infiniteSequenceEqual (takeWhile f z) (Seq.takeWhile f z)

          testProperty "drop returns the same list when asked to drop a negative or zero number of elements"
          <| fun (Finite z) n -> n <= 0 ==> (drop n z = z)

          testProperty "drop is consistent with List's skip when there are enough elements in the stream to be dropped"
          <| fun (Sized(z, size)) (NonNegativeInt n) ->
              n <= size ==> lazy (drop n z
                                  |> asList = List.skip n (asList z))

          testProperty "drop returns empty when asked to drop more elements than exists in the stream"
          <| fun (Sized(z, size)) (NonNegativeInt n) -> n > size ==> (drop n z = empty)

          testProperty "drop only evaluates the dropped elements" <| fun (Infinite z) (NonNegativeInt n) ->
              do drop n z
                 |> head
                 |> ignore
              Seq.unfold (fun s ->
                  if not (isSuspended s) then Some((), tail s) else None) z
              |> Seq.length = n + 1

          testProperty "skip throws when asked to skip a negative number of elements" <| fun z (PositiveInt n) -> 
            throws<System.ArgumentException, _> <| lazy (skip -n z)

          testProperty "skip is consistent with List's skip when there are enough elements in the stream to be skipped"
          <| fun (Sized(z, size)) (NonNegativeInt n) ->
              n <= size ==> lazy (skip n z
                                  |> asList = List.skip n (asList z))

          testProperty "skip throws when asked to skip more elements than exists in the stream"
          <| fun (Sized(z, size)) (NonNegativeInt n) ->
              n > size ==> lazy (throws<System.ArgumentException, _> <| lazy (skip n z |> evaluate))

          testProperty "skip only evaluates the skipped elements" <| fun (Infinite z) (NonNegativeInt n) ->
              do skip n z
                 |> head
                 |> ignore
              Seq.unfold (fun s ->
                  if not (isSuspended s) then Some((), tail s) else None) z
              |> Seq.length = n + 1

          testProperty "maybeSkip returns the same stream when asked to skip a negative or zero number of elements"
          <| fun (Finite z) (PositiveInt n) -> maybeSkip -n z = Some z

          testProperty "maybeSkip is consistent with skip when there are enough elements in the stream"
          <| fun (Sized(z, size)) (NonNegativeInt n) -> n <= size ==> lazy (maybeSkip n z = Some(skip n z))

          testProperty "maybeSkip returns None when asked to skip more elements that exists in the stream"
          <| fun (Sized(z, size)) (NonNegativeInt n) -> n > size ==> (maybeSkip n z = None)

          testProperty "maybeSkip only evaluates the skipped elements" <| fun (Infinite z) (NonNegativeInt n) ->
              do maybeSkip n z
                 |> Option.get
                 |> head
                 |> ignore
              Seq.unfold (fun s ->
                  if not (isSuspended s) then Some((), tail s) else None) z
              |> Seq.length = n + 1

          testProperty "skipWhile is consistent with List's skipWhile"
          <| fun (Finite z) f ->
              skipWhile f z
              |> asList = List.skipWhile f (asList z)

          testProperty "skipWhile only evaluates the skipped elements and the first element of the remaining stream" <| fun (Infinite z) f ->
              let z' = mapi (fun index item -> (index, item)) z
              let (n, _) = skipWhile f z' |> head
              Seq.unfold (fun s ->
                  if not (isSuspended s) then Some((), tail s) else None) z
              |> Seq.length = n + 1

          testProperty "rev is consistent with List's rev" <| fun (Finite z) ->
              rev z
              |> asList = List.rev (asList z)

          testProperty "item throws when the queried index is out of bounds"
          <| fun (Sized(z, size)) n ->
              "under bounds" @| (n < 0 ==> (throws<System.ArgumentException, _> <| lazy (item n z)))
              .|. "over bounds A" @| (n > size ==> (throws<System.ArgumentException, _> <| lazy (item n z)))
              .|. "over bounds B" @| (n = size ==> (throws<System.InvalidOperationException, _> <| lazy (item n z)))

          testProperty "item is consistent with Seq's item when the queried index is in bounds"
          <| fun (Infinite z) (NonNegativeInt n) -> item n z = Seq.item n z

          testList "Laziness preservation"
              [ testProperty "cons should not evaluate the stream" <| fun l x ->
                  isSuspended l ==> (l
                                     |> cons x
                                     |> tail
                                     |> isSuspended)

                testProperty "consDelayed should not evaluate the stream" <| fun f x ->
                    isSuspended (f()) ==> (f
                                           |> consDelayed x
                                           |> tail
                                           |> isSuspended)

                testProperty "uncons should not evaluate the tail of the stream" <| fun l x ->
                    isSuspended l ==> (l
                                       |> cons x
                                       |> uncons
                                       |> snd
                                       |> isSuspended)

                testProperty "tryUncons should not evaluate the tail of the stream" <| fun l x ->
                    isSuspended l ==> (l
                                       |> cons x
                                       |> maybeUncons
                                       |> Option.get
                                       |> snd
                                       |> isSuspended) ] ]
