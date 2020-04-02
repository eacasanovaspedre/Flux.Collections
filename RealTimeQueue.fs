namespace Flux.Collections

type 'T RTQueue =
    private { Front: 'T Stream
              Back: 'T list
              Schedule: 'T Stream } // |S| = |F| - |R|

module RTQueue =

    module private Helpers =

        let rec private rotate f r acc =
            Stream.delayed <| fun _ ->
                match r with
                | [ y ] when Stream.isEmpty f -> Stream.cons y acc
                | y :: r' when Stream.isNotEmpty f ->
                    let (x, f') = Stream.uncons f in Stream.cons x (rotate f' r' (Stream.cons y acc))
                | _ -> failwith "This should never happen"

        let rtQueue f b s =
            // match s with
            // | Cons (x, s) -> { Front = f; Back = b; Schedule = s }
            // | Nil -> let f' = rotate f b Stream.empty in { Front = f'; Back = []; Schedule = f' }
            if Stream.isEmpty s then
                let f' = rotate f b Stream.empty
                { Front = f'
                  Back = []
                  Schedule = f' }
            else
                { Front = f
                  Back = b
                  Schedule = Stream.tail s }

    exception NoHeadInEmptyRTQueueException

    exception NoTailInEmptyRTQueueException

    type NoHeadInEmptyRTQueue = NoHeadInEmptyRTQueue

    type NoTailInEmptyRTQueue = NoTailInEmptyRTQueue

    let empty<'T> =
        { Front = Stream.empty<'T>
          Back = []
          Schedule = Stream.empty<_> }

    let isEmpty { Front = f } = Stream.isEmpty f

    let snoc x { Front = f; Back = r; Schedule = s } = Helpers.rtQueue f (x :: r) s

    let head { Front = f } =
        if Stream.isNotEmpty f then Stream.head f else raise NoHeadInEmptyRTQueueException

    let tryHead { Front = f } =
        if Stream.isNotEmpty f then
            f
            |> Stream.head
            |> Ok
        else
            Error NoHeadInEmptyRTQueue

    let maybeHead { Front = f } =
        if Stream.isNotEmpty f then
            f
            |> Stream.head
            |> Some
        else
            None

    let tail { Front = f; Back = r; Schedule = s } =
        if Stream.isNotEmpty f then Helpers.rtQueue (Stream.tail f) r s else raise NoTailInEmptyRTQueueException

    let tryTail { Front = f; Back = r; Schedule = s } =
        if Stream.isNotEmpty f
        then Ok(Helpers.rtQueue (Stream.tail f) r s)
        else Error NoTailInEmptyRTQueue

    let maybeTail { Front = f; Back = r; Schedule = s } =
        if Stream.isNotEmpty f
        then Some(Helpers.rtQueue (Stream.tail f) r s)
        else None

    let inline ofList list =
        let rec loop acc =
            function
            | x :: xs -> xs |> loop (snoc x acc)
            | [] -> acc
        loop empty list

    let inline ofSeq seq = Seq.fold (fun q i -> snoc i q) empty seq
