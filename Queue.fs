namespace Flux.Collections

type 'T Queue = Queue of Front: 'T Stream * FrontLength: int * Back: 'T Stream * BackLength: int

module Queue =

    module private Helpers =

        let queue f fl b bl =
            if bl <= fl
            then Queue(f, fl, b, bl)
            else Queue(Stream.append f (Stream.rev b), fl + bl, Stream.empty, 0)

    exception NoHeadInEmptyQueueException

    exception NoTailInEmptyQueueException

    type NoHeadInEmptyQueue = NoHeadInEmptyQueue

    type NoTailInEmptyQueue = NoTailInEmptyQueue

    let empty<'T> = Queue(Stream.empty<'T>, 0, Stream.empty<'T>, 0)

    let isEmpty (Queue(_, frontLength, _, _)) = frontLength = 0

    let snoc x (Queue(front, frontLength, back, backLength)) =
        Helpers.queue front frontLength (Stream.cons x back) (backLength + 1)

    let head (Queue(front, _, _, _)) =
        if Stream.isNotEmpty front then Stream.head front else raise NoHeadInEmptyQueueException

    let tryHead (Queue(front, _, _, _)) =
        if Stream.isNotEmpty front then
            front
            |> Stream.head
            |> Ok
        else
            Error NoHeadInEmptyQueue

    let maybeHead (Queue(front, _, _, _)) =
        if Stream.isNotEmpty front then
            front
            |> Stream.head
            |> Some
        else
            None

    let tail (Queue(front, frontLength, back, backLength)) =
        if Stream.isNotEmpty front
        then Helpers.queue (Stream.tail front) (frontLength - 1) back backLength
        else raise NoTailInEmptyQueueException

    let tryTail (Queue(front, frontLength, back, backLength)) =
        if Stream.isNotEmpty front
        then Helpers.queue (Stream.tail front) (frontLength - 1) back backLength |> Ok
        else Error NoTailInEmptyQueue

    let maybeTail (Queue(front, frontLength, back, backLength)) =
        if Stream.isNotEmpty front
        then Helpers.queue (Stream.tail front) (frontLength - 1) back backLength |> Some
        else None

    let inline ofList list =
        let rec loop acc =
            function
            | x :: xs -> xs |> loop (snoc x acc)
            | [] -> acc
        loop empty list

    let inline ofSeq seq = Seq.fold (fun q i -> snoc i q) empty seq

    let inline toList queue =
        let rec loop acc q =
            if isEmpty q then acc else loop ((head q) :: acc) (tail q)
        loop [] queue

    let inline toSeq queue =
        queue
        |> Seq.unfold (fun q ->
            if isEmpty q then Some(head q, tail q) else None)
