//I prefer this over recursive namespaces. If one day this becomes a compile error I will be forced to change it. I hate recursive namespaces, it breaks F# linear dependencies.
#nowarn "60"#nowarn "69" // Interface implementations in augmentations are now deprecated. Interface implementations should be given on the initial declaration of a type.
namespace Flux.Collections

type 'T Queue =
    private
    | Queue of Front: 'T Stream * FrontLength: int * Back: 'T Stream * BackLength: int
    interface 'T System.Collections.Generic.IEnumerable

module Queue =

    module private Helpers =

        let queue f fl b bl =
            if bl <= fl
            then Queue(f, fl, b, bl)
            else Queue(Stream.append f (Stream.rev b), fl + bl, Stream.empty, 0)

    exception NoHeadInEmptyQueueException

    exception NoTailInEmptyQueueException

    exception NoHeadAndNoTailInEmptyQueueException

    type NoHeadInEmptyQueue = NoHeadInEmptyQueue

    type NoTailInEmptyQueue = NoTailInEmptyQueue

    type NoHeadAndNoTailInEmptyQueue = NoHeadAndNoTailInEmptyQueue

    let empty<'T> = Queue(Stream.empty<'T>, 0, Stream.empty<'T>, 0)

    let isEmpty (Queue(_, frontLength, _, _)) = frontLength = 0

    let isNotEmpty (Queue(_, frontLength, _, _)) = frontLength > 0

    let snoc x (Queue(front, frontLength, back, backLength)) =
        Helpers.queue front frontLength (Stream.cons x back) (backLength + 1)

    let uncons (Queue(front, frontLength, back, backLength)) =
        if Stream.isNotEmpty front then
            let h, frontTail = Stream.uncons front
            h, Helpers.queue frontTail (frontLength - 1) back backLength
        else
            raise NoHeadAndNoTailInEmptyQueueException

    let tryUncons (Queue(front, frontLength, back, backLength)) =
        if Stream.isNotEmpty front then
            let h, frontTail = Stream.uncons front
            Ok(h, Helpers.queue frontTail (frontLength - 1) back backLength)
        else
            Error NoHeadAndNoTailInEmptyQueue

    let maybeUncons (Queue(front, frontLength, back, backLength)) =
        if Stream.isNotEmpty front then
            let h, frontTail = Stream.uncons front
            Some(h, Helpers.queue frontTail (frontLength - 1) back backLength)
        else
            None

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

    let ofList list =
        let rec loop acc =
            function
            | x :: xs -> xs |> loop (snoc x acc)
            | [] -> acc
        loop empty list

    let ofSeq seq = Seq.fold (fun q i -> snoc i q) empty seq

    let inline toList queue =
        let rec loop acc q =
            if isEmpty q
            then acc
            else let head, tail = uncons q in loop (head :: acc) tail
        loop [] queue

    let inline toSeq queue = Seq.unfold maybeUncons queue

    let inline (|Cons|Nil|) q =
        if isNotEmpty q then Choice1Of2(head q, tail q) else Choice2Of2()

type 'T Queue with
    interface System.Collections.Generic.IEnumerable<'T> with

        member this.GetEnumerator(): _ System.Collections.Generic.IEnumerator =
            this
            |> Queue.toSeq
            |> Enumerable.enumerator

        member this.GetEnumerator(): System.Collections.IEnumerator =
            upcast (this
                    |> Queue.toSeq
                    |> Enumerable.enumerator)
