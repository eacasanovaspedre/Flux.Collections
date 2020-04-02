namespace Flux.Collections

open Flux.Collections.Stream

type 'T Queue = Queue of Front: 'T Stream * FrontLength: int * Back: 'T Stream * BackLength: int

module private QueueHelpers =

    let queue f fl b bl =
        if bl <= fl
            then Queue (f, fl, b, bl)
            else Queue (Stream.append f (Stream.rev b), fl + bl, Stream.empty, 0)

module Queue =

    let empty () = Queue (Stream.empty, 0, Stream.empty, 0)

    let isEmpty (Queue (_, frontLength, _, _)) = frontLength = 0

    let snoc x (Queue (front, frontLength, back, backLength)) =
        QueueHelpers.queue front frontLength (Stream.cons x back) (backLength + 1)

    let head (Queue (front, _, _, _)) =
        match front with
        | Cons (x, front') -> x
        | _ -> failwith "Empty queue"

    let tail (Queue (front, frontLength, back, backLength)) =
        match front with
        | Cons (x, front') -> QueueHelpers.queue front' (frontLength - 1) back backLength
        | _ -> failwith "Empty queue"