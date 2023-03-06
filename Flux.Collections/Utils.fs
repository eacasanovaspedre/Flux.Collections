namespace Flux

[<AutoOpen>]
module internal Helpers =
    
    let inline impossibleCodeBranch () = failwith "This is supposed to be an impossible branch in the code. There is something very wrong if this exception was thrown."

namespace Flux.Collections

open System.Collections.Generic

module Enumerable =

    let inline enumerator (enumerable: IEnumerable<_>) = enumerable.GetEnumerator()

module internal Array =

    let inline private blit source sourceStart targetStart count target =
        if count > 0 then
            Array.blit source sourceStart target targetStart count
            target
        else
            target

    let inline private mutate value index arr =
        Array.set arr index value
        arr

    let inline put value index arr =
        arr
        |> Array.copy
        |> mutate value index

    let insert value index arr =
        arr
        |> Array.length
        |> (+) 1
        |> Array.zeroCreate
        |> blit arr 0 0 index
        |> mutate value index
        |> blit arr index (index + 1) (Array.length arr - index)

    let remove index arr =
        Array.length arr - 1
        |> Array.zeroCreate
        |> blit arr 0 0 index
        |> blit arr (index + 1) index (Array.length arr - index - 1)

    module Unsafe =

        let inline ofListWithKnownSize count list = 
            let array = Array.zeroCreate count
            let rec loop index list =
                if index < array.Length then
                    array[index] <- List.head list
                    loop (index + 1) (List.tail list)
                else
                    array
            loop 0 list
            
        let inline ofListWithKnownSizeRev count list = 
            let array = Array.zeroCreate count
            let rec loop index list =
                if index >= 0 then
                    array[index] <- List.head list
                    loop (index - 1) (List.tail list)
                else
                    array
            loop (array.Length - 1) list

module KeyValuePair =

    let inline key (pair: KeyValuePair<_, _>) = pair.Key

    let inline value (pair: KeyValuePair<_, _>) = pair.Value

    let inline asTuple pair = key pair, value pair

    module Lens =

        let inline _Key map f e = map (f (key e)) (fun x -> KeyValuePair(x, value e))

        let inline _Value map f e = map (f (value e)) (fun x -> KeyValuePair(key x, x))
        
    [<AutoOpen>]
    module ActivePattern =
        
        let (|KeyValuePair|) (pair: KeyValuePair<_, _>) = asTuple pair