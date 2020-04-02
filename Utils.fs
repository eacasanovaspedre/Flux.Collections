namespace Flux.Collections

open System.Collections.Generic

module Enumerable =

    let inline enumerator (enumerable: IEnumerable<_>) = enumerable.GetEnumerator()
