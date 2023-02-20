namespace Flux.Collections.Benchmark.Params.Maps

[<Struct>]
type KeyType =
    | Int
    | String
    | Guid

module Find =

    let keyTypes = [ Int; String; Guid ]

    let datasetSizes = [ 10; 100; 1000; 10000; 100000 ]

module Insert =

    let keyTypes = [ Int; String; Guid ]

    let datasetSizes = [ 10; 100; 1000; 10000; 100000 ]