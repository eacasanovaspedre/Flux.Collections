namespace Flux.Collections.Benchmark.Params.Maps

[<Struct>]
type KeyType =
    | Int
    | String
    | Guid
    | CustomClassStrType
    | CustomStructStrType

module Find =

    let keyTypes = [
        Int
        String
        Guid
        CustomClassStrType
        CustomStructStrType
    ]

    let datasetSizes = [
        10
        100
        1000
        10000
    ]

module Insert =
    
    let keyTypes = [
        Int
        String
        Guid
        CustomClassStrType
        CustomStructStrType
    ]

    let datasetSizes = [
        //10
        100
        1000
        10000
    ]
