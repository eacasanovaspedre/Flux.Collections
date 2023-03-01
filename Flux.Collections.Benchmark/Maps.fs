namespace Flux.Collections.Benchmark.Maps

open Flux.Collections
open System.Collections.Generic
open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Diagnosers
open Flux.Collections.Benchmark.Keys
open Flux.Collections.Benchmark.Params.Maps

[<Struct>]
type FindDataset<'Key when 'Key: equality and 'Key: comparison> =
    { HamtStructural: Hamt<'Key, string> Lazy
      Hamt: Hamt<'Key, string> Lazy
      FSharpMap: Map<'Key, string> Lazy
      DotnetMap: Dictionary<'Key, string> Lazy
      LookupKeys: 'Key array }

[<Struct>]
type FindDataset' =
    | IntKey of intDataset: FindDataset<int>
    | StringKey of stringDataset: FindDataset<string>
    | GuidKey of guidDataset: FindDataset<Guid>
    | CustomClassStrKey of customClassDataset: FindDataset<CustomClassStr>
    | CustomStructStrKey of customStructDataset: FindDataset<CustomStructStr>

[<Struct>]
type PutDataset =
    | IntKey of intDataset: (int * string) array Lazy
    | StringKey of stringDataset: (string * string) array Lazy
    | GuidKey of guidDataset: (Guid * string) array Lazy
    | CustomClassStrKey of customClassDataset: (CustomClassStr * string) array Lazy
    | CustomStructStrKey of customStructDataset: (CustomStructStr * string) array Lazy

module Entries =

    let createEntries newKey entryCount =
        Seq.init entryCount (fun _ -> newKey (), Guid.NewGuid().ToString())
        |> Seq.toArray

module FindDataset =

    let lookupCount size = size * 10

    let private selectLookupKeys entries lookupCount =
        let rnd = Random()
        let entriesSize = Array.length entries

        [| for _ in 1..lookupCount do
               yield fst entries[rnd.Next(entriesSize)] |]

    let hamtOfSeqStructural seq =
        Seq.fold (fun hamt (k, v) -> Hamt.add k v hamt) Hamt.emptyStructural seq

    let hamtOfSeq seq =
        Seq.fold (fun hamt (k, v) -> Hamt.add k v hamt) Hamt.empty seq

    let dotnetMapOfSeq seq =
        seq
        |> Seq.distinctBy fst
        |> Seq.map (fun (k, v) -> KeyValuePair(k, v))
        |> Dictionary

    let create entryCount newKey =
        let entries = Entries.createEntries newKey entryCount
        let lookupKeys = selectLookupKeys entries (lookupCount entryCount)

        { HamtStructural = lazy hamtOfSeqStructural entries
          Hamt = lazy hamtOfSeq entries
          FSharpMap = lazy Map.ofSeq entries
          DotnetMap = lazy dotnetMapOfSeq entries
          LookupKeys = lookupKeys }

    let inline findAllAndSum ([<InlineIfLambda>] getItem) map keys =
        let rec sumLengths index sum =
            if index < Array.length keys then
                sumLengths (index + 1) (sum + (getItem keys[index] map |> String.length))
            else
                sum

        sumLengths 0 0

[<AsciiDocExporter>]
[<MemoryDiagnoser(displayGenColumns = true)>]
[<HardwareCounters(HardwareCounter.CacheMisses, HardwareCounter.BranchInstructions, HardwareCounter.BranchMispredictions)>]
type Find() =

    [<ParamsSource("KeyTypes")>]
    member val KeyType = Int with get, set

    member val KeyTypes = Find.keyTypes

    [<ParamsSource("DatasetSizes")>]
    member val DatasetSize = 10 with get, set

    member val DatasetSizes = Find.datasetSizes

    member val Dataset = None with get, set

    [<GlobalSetup>]
    member x.Setup() =
        match x.KeyType with
        | Int ->
            let maxKey = (float x.DatasetSize) * 0.8 |> int
            let random = Random()

            x.Dataset <-
                FindDataset.create x.DatasetSize (fun () -> random.Next(0, maxKey))
                |> FindDataset'.IntKey
                |> Some
        | String ->
            x.Dataset <-
                FindDataset.create x.DatasetSize (fun () -> Guid.NewGuid().ToString())
                |> FindDataset'.StringKey
                |> Some
        | Guid ->
            x.Dataset <-
                FindDataset.create x.DatasetSize (fun () -> Guid.NewGuid())
                |> FindDataset'.GuidKey
                |> Some
        | CustomClassStrType ->
            x.Dataset <-
                FindDataset.create x.DatasetSize (fun () -> CustomClassStr(Guid.NewGuid().ToString()))
                |> FindDataset'.CustomClassStrKey
                |> Some
        | CustomStructStrType ->
            x.Dataset <-
                FindDataset.create x.DatasetSize (fun () -> CustomStructStr(Guid.NewGuid().ToString()))
                |> FindDataset'.CustomStructStrKey
                |> Some

    [<Benchmark>]
    member x.HamtStructural() =
        match x.Dataset with
        | Some(FindDataset'.IntKey { HamtStructural = hamt
                                     LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.StringKey { HamtStructural = hamt
                                        LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.GuidKey { HamtStructural = hamt
                                      LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.CustomClassStrKey { HamtStructural = hamt
                                                LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.CustomStructStrKey { HamtStructural = hamt
                                                 LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | None -> failwith "Not initialized"

    [<Benchmark>]
    member x.Hamt() =
        match x.Dataset with
        | Some(FindDataset'.IntKey { Hamt = hamt; LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.StringKey { Hamt = hamt; LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.GuidKey { Hamt = hamt; LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.CustomClassStrKey { Hamt = hamt; LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | Some(FindDataset'.CustomStructStrKey { Hamt = hamt; LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Hamt.find (hamt.Force()) lookupKeys
        | None -> failwith "Not initialized"

    [<Benchmark>]
    member x.FSharpMap() =
        match x.Dataset with
        | Some(FindDataset'.IntKey { FSharpMap = map
                                     LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Map.find (map.Force()) lookupKeys
        | Some(FindDataset'.StringKey { FSharpMap = map
                                        LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Map.find (map.Force()) lookupKeys
        | Some(FindDataset'.GuidKey { FSharpMap = map
                                      LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Map.find (map.Force()) lookupKeys
        | Some(FindDataset'.CustomClassStrKey { FSharpMap = map
                                                LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Map.find (map.Force()) lookupKeys
        | Some(FindDataset'.CustomStructStrKey { FSharpMap = map
                                                 LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum Map.find (map.Force()) lookupKeys
        | None -> failwith "Not initialized"

    [<Benchmark(Baseline = true)>]
    member x.DotnetMap() =
        match x.Dataset with
        | Some(FindDataset'.IntKey { DotnetMap = map
                                     LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) (map.Force()) lookupKeys
        | Some(FindDataset'.StringKey { DotnetMap = map
                                        LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) (map.Force()) lookupKeys
        | Some(FindDataset'.GuidKey { DotnetMap = map
                                      LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) (map.Force()) lookupKeys
        | Some(FindDataset'.CustomClassStrKey { DotnetMap = map
                                                LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) (map.Force()) lookupKeys
        | Some(FindDataset'.CustomStructStrKey { DotnetMap = map
                                                 LookupKeys = lookupKeys }) ->
            FindDataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) (map.Force()) lookupKeys
        | None -> failwith "Not initialized"

module PutDataset =

    let insertOnHamt entries =
        let rec loop index map =
            if index = Array.length entries then
                map
            else
                let key, value = entries[index]
                Hamt.add key value map |> loop (index + 1)

        loop 0 Hamt.empty
        
    let insertOnHamtStructural entries =
        let rec loop index map =
            if index = Array.length entries then
                map
            else
                let key, value = entries[index]
                Hamt.add key value map |> loop (index + 1)

        loop 0 Hamt.emptyStructural

    let insertOnFSharpMap entries =
        let rec loop index map =
            if index = Array.length entries then
                map
            else
                let key, value = entries[index]
                Map.add key value map |> loop (index + 1)

        loop 0 Map.empty

    let insertOnDotnetMap entries =
        let rec loop index (map: Dictionary<_, _>) =
            if index = Array.length entries then
                map
            else
                let key, value = entries[index]
                map.TryAdd(key, value) |> ignore
                loop (index + 1) map

        loop 0 (Dictionary())

[<AsciiDocExporter>]
[<MemoryDiagnoser(displayGenColumns = true)>]
[<HardwareCounters(HardwareCounter.CacheMisses, HardwareCounter.BranchInstructions, HardwareCounter.BranchMispredictions)>]
type Insert() =

    [<ParamsSource("KeyTypes")>]
    member val KeyType = Int with get, set

    member val KeyTypes = Insert.keyTypes

    [<ParamsSource("DatasetSizes")>]
    member val DatasetSize = 10 with get, set

    member val DatasetSizes = Insert.datasetSizes

    member val Dataset = None with get, set

    [<GlobalSetup>]
    member x.Setup() =
        match x.KeyType with
        | Int ->
            let maxKey = (float x.DatasetSize) * 0.8 |> int
            let random = Random()

            x.Dataset <-
                lazy Entries.createEntries (fun () -> random.Next(0, maxKey)) x.DatasetSize
                |> PutDataset.IntKey
                |> Some
        | String ->
            x.Dataset <-
                lazy Entries.createEntries (fun () -> Guid.NewGuid().ToString()) x.DatasetSize
                |> PutDataset.StringKey
                |> Some
        | Guid ->
            x.Dataset <-
                lazy Entries.createEntries (fun () -> Guid.NewGuid()) x.DatasetSize
                |> PutDataset.GuidKey
                |> Some
        | CustomClassStrType ->
            x.Dataset <-
                lazy Entries.createEntries (fun () -> CustomClassStr(Guid.NewGuid().ToString())) x.DatasetSize
                |> PutDataset.CustomClassStrKey
                |> Some
        | CustomStructStrType ->
            x.Dataset <-
                lazy Entries.createEntries (fun () -> CustomStructStr(Guid.NewGuid().ToString())) x.DatasetSize
                |> PutDataset.CustomStructStrKey
                |> Some

    [<Benchmark>]
    member x.HamtStructural() =
        match x.Dataset with
        | Some (PutDataset.IntKey (Lazy entries)) -> PutDataset.insertOnHamtStructural entries |> ignore
        | Some (PutDataset.StringKey (Lazy entries)) -> PutDataset.insertOnHamtStructural entries |> ignore
        | Some (PutDataset.GuidKey (Lazy entries)) -> PutDataset.insertOnHamtStructural entries |> ignore
        | Some (PutDataset.CustomClassStrKey (Lazy entries)) -> PutDataset.insertOnHamtStructural entries |> ignore
        | Some (PutDataset.CustomStructStrKey (Lazy entries)) -> PutDataset.insertOnHamtStructural entries |> ignore
        | None -> failwith "Not initialized"
        
    [<Benchmark>]
    member x.Hamt() =
        match x.Dataset with
        | Some (PutDataset.IntKey (Lazy entries)) -> PutDataset.insertOnHamt entries |> ignore
        | Some (PutDataset.StringKey (Lazy entries)) -> PutDataset.insertOnHamt entries |> ignore
        | Some (PutDataset.GuidKey (Lazy entries)) -> PutDataset.insertOnHamt entries |> ignore
        | Some (PutDataset.CustomClassStrKey (Lazy entries)) -> PutDataset.insertOnHamt entries |> ignore
        | Some (PutDataset.CustomStructStrKey (Lazy entries)) -> PutDataset.insertOnHamt entries |> ignore
        | None -> failwith "Not initialized"

    [<Benchmark>]
    member x.FSharpMap() =
        match x.Dataset with
        | Some (PutDataset.IntKey (Lazy entries)) -> PutDataset.insertOnFSharpMap entries |> ignore
        | Some (PutDataset.StringKey (Lazy entries)) -> PutDataset.insertOnFSharpMap entries |> ignore
        | Some (PutDataset.GuidKey (Lazy entries)) -> PutDataset.insertOnFSharpMap entries |> ignore
        | Some (PutDataset.CustomClassStrKey (Lazy entries)) -> PutDataset.insertOnFSharpMap entries |> ignore
        | Some (PutDataset.CustomStructStrKey (Lazy entries)) -> PutDataset.insertOnFSharpMap entries |> ignore
        | None -> failwith "Not initialized"

    [<Benchmark(Baseline = true)>]
    member x.DotnetMap() =
        match x.Dataset with
        | Some (PutDataset.IntKey (Lazy entries)) -> PutDataset.insertOnDotnetMap entries |> ignore
        | Some (PutDataset.StringKey (Lazy entries)) -> PutDataset.insertOnDotnetMap entries |> ignore
        | Some (PutDataset.GuidKey (Lazy entries)) -> PutDataset.insertOnDotnetMap entries |> ignore
        | Some (PutDataset.CustomClassStrKey (Lazy entries)) -> PutDataset.insertOnDotnetMap entries |> ignore
        | Some (PutDataset.CustomStructStrKey (Lazy entries)) -> PutDataset.insertOnDotnetMap entries |> ignore
        | None -> failwith "Not initialized"
