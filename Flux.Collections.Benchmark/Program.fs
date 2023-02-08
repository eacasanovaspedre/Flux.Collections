namespace Flux.Collections.Benchmark

open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open System
open Flux.Collections
open BenchmarkDotNet.Running
open System.Collections.Generic

type KeyType =
    | Int
    | String
    | Guid

type Dataset<'Key when 'Key: equality and 'Key: comparison> =
    { Hamt: Lazy<Hamt<'Key, string>>
      FSharpMap: Lazy<Map<'Key, string>>
      DotnetMap: Lazy<Dictionary<'Key, string>>
      LookupKeys: 'Key array }

type Dataset' =
    | IntKey of Dataset<int>
    | StringKey of Dataset<string>
    | GuidKey of Dataset<Guid>

module Dataset =

    let lookupCount size = size * 10

    let private createEntries newKey entryCount =
        Seq.init entryCount (fun _ -> newKey (), (Guid.NewGuid().ToString()))
        |> Seq.toArray

    let private selectLookupKeys entries lookupCount =
        let rnd = new Random()
        let entriesSize = Array.length entries

        [| for _ in 1..lookupCount do
               yield fst entries[rnd.Next(entriesSize)] |]

    let hamtOfSeq seq =
        Seq.fold (fun hamt (k, v) -> Hamt.add k v hamt) Hamt.empty seq

    let dotnetMapOfSeq seq =
        seq
        |> Seq.distinctBy fst
        |> Seq.map (fun (k, v) -> KeyValuePair(k, v))
        |> Dictionary

    let create entryCount newKey =
        let entries = createEntries newKey entryCount
        let lookupKeys = selectLookupKeys entries (lookupCount entryCount)

        { Hamt = lazy hamtOfSeq entries
          FSharpMap = lazy Map.ofSeq entries
          DotnetMap = lazy dotnetMapOfSeq entries
          LookupKeys = lookupKeys }

    let inline findAllAndSum getItem map keys =
        keys
        |> Seq.map (fun k -> getItem k map |> String.length)
        |> Seq.sum

[<AsciiDocExporter>]
[<RPlotExporter>]
[<MemoryDiagnoser>]
[<HardwareCounters(HardwareCounter.CacheMisses, HardwareCounter.BranchInstructions, HardwareCounter.BranchMispredictions)>]
type MapsFind() =

    [<ParamsSource("KeyTypes")>]
    member val KeyType = Int with get, set

    member val KeyTypes = [ Int; String; Guid ] |> List.toSeq

    [<ParamsSource("DatasetSizes")>]
    member val DatasetSize = 10 with get, set

    member val DatasetSizes =
        [ 10
          100
          1000
          10000
          100000 ]
        |> List.toSeq

    member val HamtDataset = None with get, set

    member val Dataset = None with get, set

    [<GlobalSetup>]
    member x.Setup() =
        match x.KeyType with
        | Int ->
            let maxKey = (float x.DatasetSize) * 0.8 |> int
            let random = Random()

            x.Dataset <-
                Dataset.create x.DatasetSize (fun () -> random.Next(0, maxKey))
                |> IntKey
                |> Some
        | String ->
            x.Dataset <-
                Dataset.create x.DatasetSize (fun () -> Guid.NewGuid().ToString())
                |> StringKey
                |> Some
        | Guid ->
            x.Dataset <-
                Dataset.create x.DatasetSize (fun () -> Guid.NewGuid())
                |> GuidKey
                |> Some

    [<Benchmark>]
    member x.Hamt() =
        match x.Dataset with
        | Some (IntKey { Hamt = Lazy hamt
                         LookupKeys = lookupKeys }) -> Dataset.findAllAndSum (Hamt.find) hamt lookupKeys
        | Some (StringKey { Hamt = Lazy hamt
                            LookupKeys = lookupKeys }) -> Dataset.findAllAndSum (Hamt.find) hamt lookupKeys
        | Some (GuidKey { Hamt = Lazy hamt
                          LookupKeys = lookupKeys }) -> Dataset.findAllAndSum (Hamt.find) hamt lookupKeys
        | None -> failwith "Not initialized"

    [<Benchmark>]
    member x.FSharpMap() =
        match x.Dataset with
        | Some (IntKey { FSharpMap = Lazy map
                         LookupKeys = lookupKeys }) -> Dataset.findAllAndSum (Map.find) map lookupKeys
        | Some (StringKey { FSharpMap = Lazy map
                            LookupKeys = lookupKeys }) -> Dataset.findAllAndSum (Map.find) map lookupKeys
        | Some (GuidKey { FSharpMap = Lazy map
                          LookupKeys = lookupKeys }) -> Dataset.findAllAndSum (Map.find) map lookupKeys
        | None -> failwith "Not initialized"

    [<Benchmark(Baseline = true)>]
    member x.DotnetMap() =
        match x.Dataset with
        | Some (IntKey { DotnetMap = Lazy map
                         LookupKeys = lookupKeys }) ->
            Dataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) map lookupKeys
        | Some (StringKey { DotnetMap = Lazy map
                            LookupKeys = lookupKeys }) ->
            Dataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) map lookupKeys
        | Some (GuidKey { DotnetMap = Lazy map
                          LookupKeys = lookupKeys }) ->
            Dataset.findAllAndSum (fun k (t: Dictionary<_, _>) -> t[k]) map lookupKeys
        | None -> failwith "Not initialized"

module Main =
    [<EntryPoint>]
    let main args =
        BenchmarkRunner.Run<MapsFind>() |> ignore
        0
