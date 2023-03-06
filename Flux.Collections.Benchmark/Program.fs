namespace Flux.Collections.Benchmark

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Diagnosers
open Flux.Collections

[<MemoryDiagnoser(displayGenColumns = true)>]
[<HardwareCounters(HardwareCounter.CacheMisses, HardwareCounter.BranchInstructions, HardwareCounter.BranchMispredictions)>]
type ListMap () =
    
    let map mapper list =
        let rec loopOverEntries acc =
            function
            | KVEntry (key, value) :: xs ->
                let newValue = mapper key value
                let newEntry = KVEntry (key, newValue)
                loopOverEntries (newEntry :: acc) xs
            | [] -> List.rev acc
        loopOverEntries [] list
    
    member val Dataset = [] with get, set
    
    member val Map = fun k v -> k + v with get, set
    
    [<GlobalSetup>]
    member x.Setup() =
        x.Map <- if DateTime.Now.Millisecond % 2 = 0 then (fun k v -> k + v) else (fun k v -> v - k)
        x.Dataset <- List.init 10000 (fun x -> KVEntry(x, x * 2))
        
    [<Benchmark(Baseline = true)>]
    member x.Std() =
        x.Dataset
        |> List.map (fun (KVEntry (k, v)) -> x.Map k v)
        |> List.head
        
    [<Benchmark>]
    member x.Mine() =
        x.Dataset
        |> map x.Map
        |> List.head


module Main =
    open BenchmarkDotNet.Running
    open System.Reflection
    
    [<EntryPoint>]
    let main args =
        // let entries = Seq.init 10000 id |> Seq.map (fun x -> x, x.ToString().Substring(0, 1)) |> Seq.toArray
        // let hamt = entries |> Seq.fold (fun hamt (k, v) -> Hamt.add k v hamt) Hamt.empty |> Hamt.filter (fun k v -> k % 3 = 0)
        // let hamt = hamt |> Hamt.map (fun x v -> v + $"%d{x}")
        // printfn $"%A{hamt}"
        BenchmarkSwitcher.FromAssembly(Assembly.GetEntryAssembly()).Run(args) |> ignore
        0
