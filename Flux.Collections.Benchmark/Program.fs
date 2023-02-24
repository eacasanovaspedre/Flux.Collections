namespace Flux.Collections.Benchmark

module Main =
    open BenchmarkDotNet.Running
    open System.Reflection
    
    [<EntryPoint>]
    let main args =
        BenchmarkSwitcher.FromAssembly(Assembly.GetEntryAssembly()).Run(args) |> ignore
        0
