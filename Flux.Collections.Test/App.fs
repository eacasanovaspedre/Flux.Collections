open System
open Expecto.Tests

[<EntryPoint>]
let main _ =
    
    runTests defaultConfig Flux.Collections.Stream.tests
