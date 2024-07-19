module Flux.Collections.Test

open Expecto.Tests

[<EntryPoint>]
let main _ =
    
    runTestsWithCLIArgs [] [||] Stream.tests
