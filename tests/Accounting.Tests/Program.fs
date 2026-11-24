/// The .NET run of the suites: TAP on standard output, exit code = number of failures.
module Accounting.Tests.Program

[<EntryPoint>]
let main _ = if Wasmecto.Runner.runAll (fun line -> printfn "%s" line) Accounting.Tests.Tests.suites = 0 then 0 else 1
