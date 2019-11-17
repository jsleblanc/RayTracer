// Learn more about F# at http://fsharp.org

open BenchmarkDotNet.Running
open RayTracer.Perf.RayIntersections

let defaultSwitch () = BenchmarkSwitcher [|typeof<RayIntersectionBenchmarks>;|]

[<EntryPoint>]
let main argv =
    let summary = defaultSwitch().Run argv
    0 // return an integer exit code
