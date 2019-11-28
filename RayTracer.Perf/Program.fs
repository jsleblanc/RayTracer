// Learn more about F# at http://fsharp.org

open BenchmarkDotNet.Running
open RayTracer.Perf.RayIntersections
open RayTracer.Perf.TupleCalculations

let defaultSwitch () = 
    BenchmarkSwitcher [|
        typeof<TupleBenchmarks>;
        typeof<RayIntersectionBenchmarks>;
        typeof<RayIntersectionPatternsBenchmarks>;
        typeof<RayIntersectionBoundingBoxBenchmarks>;
    |]

[<EntryPoint>]
let main argv =
    (*
    BenchmarkRunner.Run<TupleBenchmarks>() |> ignore    
    BenchmarkRunner.Run<RayIntersectionBenchmarks>() |> ignore
    BenchmarkRunner.Run<RayIntersectionPatternsBenchmarks>() |> ignore
    BenchmarkRunner.Run<RayIntersectionBoundingBoxBenchmarks>() |> ignore
    *)
    let summary = defaultSwitch().Run argv
    0 // return an integer exit code
