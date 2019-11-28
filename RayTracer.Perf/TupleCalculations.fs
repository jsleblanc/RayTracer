namespace RayTracer.Perf

open System
open RenderLib
open RenderLib.Tuple
open RenderLib.Material
open RenderLib.Translations
open RenderLib.Color
open RenderLib.Worlds
open RenderLib.Ray
open RenderLib.Patterns
open RenderLib.Lights
open RenderLib.Camera
open BenchmarkDotNet.Attributes

module TupleCalculations = 

    type TupleBenchmarks () =
        let v1 = vector 1.0 2.0 3.0
        let v2 = vector 2.0 3.0 4.0

        [<Benchmark>]
        member self.DotProduct () =
            v1.dotProduct(v2)

        [<Benchmark>]
        member self.CrossProduct () =
            v1.crossProduct(v2)

        [<Benchmark>]
        member self.Reflect () =
            reflect v1 v2

        [<Benchmark>]
        member self.Normalize() =
            v1.normalize()

        [<Benchmark>]
        member self.Magnitude() =
            v1.magnitude()