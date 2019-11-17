namespace RayTracer.Perf

open RenderLib
open RenderLib.Tuple
open RenderLib.Material
open RenderLib.Translations
open RenderLib.Color
open RenderLib.Worlds
open RenderLib.Ray
open BenchmarkDotNet.Attributes

module RayIntersections =

    type RayIntersectionBenchmarks () =
        let default_world = 
            let s1 = 
                ShapeSphere.build
                |> Shapes2.texture { Material.material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }
            let s2 = 
                ShapeSphere.build
                |> Shapes2.texture Material.material.Default
                |> Shapes2.transform (scaling 0.5 0.5 0.5)
            let w = Worlds.build_default [s1; s2;]
            (s1,s2,w)
        let mutable world = Worlds.build_default []

        [<GlobalSetup>]
        member self.GlobalSetup() =
            let (_,_,w) = default_world
            world <- w

        [<Benchmark>]
        member self.IntersectWorldWithRay () =
            let r = {
                origin = point 0.0 0.0 -5.0;
                direction = vector 0.0 0.0 1.0;
            }
            intersect_world world r