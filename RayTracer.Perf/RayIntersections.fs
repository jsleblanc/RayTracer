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

            
    type RayIntersectionPatternsBenchmarks () =
        let blue = color 0.0 0.0 1.0
        let white = color 1.0 1.0 1.0
        let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
        let vt = view_transform (point 0.0 2.5 0.0) (point 0.0 0.0 0.0) (vector 1.0 0.0 0.0)
        let camera = { create_default_camera 640 480 with field_of_view = Math.PI / 3.0; transform = vt; }
        let default_world = 
            let plane = 
                ShapePlane.build
                |> Shapes2.transform (translation 0.0 -10.1 0.0)
                |> Shapes2.texture { Material.material.Default with pattern = Some (checkers_pattern (translation 0.0 0.1 0.0) blue white); }
            let s2 = 
                ShapeSphere.build
                |> Shapes2.texture Material.material.Default
                |> Shapes2.transform (scaling 0.5 0.5 0.5)
            let w = Worlds.build [s2; plane;] light
            w
        let mutable world = Worlds.build_default []

        [<GlobalSetup>]
        member self.GlobalSetup() =
            world <- default_world

        [<Benchmark>]
        member self.IntersectWorldWithRay () =
            color_at world (ray_for_pixel camera 180 150) 5 |> ignore
            color_at world (ray_for_pixel camera 510 150) 5 |> ignore
            color_at world (ray_for_pixel camera 430 330) 5 |> ignore
            color_at world (ray_for_pixel camera 130 330) 5 |> ignore

            