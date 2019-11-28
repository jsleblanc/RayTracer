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
                |> Shapes.texture { Material.material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }
            let s2 = 
                ShapeSphere.build
                |> Shapes.texture Material.material.Default
                |> Shapes.transform (scaling 0.5 0.5 0.5)
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
        let pt = Patterns.checkers (solid_c blue) (solid_c white) |> Patterns.transform (translation 0.0 0.1 0.0)
        let default_world = 
            let plane = 
                ShapePlane.build
                |> Shapes.transform (translation 0.0 -10.1 0.0)
                |> Shapes.texture { Material.material.Default with pattern = Some pt; }
            let s2 = 
                ShapeSphere.build
                |> Shapes.texture Material.material.Default
                |> Shapes.transform (scaling 0.5 0.5 0.5)
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

    type RayIntersectionBoundingBoxBenchmarks () =
        let spheres = 
            let c = 3
            seq {
                for x in -c .. c do
                    for y in -c .. c do
                        for z in -c .. c do
                            yield ShapeSphere.build |> Shapes.transform ((translation (float x) (float y) (float z)) * (scaling 0.5 0.5 0.5))
            } 
            |> Seq.toList
        let light = { position = point 0.0 0.0 -10.0; intensity = color 0.7 0.7 0.7; }
        let vt = view_transform (point 0.0 -2.0 -10.0) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)
        let camera = { create_default_camera 640 480 with field_of_view = Math.PI; transform = vt; }
        let default_world_group =             
            let g = ShapeGroup.build spheres
            Worlds.build [g;] light
        let default_world_without_group =
            Worlds.build spheres light

        let mutable world_with_group = Worlds.build_default []
        let mutable world_without_group = Worlds.build_default []

        [<GlobalSetup>]
        member self.GlobalSetup() =
            world_with_group <- default_world_group
            world_without_group <- default_world_without_group

        [<Benchmark(Baseline = true)>]
        member self.IntersectWorldWithoutGroup () =
            render camera world_without_group

        [<Benchmark>]
        member self.IntersectWorldWithGroup () =
            render camera world_with_group