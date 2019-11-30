// Learn more about F# at http://fsharp.org

open System
open RenderLib
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Material
open RenderLib.Patterns
open RenderLib.Lights
open RenderLib.Worlds
open RenderLib.Camera
open RenderLib.Ray
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg
open System.Diagnostics
open RenderLib.Matrix

[<EntryPoint>]
let main argv =

    let red = color 1.0 0.0 0.0
    let blue = color 0.0 0.0 1.0
    let green = color 0.0 1.0 0.0
    let yellow = color 1.0 1.0 0.0

    printfn "Calculating..."
    let sw = Stopwatch.StartNew()   

    let canvas_to_jpg (fileName:string) (c:Canvas.canvas) =
        let encoder = new JpegEncoder()
        encoder.Quality <- new Nullable<int>(100)
        let image = new Image<Argb32>(c.GetLength(0), c.GetLength(1))
        for x in 0 .. c.GetLength(0) - 1 do
            for y in 0 .. c.GetLength(1) - 1 do
                let c = c.[x,y]
                let r = single c.red
                let g = single c.green
                let b = single c.blue
                let pixel = new Argb32(r,g,b,0.0f)
                image.[x,y] <- pixel
        image.Save(fileName, encoder)



    //let p = Solid(red)// blue
    
    //let p = Blended(stripe_pattern_default white blue, stripe_pattern (rotation_y(-Math.PI/2.0)) white blue)
    //let planeMaterial = { material.Default with color = color 1.0 0.9 0.9; specular = 0.0; pattern = Some p; }

    let pt = Patterns.checkers (solid_c blue) (solid_c white) |> Patterns.transform (scaling 0.5 0.5 0.5)
    let plane = 
        ShapePlane.build
        |> Shapes.texture { Material.material.Default with color = color 1.0 0.9 0.9; specular = 0.0; pattern = Some pt; }
        //|> Shapes2.transform ((translation 0.0 0.0 10.0) * (rotation_x (Math.PI/2.0)))

    let middle = 
        ShapeSphere.build
        |> Shapes.texture { glass with diffuse = 0.01; ambient = 0.02; reflective = 0.9; specular = 1.0; shininess = 300.0; }
        |> Shapes.transform (translation -0.5 1.0 0.5)

    let right =
        ShapeSphere.build
        |> Shapes.texture { Material.material.Default with color = red; diffuse = 0.7; specular = 0.3; }
        |> Shapes.transform ((translation -0.75 1.5 5.0) * (scaling 0.75 0.75 0.75))

    let left = 
        ShapeSphere.build
        |> Shapes.texture { Material.material.Default with color = yellow; diffuse = 0.7; specular = 0.3; }
        |> Shapes.transform ((translation -1.5 0.33 -0.75) * (scaling 0.33 0.33 0.33))

    let cube =
        ShapeCube.build
        |> Shapes.texture { Material.material.Default with color = green; }
        |> Shapes.transform ((translation -3.5 1.0 0.5) * (scaling 0.75 0.75 0.75) * (rotation_y (Math.PI/3.5)))

    //let cylinder = Cylinder({ material.Default with color = green; },(translation -3.5 1.0 0.5) * (scaling 0.75 0.75 0.75) * (rotation_y (Math.PI/3.5)),None,1.0,3.0,true)
    //((rotation_z (Math.PI / -3.0)) * (rotation_x (Math.PI/2.0)))
    (*
    let vt = view_transform (point 0.0 1.5 -3.0) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)    
    let camera = { create_default_camera 640 480 with transform = vt; }
    let light = { position = point 0.0 10.0 -10.0; intensity = color 1.0 1.0 1.0; }


    let world = Worlds.build [left;right;middle;cube;plane;] light
    let canvas = render camera world
    canvas_to_jpg "output.jpg" canvas    
    printfn "Calculations completed in %s" (sw.Elapsed.ToString())
    *)
    (*
    let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
    //let vt = view_transform (point 0.0 2.5 0.0) (point 0.0 0.0 0.0) (vector 1.0 0.0 0.0)
    let vt = view_transform (point 2.0 2.0 -3.0) (point 0.0 0.0 0.0) (vector 0.0 1.0 0.0)
    let camera = { create_default_camera 1024 768 with field_of_view = Math.PI / 2.0; transform = vt; }
    let pt = Patterns.solid_color blue //|> Patterns.transform (translation 0.0 0.1 0.0)
    *)
    (*
    let teapot = 
        ObjectFiles.parse_file @"C:\Users\josep\Source\Repos\RayTracer\RayTracerCLI\Scenes\teapot.obj"
        |> Shapes.transform (scaling 0.75 0.75 0.75)
        |> Shapes.pattern pt
    let dragon =
        ObjectFiles.parse_file @"C:\Users\josep\Source\Repos\RayTracer\RayTracerCLI\Scenes\dragon.obj"
        |> Shapes.transform (scaling 0.75 0.75 0.75)
    printfn "Loading OBJ files took %s" (sw.Elapsed.ToString())
    *)
    (*
    let shapes = [    
        FunShapes.csb_cube { glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; }
        ShapePlane.build |> Shapes.transform (translation 0.0 -10.1 0.0) |> Shapes.texture { Material.material.Default with pattern = Some pt; }
    ]

    //let u = ShapeCSG.union c1 c2

    let default_world = 
        let plane = 
            ShapePlane.build
            |> Shapes.transform (translation 0.0 -10.1 0.0)
            |> Shapes.texture { Material.material.Default with pattern = Some pt; }
        let s2 = 
            ShapeSphere.build
            |> Shapes.texture Material.material.Default
            |> Shapes.transform (scaling 0.5 0.5 0.5)
        let w = Worlds.build_default [plane;s2;]
        w
    let world = Worlds.build_default shapes
    let canvas = render camera world
    //let color = color_at world { origin = point 0.0 0.0 0.0; direction = vector  } 5
    canvas_to_jpg "output.jpg" canvas
    *)
    (*
    //sphere inside a sphere 
    let pt = Patterns.checkers (solid_c blue) (solid_c white) |> Patterns.transform (translation 0.0 0.1 0.0)
    let plane = ShapePlane.build |> Shapes.texture { Material.material.Default with pattern = Some pt; } |> Shapes.transform (translation 0.0 -10.1 0.0)
    let s1 = ShapeSphere.build |> Shapes.texture { glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; } |> Shapes.transform (scaling 1.25 1.25 1.25)
    let s2 = ShapeSphere.build |> Shapes.texture { glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; refractive_index = 1.0;} |> Shapes.transform (scaling 0.75 0.75 0.75)
    let g = ShapeGroup.build [s1;s2;]
    let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
    let vt = view_transform (point 0.0 2.5 0.0) (point 0.0 0.0 0.0) (vector 1.0 0.0 0.0)
    let camera = { create_default_camera 3840 2160 with field_of_view = Math.PI / 3.0; transform = vt; }
    let world = Worlds.build [plane;g;] light
    let canvas = render camera world
    canvas_to_jpg "output.jpg" canvas
    *)
    (*
    printfn "Generating Hexagon Rotations..."
    let radians = Math.PI / 180.0
    let mutable r = 1.0 * radians
    for x in 1 .. 360 do
        let world =  Worlds.build [hexagon (rotation_x r)] light
        let canvas = render camera world
        let filename = sprintf "Test_%03i.jpg" x
        canvas_to_jpg filename canvas
        printfn "%s done in %s" filename (sw.Elapsed.ToString())
        r <- float x * radians
    *)
    (*
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
    let g = ShapeGroup.build spheres
    let world = Worlds.build spheres light
    let canvas = render camera world
    canvas_to_jpg "output.jpg" canvas
    *)

    let file = @"C:\Users\josep\Source\Repos\RayTracer\RayTracerCLI\Scenes\table.yml"
    let scene = Scenes.parse_file file
    let (camera,world) = Scenes.scene_to_world scene
    let canvas = render camera world
    canvas_to_jpg "output.jpg" canvas

    printfn "Calculations completed in %s" (sw.Elapsed.ToString())
    0 // return an integer exit code
