// Learn more about F# at http://fsharp.org

open System
open RenderLib
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Shapes2
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

    let hexagon_corner =
        ShapeSphere.build
        |> Shapes2.transform ((translation 0.0 0.0 -1.0) * (scaling 0.25 0.25 0.25))

    let hexagon_edge =
        ShapeCylinder.build 0.0 1.0 false
        |> Shapes2.transform ((translation 0.0 0.0 -1.0) * (rotation_y (-Math.PI/6.0)) * (rotation_z (-Math.PI/2.0)) * (scaling 0.25 1.0 0.25))

    let hexagon_side t =
        ShapeGroup.build [hexagon_corner;hexagon_edge;] |> Shapes2.transform t

    let hexagon t = 
        let mutable sides = []
        for x in 0 .. 5 do
            let side = hexagon_side (rotation_y (float x * Math.PI / 3.0))
            sides <- side::sides
        ShapeGroup.build sides |> Shapes2.transform t

    //let p = Solid(red)// blue
    
    //let p = Blended(stripe_pattern_default white blue, stripe_pattern (rotation_y(-Math.PI/2.0)) white blue)
    //let planeMaterial = { material.Default with color = color 1.0 0.9 0.9; specular = 0.0; pattern = Some p; }

    let pt = Patterns.checkers (solid_c blue) (solid_c white) |> Patterns.transform (scaling 0.5 0.5 0.5)
    let plane = 
        ShapePlane.build
        |> Shapes2.texture { Material.material.Default with color = color 1.0 0.9 0.9; specular = 0.0; pattern = Some pt; }
        //|> Shapes2.transform ((translation 0.0 0.0 10.0) * (rotation_x (Math.PI/2.0)))

    let middle = 
        ShapeSphere.build
        |> Shapes2.texture { glass with diffuse = 0.01; ambient = 0.02; reflective = 0.9; specular = 1.0; shininess = 300.0; }
        |> Shapes2.transform (translation -0.5 1.0 0.5)

    let right =
        ShapeSphere.build
        |> Shapes2.texture { Material.material.Default with color = red; diffuse = 0.7; specular = 0.3; }
        |> Shapes2.transform ((translation -0.75 1.5 5.0) * (scaling 0.75 0.75 0.75))

    let left = 
        ShapeSphere.build
        |> Shapes2.texture { Material.material.Default with color = yellow; diffuse = 0.7; specular = 0.3; }
        |> Shapes2.transform ((translation -1.5 0.33 -0.75) * (scaling 0.33 0.33 0.33))

    let cube =
        ShapeCube.build
        |> Shapes2.texture { Material.material.Default with color = green; }
        |> Shapes2.transform ((translation -3.5 1.0 0.5) * (scaling 0.75 0.75 0.75) * (rotation_y (Math.PI/3.5)))

    //let cylinder = Cylinder({ material.Default with color = green; },(translation -3.5 1.0 0.5) * (scaling 0.75 0.75 0.75) * (rotation_y (Math.PI/3.5)),None,1.0,3.0,true)
    //((rotation_z (Math.PI / -3.0)) * (rotation_x (Math.PI/2.0)))
    (*
    let vt = view_transform (point 0.0 1.5 -3.0) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)    
    let camera = { create_default_camera 640 480 with transform = vt; }
    let light = { position = point 0.0 10.0 -10.0; intensity = color 1.0 1.0 1.0; }

    printfn "Calculating..."
    let sw = Stopwatch.StartNew()   

    let world = Worlds.build [left;right;middle;cube;plane;] light
    let canvas = render camera world
    canvas_to_jpg "output.jpg" canvas    
    printfn "Calculations completed in %s" (sw.Elapsed.ToString())
    *)
        (*
    let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
    let vt = view_transform (point 0.0 2.5 0.0) (point 0.0 0.0 0.0) (vector 1.0 0.0 0.0)
    let camera = { create_default_camera 640 480 with field_of_view = Math.PI / 3.0; transform = vt; }
    let pt = Patterns.checkers (solid_c blue) (solid_c white) |> Patterns.transform (translation 0.0 0.1 0.0)
    let default_world = 
        let plane = 
            ShapePlane.build
            |> Shapes2.transform (translation 0.0 -10.1 0.0)
            |> Shapes2.texture { Material.material.Default with pattern = Some pt; }
        let s2 = 
            ShapeSphere.build
            |> Shapes2.texture Material.material.Default
            |> Shapes2.transform (scaling 0.5 0.5 0.5)
        let w = Worlds.build_default [s2; plane;]
        w
    let world = default_world
    let canvas = render camera world
    canvas_to_jpg "output.jpg" canvas
    *)
    (*
    //sphere inside a sphere 
    let pt = Patterns.checkers (solid_c blue) (solid_c white) |> Patterns.transform (translation 0.0 0.1 0.0)
    let plane = ShapePlane.build |> Shapes2.texture { Material.material.Default with pattern = Some pt; } |> Shapes2.transform (translation 0.0 -10.1 0.0)
    let s1 = ShapeSphere.build |> Shapes2.texture { glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; } |> Shapes2.transform (scaling 1.25 1.25 1.25)
    let s2 = ShapeSphere.build |> Shapes2.texture { glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; refractive_index = 1.0;} |> Shapes2.transform (scaling 0.75 0.75 0.75)
    let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
    let vt = view_transform (point 0.0 2.5 0.0) (point 0.0 0.0 0.0) (vector 1.0 0.0 0.0)
    let camera = { create_default_camera 3840 2160 with field_of_view = Math.PI / 3.0; transform = vt; }
    let world = Worlds.build [plane;s1;s2;] light
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
    
    let spheres = [
        ShapeSphere.build |> Shapes2.transform (translation -3.0 -3.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation -3.0 -3.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation -3.0 -3.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation -3.0 0.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation -3.0 0.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation -3.0 0.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation -3.0 3.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation -3.0 3.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation -3.0 3.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation 0.0 -3.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation 0.0 -3.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation 0.0 -3.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation 0.0 0.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation 0.0 0.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation 0.0 0.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation 0.0 3.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation 0.0 3.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation 0.0 3.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation 3.0 -3.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation 3.0 -3.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation 3.0 -3.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation 3.0 0.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation 3.0 0.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation 3.0 0.0 9.0);

        ShapeSphere.build |> Shapes2.transform (translation 3.0 3.0 3.0);
        ShapeSphere.build |> Shapes2.transform (translation 3.0 3.0 6.0);
        ShapeSphere.build |> Shapes2.transform (translation 3.0 3.0 9.0);
    ]
    let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
    let vt = view_transform (point 0.0 1.5 -3.0) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)
    let camera = { create_default_camera 1027 768 with field_of_view = Math.PI; transform = vt; }
    let world = Worlds.build spheres light
    //let canvas = render camera world
    //canvas_to_jpg "output.jpg" canvas

    let ray = {
        origin = point 4.0 4.0 4.0;
        direction = vector 0.0 1.0 0.0;
    }
    let x = color_at world ray 5

    0 // return an integer exit code
