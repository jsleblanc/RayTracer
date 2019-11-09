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
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg
open System.Diagnostics
open RenderLib.Matrix
open System.Collections.Generic

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
        Sphere(material.Default,(translation 0.0 0.0 -1.0) * (scaling 0.25 0.25 0.25),None)

    let hexagon_edge =
        Cylinder(material.Default,(translation 0.0 0.0 -1.0) * (rotation_y (-Math.PI/6.0)) * (rotation_z (-Math.PI/2.0)) * (scaling 0.25 1.0 0.25),None,0.0,1.0,false)

    let hexagon_side t =
        let side = Group(None,t,None,new HashSet<shape>())
        side
        |> with_child hexagon_corner
        |> with_child hexagon_edge

    let hexagon t = 
        let hex = Group(None,t,None,new HashSet<shape>())
        for x in 0 .. 5 do
            let v = float x
            let side = hexagon_side (rotation_y (v*Math.PI/3.0))
            with_child side hex
        hex

    let g = Group(None,identity_matrix(),None,new HashSet<shape>())

    //let p = Solid(red)// blue
    let p = checkers_pattern (scaling 0.5 0.5 0.5) white blue
    //let p = Blended(stripe_pattern_default white blue, stripe_pattern (rotation_y(-Math.PI/2.0)) white blue)
    let planeMaterial = 
        { material.Default with color = color 1.0 0.9 0.9; specular = 0.0; pattern = Some p; }

    let plane = Plane(planeMaterial,identity_matrix(),None)
        //Plane({ shapeProperties.Default with material = planeMaterial; default_transformation = (translation 0.0 0.0 10.0) * (rotation_x (Math.PI/2.0)); })

    let middle = 
        let m = { glass with diffuse = 0.01; ambient = 0.02; reflective = 0.9; specular = 1.0; shininess = 300.0; }
        Sphere(m,translation -0.5 1.0 0.5,None)

    let right =
        let m = { material.Default with color = red; diffuse = 0.7; specular = 0.3; }
        Sphere(m,(translation -0.75 1.5 5.0) * (scaling 0.75 0.75 0.75),None)

    let left = 
        let m = { material.Default with color = yellow; diffuse = 0.7; specular = 0.3; }
        Sphere(m,(translation -1.5 0.33 -0.75) * (scaling 0.33 0.33 0.33),None)

    let cube =
        Cube({ material.Default with color = green; },(translation -3.5 1.0 0.5) * (scaling 0.75 0.75 0.75) * (rotation_y (Math.PI/3.5)),None)

    let cylinder =
        Cylinder({ material.Default with color = green; },(translation -3.5 1.0 0.5) * (scaling 0.75 0.75 0.75) * (rotation_y (Math.PI/3.5)),None,1.0,3.0,true)

    //with_child g plane
    with_child middle g
    with_child right g
    with_child left g
    with_child cube g
    with_child cylinder g

    //((rotation_z (Math.PI / -3.0)) * (rotation_x (Math.PI/2.0)))
    
    let vt = view_transform (point 0.0 1.5 -3.0) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)    
    let camera = { create_default_camera 640 480 with transform = vt; }
    let light = { position = point 0.0 10.0 -10.0; intensity = color 1.0 1.0 1.0; }
        
    (*
    //sphere inside a sphere 
    let pt = checkers_pattern (translation 0.0 0.1 0.0) blue white
    let plane = Plane({ material.Default with pattern = Some pt; },translation 0.0 -10.1 0.0,None)
    let s1 = Sphere({glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; },scaling 1.25 1.25 1.25,None)
    let s2 = Sphere({glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; refractive_index = 1.0;},scaling 0.75 0.75 0.75,None)
    let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
    let vt = view_transform (point 0.0 2.5 0.0) (point 0.0 0.0 0.0) (vector 1.0 0.0 0.0)
    let camera = { create_default_camera 3840 2160 with field_of_view = Math.PI / 3.0; transform = vt; }
    let world = { world.Default with light = light; objs = [plane;s1;s2;];}
    *)

    printfn "Calculating..."
    let sw = Stopwatch.StartNew()   

    let radians = Math.PI / 180.0
    let mutable r = 1.0 * radians
    for x in 1 .. 360 do
        let world = { world.Default with light = light; objs = [ hexagon (rotation_x r); ]; }    
        let canvas = render camera world
        let filename = sprintf "Test_%03i.jpg" x
        canvas_to_jpg filename canvas
        printfn "%s done in %s" filename (sw.Elapsed.ToString())
        r <- float x * radians

    //let canvas = render camera world

    printfn "Calculations completed in %s" (sw.Elapsed.ToString())
    
    //canvas_to_jpg canvas
    //printfn "Written to canvas"

    
    0 // return an integer exit code
