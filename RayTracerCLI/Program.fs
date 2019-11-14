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
        let side = Group(Some material.Default,t,None,new HashSet<shape>())
        side
        |> with_child hexagon_corner
        |> with_child hexagon_edge

    let hexagon t =         
        let hex = Group(None,t,None,new HashSet<shape>())
        for x in 0 .. 5 do
            let side = hexagon_side (rotation_y (float x*Math.PI/3.0))
            with_child side hex
        hex

    let h = hexagon ((rotation_x (Math.PI / -3.0)))// * (rotation_x (Math.PI/2.0)))
    
    let vt = view_transform (point 0.0 0.0 -3.0) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)    
    let camera = { create_default_camera 640 480 with transform = vt; }
    let light = { position = point 0.0 10.0 -10.0; intensity = color 1.0 1.0 1.0; }
    
    let us = Sphere(material.Default,(rotation_z (Math.PI/4.0))*(rotation_z (Math.PI/3.0))*(rotation_z (Math.PI/3.0))*(scaling 1.0 0.5 0.5),None)

    let sr = Sphere({ material.Default with color = blue; },identity_matrix(),None)
    let g1 = Group(None,(rotation_z (Math.PI/3.0)),None,new HashSet<shape>())
    let s = Sphere(material.Default,(rotation_z (Math.PI/3.0))*(scaling 1.0 0.5 0.5),Some g1)
    //let s = Cylinder(material.Default,(scaling 1.0 0.5 0.5),Some g1,0.0,1.0,false)
    with_child s g1
    let g2 = Group(None,(rotation_z (Math.PI/4.0)),Some g1,new HashSet<shape>())
    with_child g1 g2
    let world = { world.Default with light = light; objs = [h;]; }    

    printfn "Calculating..."
    let sw = Stopwatch.StartNew()   

    let canvas = render camera world
    canvas_to_jpg "output.jpg" canvas

    (*
    let radians = Math.PI / 180.0
    let mutable r = 53.0 * radians
    //for x in 1 .. 360 do
    let x = 53
    let world = { world.Default with light = light; objs = [ hexagon (rotation_x r); ]; }    
    let canvas = render camera world
    let filename = sprintf "Test_%03i.jpg" x
    canvas_to_jpg filename canvas
    printfn "%s done in %s" filename (sw.Elapsed.ToString())
    r <- float x * radians
  *)

    //let corner_transform = (translation 0.0 0.0 -1.0) * (scaling 0.25 0.25 0.25)// * (rotation_x r)
    //let edge_transform = (translation 0.0 0.0 -1.0) * (rotation_y (-Math.PI/6.0)) * (rotation_z (-Math.PI/2.0)) * (scaling 0.25 1.0 0.25)// * (rotation_x r)

    let ray = ray_for_pixel camera 260 375
    let color = color_at world ray 5

    //let canvas = render camera world

    printfn "Calculations completed in %s" (sw.Elapsed.ToString())
    
    //canvas_to_jpg canvas
    //printfn "Written to canvas"

    
    0 // return an integer exit code
