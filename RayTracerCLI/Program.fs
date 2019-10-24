﻿// Learn more about F# at http://fsharp.org

open System
open RenderLib
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Material
open RenderLib.Patterns
open RenderLib.Worlds
open RenderLib.Camera
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg
open System.Diagnostics

[<EntryPoint>]
let main argv =

    let red = color 1.0 0.0 0.0
    let blue = color 0.0 0.0 1.0
    let green = color 0.0 1.0 0.0
    let yellow = color 1.0 1.0 0.0

    let canvas_to_jpg (c:Canvas.canvas) =
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
        image.Save("output.jpg", encoder)

    //let p = Solid(red)// blue
    //let p = checkers_pattern (scaling 0.5 0.5 0.5) red blue
    let p = Blended(stripe_pattern_default white blue, stripe_pattern (rotation_y(-Math.PI/2.0)) white blue)
    let planeMaterial = 
        { material.Default with color = color 1.0 0.9 0.9; specular = 0.0; pattern = Some p; }

    let plane = 
        Plane({ shapeProperties.Default with material = planeMaterial;} )
        //Plane({ shapeProperties.Default with material = planeMaterial; default_transformation = (translation 0.0 0.0 10.0) * (rotation_x (Math.PI/2.0)); })

    let middle = 
        let m = { glass with diffuse = 0.01; ambient = 0.2; reflective = 0.9; specular = 1.0; shininess = 300.0; }
        Sphere({ shapeProperties.Default with material = m; default_transformation = translation -0.5 1.0 0.5; })

    let right =
        let m = { material.Default with color = red; diffuse = 0.7; specular = 0.3; }
        Sphere({ shapeProperties.Default with material = m; default_transformation = (translation -0.75 1.5 5.0) * (scaling 0.75 0.75 0.75); })

    let left = 
        let m = { material.Default with color = yellow; diffuse = 0.7; specular = 0.3; }
        Sphere({ shapeProperties.Default with material = m; default_transformation = (translation -1.5 0.33 -0.75) * (scaling 0.33 0.33 0.33); })

    let cube =
        Cube({ shapeProperties.Default with material = { material.Default with color = green; }; default_transformation = (translation -3.5 1.0 0.5) * (scaling 0.75 0.75 0.75) * (rotation_y (Math.PI/3.5)); })

    //let light = { position = point 0.0 10.0 -10.0; intensity = color 1.0 1.0 1.0; }
    let world = { world.Default with objs = [ plane; middle; right; left; cube; ]; }

    //let vt = view_transform (point 0.0 1.5 -5.0) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)
    let vt = view_transform (point 3.0 1.5 -3.5) (point 0.0 1.0 0.0) (vector 0.0 1.0 0.0)
    let camera = { create_default_camera 1920 1200 with transform = vt; }

    printfn "Calculating..."
    let sw = Stopwatch.StartNew()

    let canvas = render camera world

    printfn "Calculations completed in %s" (sw.Elapsed.ToString())
    
    canvas_to_jpg canvas
    printfn "Written to canvas"

    
    0 // return an integer exit code
