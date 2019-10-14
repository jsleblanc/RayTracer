// Learn more about F# at http://fsharp.org

open System
open System.IO
open RenderLib
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Shapes
open RenderLib.Ray
open RenderLib.Translations
open RenderLib.Lights
open RenderLib.Ray
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg
open System.Diagnostics

[<EntryPoint>]
let main argv =

    let white = color 1.0 1.0 1.0
    let black = color 0.0 0.0 0.0

    let canvas_to_jpg (c:Canvas.canvas) =
        let encoder = new JpegEncoder()
        encoder.Quality <- new Nullable<int>(100)
        let image = new Image<Rgb24>(c.GetLength(1), c.GetLength(0))
        for y in 0 .. c.GetLength(0) - 1 do
            for x in 0 .. c.GetLength(1) - 1 do
                let (r,g,b) = Color.color_byte c.[x,y]
                let pixel = new Rgb24(r,g,b)
                image.[x,y] <- pixel
        image.Save("output.jpg", encoder)


    let ray_origin = point 0.0 0.0 -5.0
    let wall_z = 10.0
    let wall_size = 7.0
    let canvas_pixels = 1000
    let pixel_size = wall_size / float canvas_pixels
    let half = wall_size / 2.0

    let canvas = Canvas.create_canvas canvas_pixels canvas_pixels
    let shape = sphere()
    shape.material <- { shape.material with color = color 1.0 0.2 1.0; }
    let light_position = point -10.0 10.0 -10.0
    let light_color = white
    let light = {
        position = light_position;
        intensity = light_color;
    }

    //shape.default_transformation <- rotation_z (Math.PI / 4.0) * scaling 0.5 1.0 1.0

    printfn "Calculating..."
    let sw = Stopwatch.StartNew()
    for y in 0 .. canvas_pixels - 1 do
        let world_y = half - pixel_size * float y
        for x in 0 .. canvas_pixels - 1 do
            let world_x = -half + pixel_size * float x
            let pos = point world_x world_y wall_z
            let r = { origin = ray_origin; direction = (pos - ray_origin).normalize(); }
            let xs = intersect shape r
            match hit xs with
            | Some i -> 
                let p = position r i.t
                let normal = normal_at shape p
                let eye = -r.direction
                let color = lighting shape.material light p eye normal
                Canvas.write_pixel x y color canvas
            | None -> canvas
            |> ignore

    printfn "Calculations completed in %s" (sw.Elapsed.ToString())
    
    canvas_to_jpg canvas
    printfn "Written to canvas"

    
    0 // return an integer exit code
