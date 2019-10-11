// Learn more about F# at http://fsharp.org

open System
open System.IO
open RenderLib
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Shapes
open RenderLib.Ray
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg

[<EntryPoint>]
let main argv =

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
    let canvas_pixels = 100
    let pixel_size = wall_size / float canvas_pixels
    let half = wall_size / 2.0

    let canvas = Canvas.create_canvas canvas_pixels canvas_pixels
    let color = color 1.0 0.0 0.0
    let shape = sphere()

    for y in 0 .. canvas_pixels - 1 do
        let world_y = half - pixel_size * float y
        for x in 0 .. canvas_pixels - 1 do
            let world_x = -half + pixel_size * float x
            let position = point world_x world_y wall_z
            let r = { origin = ray_origin; direction = (position - ray_origin).normalize(); }
            let xs = intersect shape r
            match hit xs with
            | Some i -> Canvas.write_pixel x y color canvas
            | None -> canvas
            |> ignore

    canvas_to_jpg canvas
    

    
    0 // return an integer exit code
