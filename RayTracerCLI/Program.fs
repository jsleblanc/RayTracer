// Learn more about F# at http://fsharp.org

open System
open System.IO
open RenderLib
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg

[<EntryPoint>]
let main argv =

    let canvas_to_jpg (c:Canvas.canvas) =
        let image = new Image<Rgb24>(c.GetLength(1), c.GetLength(0))
        for y in 0 .. c.GetLength(0) - 1 do
            for x in 0 .. c.GetLength(1) - 1 do
                let (r,g,b) = Color.color_byte c.[x,y]
                let pixel = new Rgb24(r,g,b)
                image.[x,y] <- pixel
        image.Save("output.jpg", new JpegEncoder())


    let c = Canvas.create_canvas 200 200
    let o = Tuple.point 0.0 0.0 0.0
    let center = Tuple.point 100.0 0.0 100.0

    let red = Color.color 1.0 1.0 1.0
    let radius = 3.0/8.0

    let noon = Tuple.point 0.0 0.0 1.0
    for hour in 1 .. 11 do
        let r = Math.PI / 6.0
        let p = Translations.rotation_y (r * float hour) * noon
        let px = p.x * radius
        let pz = p.z * radius
        let p = Tuple.point px 0.0 pz
        let cp = p + center
        let ix = int cp.x
        let iy = int cp.z
        Canvas.write_pixel ix iy red c |> ignore

    canvas_to_jpg c

    
    0 // return an integer exit code
