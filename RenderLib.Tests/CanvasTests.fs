namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Color
open RenderLib.Canvas

module CanvasTests = 

    [<Fact>]
    let ``creating a canvas``() =
        let black = color 0.0 0.0 0.0
        let canvas = create_canvas 10 10
        let allPixels =
            seq {
                for y = 0 to 9 do
                    for x = 0 to 9 do
                        yield (x,y)
            }
        let result = Seq.forall (fun (x,y) -> canvas.[x,y] = black) allPixels
        Assert.True(result)

    [<Fact>]
    let ``writing pixels to a canvas``() =
        let canvas = create_canvas 10 20
        let red = color 1.0 0.0 0.0
        write_pixel 2 3 red canvas |> ignore
        let result = canvas.[2,3] = red
        Assert.True(result)

    [<Fact>]
    let ``test``() =
        let c = create_canvas 20 20
        for y = 0 to 19 do
            for x = 0 to 19 do 
                write_pixel x y (color 1.0 0.8 0.6) c |> ignore
        let x = canvas_to_ppm c

        let s = 
            create_canvas 20 20
            |> write_pixel 5 5 (color 1.0 0.0 0.0)
            |> write_pixel 6 5 (color 1.0 0.0 0.0)
            |> write_pixel 7 5 (color 1.0 0.0 0.0)
            |> write_pixel 8 5 (color 1.0 0.0 0.0)
            |> write_pixel 9 5 (color 1.0 0.0 0.0)
            |> canvas_to_ppm        
        Assert.True(true)
