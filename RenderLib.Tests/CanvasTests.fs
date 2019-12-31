namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Color
open RenderLib.Canvas

module CanvasTests = 

    [<Fact>]
    let ``creating a canvas``() =
        let black = color 0.0 0.0 0.0
        let canvas = build_canvas 10 10
        let allPixels =
            seq {
                for y = 0 to 9 do
                    for x = 0 to 9 do
                        yield (x,y)
            }
        let result = Seq.forall (fun (x,y) -> canvas.canvas.[x,y] = black) allPixels
        Assert.True(result)

    [<Fact>]
    let ``writing pixels to a canvas``() =
        let canvas = build_canvas 10 20
        let red = color 1.0 0.0 0.0
        write_pixel 2 3 red canvas |> ignore
        let result = canvas.canvas.[2,3] = red
        Assert.True(result)