namespace RenderLib

open System.Text
open Color

module Canvas = 

    type canvas = color[,]

    let create_canvas x y : canvas =
        let black = color 0.0 0.0 0.0
        Array2D.create x y black

    let write_pixel x y color (cv:canvas) =
        cv.[x,y] <- color
        cv

    let canvas_to_ppm (c:canvas) : string =
        let appendLine s (sb:StringBuilder) = sb.AppendLine s
        let appendLines (sq:seq<string>) (sb:StringBuilder) = 
            sq |> Seq.iter (fun (s) -> sb.AppendLine(s) |> ignore)
            sb
        let toString sb = sb.ToString()
        let colorString c = 
            let r,g,b = color_byte c
            sprintf "%d %d %d" r g b
        let allPixels = 
            seq {
                for y = 0 to c.GetLength(0) - 1 do
                    for x = 0 to c.GetLength(1) - 1 do   
                        yield (x,y)
            }
        let pixelLines =
            allPixels 
                |> Seq.map (fun (x,y) -> c.[x,y])
                |> Seq.chunkBySize 6
                |> Seq.map (fun (x) -> Seq.map colorString x)
                |> Seq.map (fun (x) -> x |> String.concat " ")
        new StringBuilder()
        |> appendLine "P3"
        |> appendLine (sprintf "%d %d" (c.GetLength(0)) (c.GetLength(1)))
        |> appendLine "255"
        |> appendLines pixelLines
        |> appendLine ""
        |> toString