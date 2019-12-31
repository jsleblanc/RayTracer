namespace RenderLib

open System
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.ColorSpaces
open SixLabors.ImageSharp.ColorSpaces.Conversion
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg
open SixLabors.ImageSharp.Formats.Png
open Color

module Canvas = 

    type canvas = color[,]
        
    let private canvas_to_image (c:canvas) =
        let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x) 
        let conv = new ColorSpaceConverter()
        let width = c.GetLength(0)
        let height = c.GetLength(1)
        let linearRgb = 
            seq {
                for y in 0 .. c.GetLength(1) - 1 do
                    for x in 0 .. c.GetLength(0) - 1 do
                        let c = c.[x,y]
                        let r = single c.red
                        let g = single c.green
                        let b = single c.blue
                        yield new ColorSpaces.LinearRgb(r, g, b)
                }
            |> Seq.toArray
        let span = new ReadOnlySpan<LinearRgb>(linearRgb)
        let dst = new Span<Rgb>(Array.zeroCreate span.Length)
        conv.Convert(span, dst)
        let rgb24 = 
            let a = Array.zeroCreate dst.Length
            let f (x:Rgb) : Rgb24 = !> x
            for x in 0 .. dst.Length - 1 do
                a.[x] <- f dst.[x]
            a
        Image.LoadPixelData<Rgb24>(rgb24, width, height)

    let private canvas_to_jpg canvas filename = 
        let encoder = new JpegEncoder()
        encoder.Quality <- new Nullable<int>(100)
        let image = canvas_to_image canvas
        let outputFileName = Path.ChangeExtension(filename, "jpg")
        image.Save(outputFileName, encoder)
        new FileInfo(outputFileName)

    let private canvas_to_png canvas filename = 
        let encoder = new PngEncoder()
        encoder.ColorType <- new Nullable<PngColorType>(PngColorType.Rgb)
        encoder.BitDepth <- new Nullable<PngBitDepth>(PngBitDepth.Bit8)
        let image = canvas_to_image canvas
        let outputFileName = Path.ChangeExtension(filename, "png")
        image.Save(outputFileName, encoder)
        new FileInfo(outputFileName)
        
    type canvas_t = {
        canvas:canvas;
        save_jpg:string -> FileInfo;
        save_png:string -> FileInfo;
    }

    let private create_canvas x y : canvas =
        let black = color 0.0 0.0 0.0
        Array2D.create x y black

    let build_canvas x y : canvas_t = 
        let c = create_canvas x y;
        {
            canvas = c;
            save_jpg = canvas_to_jpg c;
            save_png = canvas_to_png c;
        }

    let write_pixel x y color (cv:canvas_t) =
        cv.canvas.[x,y] <- color
        cv
