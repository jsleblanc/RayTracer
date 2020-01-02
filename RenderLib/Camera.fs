namespace RenderLib

open System
open System.Diagnostics
open Canvas
open Matrix
open Tuple
open Ray
open Worlds
open FSharp.Collections.ParallelSeq

module Camera = 

    type camera_t = {
        hsize: int;
        vsize: int;
        field_of_view: float;
        transform: matrix
        pixel_size: float;
        half_width: float;
        half_height: float;
    } 

    let private calculate_camera_fields hsize vsize field_of_view =
        let half_view = Math.Tan (field_of_view / 2.0)
        let aspect = (float) hsize / (float) vsize
        let mutable half_width = 0.0
        let mutable half_height = 0.0
        if aspect >= 1.0 then
            half_width <- half_view
            half_height <- half_view / aspect
        else
            half_width <- half_view * aspect
            half_height <- half_view
        let pixel_size = (half_width * 2.0) / (float) hsize
        (half_width, half_height, pixel_size)

    
    let create_camera hsize vsize field_of_view =
        let (hw, hh, ps) = calculate_camera_fields hsize vsize field_of_view
        {
            hsize = hsize;
            vsize = vsize;
            field_of_view = field_of_view;
            transform = identity_matrix ();
            pixel_size = ps;
            half_height = hh;
            half_width = hw;
        }

    let create_default_camera hsize vsize =
        create_camera hsize vsize (Math.PI / 2.0)

    let ray_for_pixel camera (px:int) (py:int) =
        let xoffset = ((float)px + 0.5) * camera.pixel_size
        let yoffset = ((float)py + 0.5) * camera.pixel_size
        let world_x = camera.half_width - xoffset
        let world_y = camera.half_height - yoffset
        let ict = inverse camera.transform
        let pixel = ict * (point world_x world_y -1.0)
        let origin = ict * (point 0.0 0.0 0.0)
        let direction = (pixel - origin).normalize()
        {
            origin = origin;
            direction = direction;
        }

    type chunk_t = {
        size:int;
        coords:(int*int)[];
    }
    
    type rendered_chunk_t = {
        size:int;
        pixels:(Color.color*int*int)[];
        renderTime:TimeSpan;
    }
    
    type rendered_scene_t = {
        canvas:canvas_t
        chunkRenderTimes:TimeSpan list
        slowestChunk:TimeSpan
        fastestChunk:TimeSpan
        averageChunk:TimeSpan;
    }
        
    let render camera world =
        let coords = seq {
            for y in 0 .. camera.vsize - 1 do
                for x in 0 .. camera.hsize - 1 do
                    yield (x,y)
            }
        let calcPixel (x,y) = 
            let ray = ray_for_pixel camera x y
            let color = color_at world ray 5
            (color, x, y)
        let calc_chunk chunk =
            let sw = Stopwatch.StartNew()
            let pixels = chunk.coords |> Array.map calcPixel
            sw.Stop()
            {
                size = chunk.size
                pixels = pixels
                renderTime = sw.Elapsed;
            }
        let totalPixels = camera.hsize * camera.vsize
        let pixelsPerChunk = totalPixels / Environment.ProcessorCount
        let canvas = build_canvas camera.hsize camera.vsize
        let renderedChunks = 
            coords
            |> Seq.toArray
            |> Array.chunkBySize pixelsPerChunk
            |> Array.map (fun c -> {
                size = Array.length c
                coords = c;
            })
            |> PSeq.map calc_chunk
            |> Seq.toArray
        let renderedPixels = renderedChunks |> Array.collect (fun c -> c.pixels)
        let renderTimes = renderedChunks |> Array.map (fun c -> c.renderTime)
        renderedPixels |> Seq.iter (fun (c, x, y) -> write_pixel x y c canvas |> ignore)
        {
            canvas = canvas
            chunkRenderTimes = Array.toList renderTimes;
            slowestChunk = Array.max renderTimes;
            fastestChunk = Array.min renderTimes
            averageChunk = TimeSpan.Zero; //Array.average renderTimes;
        }
