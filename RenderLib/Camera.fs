namespace RenderLib

open System
open Canvas
open Matrix
open Tuple
open Ray
open Worlds
open FSharp.Collections.ParallelSeq

module Camera = 

    type camera = {
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
        match inverse camera.transform with
        | Ok ict -> 
            let pixel = ict * (point world_x world_y -1.0)
            let origin = ict * (point 0.0 0.0 0.0)
            let direction = (pixel - origin).normalize()
            {
                origin = origin;
                direction = direction;
            }
        | Error s -> raise (Exception(s)) //TODO - decide how I want this to propagate through the code; exception is temporary

    let render camera world =
        let image = create_canvas camera.hsize camera.vsize
        let coords = seq {
            for y in 0 .. camera.vsize - 1 do
                for x in 0 .. camera.hsize - 1 do
                    yield (x,y)
            }
        coords
        |> Seq.toList
        |> PSeq.map (fun (x,y) -> 
                let ray = ray_for_pixel camera x y
                let color = color_at world ray
                (color, x, y)
            )
        |> Seq.iter (fun (c, x, y) -> write_pixel x y c image |> ignore)
        image
