namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes2

module ShapeGroup =

    let get_children g =
        match g.shape with
        | Group cs -> cs
        | _ -> raise (Exception "Only groups can have children!")

    let build children = 
        let local_intersect shape trail ray =
            if BoundingBoxes.intersects shape.bounding_box ray then
                let rec loop xs children =
                    match children with
                    | [] -> xs
                    | child :: children ->
                        let xs_p = Shapes2.intersect child (shape :: trail) ray
                        loop (List.append xs_p xs) children
                Shapes2.sort_intersection (loop [] (get_children shape))
            else []
        let local_normal_at hit shape pt = raise (Exception "Groups do not have normals!")
        let bounds_of shape = 
            let func box shape = BoundingBoxes.add_boxes box (parent_space_bounds_of shape)
            get_children shape |> List.fold func BoundingBoxes.build_default
        build (Group(children)) local_intersect local_normal_at bounds_of