namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes

module ShapeGroup =

    let get_children g =
        match g.shape with
        | Group cs -> cs
        | _ -> raise (Exception "Only groups can have children!")

    let rec divide (threshold:int) (shape:shape) (build_fn:shape list -> shape) =
        let get_children shape = 
            match shape.shape with
            | Group g -> g
            | _ -> [shape]
        let test_box box shape =
            match BoundingBoxes.contains_box box (parent_space_bounds_of shape) with
            | true -> Some shape
            | false -> None
        let make_subgroup shapes =
            match shapes with 
            | [] -> None
            | s -> Some (build_fn s)
        let divide_if_above_threshold shape =
            if List.length (get_children shape) > threshold then divide threshold shape build_fn
            else shape
        let (left_box,right_box) = BoundingBoxes.split shape.bounding_box
        let children = get_children shape
        let left_shapes = children |> List.map (test_box left_box) |> List.choose id
        let right_shapes = children |> List.map (test_box right_box) |> List.choose id
        let other_shapes = children |> List.except left_shapes |> List.except right_shapes        
        let left_group = make_subgroup left_shapes |> Option.map divide_if_above_threshold
        let right_group = make_subgroup right_shapes |> Option.map divide_if_above_threshold        
        let groups = [left_group;right_group;] |> List.choose id
        build_fn (groups @ other_shapes)

    let build children = 
        let local_intersect shape trail ray =
            if BoundingBoxes.intersects shape.bounding_box ray then
                let rec loop xs children =
                    match children with
                    | [] -> xs
                    | child :: children ->
                        let xs_p = Shapes.intersect child (shape :: trail) ray
                        loop (List.append xs_p xs) children
                Shapes.sort_intersection (loop [] (get_children shape))
            else []
        let local_normal_at hit shape pt = raise (Exception "Groups do not have normals!")
        let bounds_of shape = 
            let func box shape = BoundingBoxes.add_boxes box (parent_space_bounds_of shape)
            get_children shape |> List.fold func BoundingBoxes.build_default
        build (Group(children)) local_intersect local_normal_at bounds_of        

