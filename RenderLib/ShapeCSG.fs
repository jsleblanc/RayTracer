namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes

module ShapeCSG =

    let private get_children shape = 
        match shape.shape with
        | Union (l,r,_) -> [l;r;]
        | Intersect (l,r,_) -> [l;r;]
        | Difference (l,r,_) -> [l;r;]
        | _ -> failwith "expected a CSG operation to get_children"

    let private intersection_allowed_union lhit inl inr =
        (lhit && not inr) || (not lhit && not inl)

    let private intersection_allowed_intersect lhit inl inr =
        (lhit && inr) || (not lhit && inl)

    let private intersection_allowed_difference lhit inl inr =
        (lhit && not inr) || (not lhit && inl)

    let filter_intersections shape xs =
        let (left,right,rule) =
            match shape.shape with
            | Union (l,r,fn) -> (l,r,fn)
            | Intersect (l,r,fn) -> (l,r,fn)
            | Difference (l,r,fn) -> (l,r,fn)
            | _ -> failwith "expected a CSG operation to filter_intersections"
        let rec is_included child parent =
            match parent.shape with
            | Group children -> List.exists (is_included child) children
            | Union (l,r,_) -> (is_included child l) || (is_included child r)
            | Intersect (l,r,_) -> (is_included child l) || (is_included child r)
            | Difference (l,r,_) -> (is_included child l) || (is_included child r)
            | _ -> parent = child
        let rec loop inl inr acc xs =
            match xs with
            | [] -> List.rev acc
            | i :: xs_p ->
                let lhit = is_included i.obj left
                let acc_p = if rule lhit inl inr then i :: acc else acc
                let (inl_p,inr_p) = if lhit then (not inl,inr) else (inl,not inr)
                loop inl_p inr_p acc_p xs_p
        loop false false [] xs

    let private build operation =
        let local_normal_at hit shape pt = failwith "should not calculate normal on CSG shape itself!"
        let local_intersect shape trail ray = 
            if BoundingBoxes.intersects shape.bounding_box ray then
                let (left,right) =
                    match shape.shape with
                    | Union (l,r,_) -> (l,r)
                    | Intersect (l,r,_) -> (l,r)
                    | Difference (l,r,_) -> (l,r)
                    | _ -> failwith "expected a CSG operation to local_intersect"
                let trail = (shape :: trail)
                let leftxs = Shapes.intersect left trail ray
                let rightxs = Shapes.intersect right trail ray
                let xs = Shapes.sort_intersection (List.append (List.rev leftxs) rightxs)
                filter_intersections shape xs
            else []
        let bounds_of shape = 
            let func box shape = BoundingBoxes.add_boxes box (parent_space_bounds_of shape)
            get_children shape |> List.fold func BoundingBoxes.build_default
        build operation local_intersect local_normal_at bounds_of

    let union left right = 
        build (Union(left,right,intersection_allowed_union))

    let intersect left right = 
        build (Intersect(left,right,intersection_allowed_intersect))

    let difference left right =
        build (Difference(left,right,intersection_allowed_difference))