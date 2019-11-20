namespace RenderLib

open System
open Tuple
open Matrix
open Ray

module BoundingBoxes =

    type boundingBox_t = {
        minimum: tuple;
        maximum: tuple;
    }

    let build minimum maximum = {
        minimum = minimum;
        maximum = maximum;
    }
        
    let build_default = {
        minimum = point Double.PositiveInfinity Double.PositiveInfinity Double.PositiveInfinity;
        maximum = point Double.NegativeInfinity Double.NegativeInfinity Double.NegativeInfinity;
    }

    let add_point point box = {
        minimum = Tuple.point (Math.Min(point.x, box.minimum.x)) (Math.Min(point.y, box.minimum.y)) (Math.Min(point.z, box.minimum.z));
        maximum = Tuple.point (Math.Max(point.x, box.maximum.x)) (Math.Max(point.y, box.maximum.y)) (Math.Max(point.z, box.maximum.z));
    }

    let add_boxes box1 box2 =
        box1 |> add_point box2.minimum |> add_point box2.maximum        

    let contains_point box point =
        point.x >= box.minimum.x && point.x <= box.maximum.x &&
        point.y >= box.minimum.y && point.y <= box.maximum.y &&
        point.z >= box.minimum.z && point.z <= box.maximum.z

    let contains_box box1 box2 =
        contains_point box1 box2.minimum &&
        contains_point box1 box2.maximum

    let transform (transform:matrix) box =
        let pts = [
            box.minimum;
            point box.minimum.x box.minimum.y box.maximum.z;
            point box.minimum.x box.maximum.y box.minimum.z;
            point box.minimum.x box.maximum.y box.maximum.z;
            point box.maximum.x box.minimum.y box.minimum.z;
            point box.maximum.x box.minimum.y box.minimum.z;
            point box.maximum.x box.maximum.y box.minimum.z;
            box.maximum;
        ]
        let mutable new_box = build_default
        for p in pts do
            new_box <- add_point (transform * p) new_box
        new_box

    let private check_axis origin (direction:float) min max = 
        let tmin_numerator = min - origin
        let tmax_numerator = max - origin
        let mutable tmin = 0.0
        let mutable tmax = 0.0
        if Math.Abs(direction) >= Common.epsilon then
            tmin <- tmin_numerator / direction
            tmax <- tmax_numerator / direction
        else
            tmin <- tmin_numerator * Double.PositiveInfinity
            tmax <- tmax_numerator * Double.PositiveInfinity
        Common.swapIfGreater tmin tmax

    let intersects box ray = 
        let (xtmin, xtmax) = check_axis ray.origin.x ray.direction.x box.minimum.x box.maximum.x
        let mutable tmin = xtmin
        let mutable tmax = xtmax
        let (ytmin, ytmax) = check_axis ray.origin.y ray.direction.y box.minimum.y box.maximum.y
        tmin <- if ytmin > tmin then ytmin else tmin
        tmax <- if ytmax < tmax then ytmax else tmax
        let (ztmin, ztmax) = check_axis ray.origin.z ray.direction.z box.minimum.z box.maximum.z
        tmin <- if ztmin > tmin then ztmin else tmin
        tmax <- if ztmax < tmax then ztmax else tmax        
        if tmin > tmax then false else true

    let split box =
        let dx = Math.Abs(box.maximum.x - box.minimum.x)
        let dy = Math.Abs(box.maximum.y - box.minimum.y)
        let dz = Math.Abs(box.maximum.z - box.minimum.z)
        let greatest = [|dx;dy;dz;|] |> Array.max
        let mutable (x0,y0,z0) = (box.minimum.x,box.minimum.y,box.minimum.z)
        let mutable (x1,y1,z1) = (box.maximum.x,box.maximum.y,box.maximum.z)
        if greatest = dx then
            let a = x0 + dx / 2.0
            x0 <- a
            x1 <- a
        else if greatest = dy then
            let a = y0 + dy / 2.0
            y0 <- a
            y1 <- a
        else 
            let a = z0 + dz / 2.0
            z0 <- a
            z1 <- a
        let mid_min = point x0 y0 z0
        let mid_max = point x1 y1 z1
        let left = build box.minimum mid_max
        let right = build mid_min box.maximum
        (left,right)