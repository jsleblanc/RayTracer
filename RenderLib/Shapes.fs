namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open System.Collections.Generic

module Shapes = 

    type shape =
    | Plane of material:material * transformation:matrix * Parent:shape option
    | Sphere of material:material * transformation:matrix * Parent:shape option
    | Cube of material:material * transformation:matrix * Parent:shape option
    | Cylinder of material:material * transformation:matrix * Parent:shape option * Minimum:float * Maximum:float * Closed:bool
    | Cone of material:material * transformation:matrix * Parent:shape option * Minimum:float * Maximum:float * Closed:bool
    | Group of material:material * transformation:matrix * Parent:shape option * Children:HashSet<shape>

    type intersection = {
        t: float;
        obj: shape;
    }

    type precomputed = {
        t: float;
        obj: shape;
        point: tuple;
        eyev: tuple;
        normalv: tuple;
        reflectv: tuple;
        inside: bool;
        over_point: tuple;
        under_point: tuple;
        n1: float;
        n2: float;
    }

    type boundingBox = {
        minimum: tuple;
        maximum: tuple;
    } with static member Default = {
            minimum = point Double.PositiveInfinity Double.PositiveInfinity Double.PositiveInfinity;
            maximum = point Double.NegativeInfinity Double.NegativeInfinity Double.NegativeInfinity;
        }

    let shapeTransformation shape = 
        match shape with
        | Plane (_,t,_) -> t
        | Sphere (_,t,_) -> t
        | Cube (_,t,_) -> t
        | Cylinder (_,t,_,_,_,_) -> t
        | Cone (_,t,_,_,_,_) -> t
        | Group (_,t,_,_) -> t

    let shapeMaterial shape =
        match shape with
        | Plane (m,_,_) -> m
        | Sphere (m,_,_) -> m
        | Cube (m,_,_) -> m
        | Cylinder (m,_,_,_,_,_) -> m
        | Cone (m,_,_,_,_,_) -> m
        | Group (m,t,_,_) -> m

    let shapeParent shape =
        match shape with
        | Plane (_,_,p) -> p
        | Sphere (_,_,p) -> p
        | Cube (_,_,p) -> p
        | Cylinder (_,_,p,_,_,_) -> p
        | Cone (_,_,p,_,_,_) -> p
        | Group (_,_,p,_) -> p
        | _ -> None

    let private swapIfGreater min max =
        if min > max then
            (max, min)
        else
            (min, max)            

    let private check_axis origin (direction:float) min max = 
        let tmin_numerator = min - origin
        let tmax_numerator = max - origin
        let mutable tmin = 0.0
        let mutable tmax = 0.0
        if Math.Abs(direction) >= epsilon then
            tmin <- tmin_numerator / direction
            tmax <- tmax_numerator / direction
        else
            tmin <- tmin_numerator * Double.PositiveInfinity
            tmax <- tmax_numerator * Double.PositiveInfinity
        swapIfGreater tmin tmax

    let boundingBoxAddPoint pt (box:boundingBox) =
        {
            minimum = point (Math.Min(pt.x, box.minimum.x)) (Math.Min(pt.y, box.minimum.y)) (Math.Min(pt.z, box.minimum.z));
            maximum = point (Math.Max(pt.x, box.maximum.x)) (Math.Max(pt.y, box.maximum.y)) (Math.Max(pt.z, box.maximum.z));
        }

    let add_bounding_boxes (box1:boundingBox) (box2:boundingBox) =
        boundingBoxAddPoint box2.minimum box1 |> boundingBoxAddPoint box2.maximum        

    let box_contains_point (box:boundingBox) (pt:tuple) =
        pt.x >= box.minimum.x && pt.x <= box.maximum.x &&
        pt.y >= box.minimum.y && pt.y <= box.maximum.y &&
        pt.z >= box.minimum.z && pt.z <= box.maximum.z

    let box_contains_box (box1:boundingBox) (box2:boundingBox) =
        box_contains_point box1 box2.minimum &&
        box_contains_point box1 box2.maximum

    let transform_box (box:boundingBox) (m:matrix) =
        let p1 = box.minimum
        let p2 = point box.minimum.x box.minimum.y box.maximum.z
        let p3 = point box.minimum.x box.maximum.y box.minimum.z
        let p4 = point box.minimum.x box.maximum.y box.maximum.z
        let p5 = point box.maximum.x box.minimum.y box.minimum.z
        let p6 = point box.maximum.x box.minimum.y box.minimum.z
        let p7 = point box.maximum.x box.maximum.y box.minimum.z
        let p8 = box.maximum
        let mutable new_box = boundingBox.Default
        for p in [|p1;p2;p3;p4;p5;p6;p7;p8;|] do
            new_box <- boundingBoxAddPoint (m * p) new_box
        new_box

    let rec bounds_of shape =
        match shape with
        | Sphere _ -> { minimum = point -1.0 -1.0 -1.0; maximum = point 1.0 1.0 1.0; }
        | Plane _ -> { minimum = point Double.NegativeInfinity 0.0 Double.NegativeInfinity; maximum = point Double.PositiveInfinity 0.0 Double.PositiveInfinity; }
        | Cube _ -> { minimum = point -1.0 -1.0 -1.0; maximum = point 1.0 1.0 1.0; }
        | Cylinder (_,_,_,min,max,_) -> { minimum = point -1.0 min -1.0; maximum = point 1.0 max 1.0; }
        | Cone (_,_,_,min,max,_) ->
            let a = Math.Abs(min)
            let b = Math.Abs(max)
            let limit = Math.Max(a,b)
            { minimum = point -limit min -limit; maximum = point limit max limit; }
        | Group (_,_,_,children) ->
            let mutable box = boundingBox.Default
            for child in children do
                let cbox = parent_space_bounds_of child
                box <- add_bounding_boxes box cbox
            box

    and parent_space_bounds_of shape = 
        let t = shapeTransformation shape
        transform_box (bounds_of shape) t        

    let intersects (box:boundingBox) ray = 
        let (xtmin, xtmax) = check_axis ray.origin.x ray.direction.x box.minimum.x box.maximum.x
        let (ytmin, ytmax) = check_axis ray.origin.y ray.direction.y box.minimum.y box.maximum.y
        let (ztmin, ztmax) = check_axis ray.origin.z ray.direction.z box.minimum.z box.maximum.z
        let tmin = seq { xtmin; ytmin; ztmin; } |> Seq.max
        let tmax = seq { xtmax; ytmax; ztmax; } |> Seq.min
        if tmin > tmax then 
            false
        else
            true

    let local_normal_at shape pt =
        match shape with
        | Group _ -> raise (Exception("local_normal_at should not be called on a Group shape!"))
        | Sphere _ -> pt - (point 0.0 0.0 0.0)
        | Cube _ -> 
            let ax = Math.Abs(pt.x)
            let ay = Math.Abs(pt.y)
            let maxc = seq { ax; ay; Math.Abs(pt.z); } |> Seq.max
            if maxc = ax then
                vector pt.x 0.0 0.0
            else
                if maxc = ay then
                    vector 0.0 pt.y 0.0
                else
                    vector 0.0 0.0 pt.z
        | Plane _ -> vector 0.0 1.0 0.0
        | Cylinder (_,_,_,min,max,_) -> 
            let dist = pt.x**2.0 + pt.z**2.0
            if dist < 1.0 && pt.y >= (max - epsilon) then
                vector 0.0 1.0 0.0
            else 
                if dist < 1.0 && pt.y <= (min + epsilon) then
                    vector 0.0 -1.0 0.0
                else
                    vector pt.x 0.0 pt.z
        | Cone (_,_,_,min,max,_) -> 
            let dist = pt.x**2.0 + pt.z**2.0
            if dist < 1.0 && pt.y >= (max - epsilon) then
                vector 0.0 1.0 0.0
            else 
                if dist < 1.0 && pt.y <= (min + epsilon) then
                    vector 0.0 -1.0 0.0
                else
                    let mutable y = Math.Sqrt(dist)
                    if pt.y > 0.0 then 
                        y <- -y
                    vector pt.x y pt.z

    let rec local_intersect shape ray =
        let hits_box = intersects (bounds_of shape) ray
        if not hits_box then
            Seq.empty<intersection>
        else
            match shape with
            | Sphere _ ->
                let normal = local_normal_at shape ray.origin
                let a = ray.direction.dotProduct(ray.direction)
                let b = 2.0 * ray.direction.dotProduct(normal)
                let c = normal.dotProduct(normal) - 1.0
                let discriminant = b**2.0 - 4.0 * a * c
                if discriminant < 0.0 then
                    Seq.empty<intersection>
                else 
                    let t1 = (-b - Math.Sqrt(discriminant)) / (2.0 * a)
                    let t2 = (-b + Math.Sqrt(discriminant)) / (2.0 * a)
                    seq {
                        { t = t1; obj = shape; }; 
                        { t = t2; obj = shape; };
                    }
            | Cube _ ->
                let (xtmin, xtmax) = check_axis ray.origin.x ray.direction.x -1.0 1.0
                let (ytmin, ytmax) = check_axis ray.origin.y ray.direction.y -1.0 1.0
                let (ztmin, ztmax) = check_axis ray.origin.z ray.direction.z -1.0 1.0
                let tmin = seq { xtmin; ytmin; ztmin; } |> Seq.max
                let tmax = seq { xtmax; ytmax; ztmax; } |> Seq.min
                if tmin > tmax then
                    Seq.empty<intersection>
                else
                    seq {
                        { t = tmin; obj = shape; };
                        { t = tmax; obj = shape; };
                    }
            | Plane _ -> 
                if Math.Abs(ray.direction.y) < epsilon then
                    Seq.empty<intersection>
                else
                    seq [{ t = -ray.origin.y / ray.direction.y; obj = shape; }]
            | Cylinder (_,_,_,cmin,cmax,closed) ->
                let check_cap t =
                    let x = ray.origin.x + t * ray.direction.x
                    let z = ray.origin.z + t * ray.direction.z
                    (x**2.0 + z**2.0) <= 1.0
                let intersect_caps =
                    if not closed || (areEqualFloat ray.direction.y 0.0) then
                        Seq.empty<intersection>
                    else
                        let calc v =
                            let t = (v - ray.origin.y) / ray.direction.y
                            if check_cap t then
                                Some { t = t; obj = shape;}
                            else
                                None
                        seq { calc cmin; calc cmax; } |> Seq.choose id
                let a = ray.direction.x**2.0 + ray.direction.z**2.0
                if areEqualFloat a 0.0 then
                    intersect_caps
                else
                    let b = 2.0 * ray.origin.x * ray.direction.x + 2.0 * ray.origin.z * ray.direction.z
                    let c = ray.origin.x**2.0 + ray.origin.z**2.0 - 1.0
                    let disc = b**2.0 - 4.0 * a * c
                    if disc < 0.0 then
                        Seq.empty<intersection>
                    else
                        let (t0,t1) = swapIfGreater ((-b - Math.Sqrt(disc)) / (2.0 * a)) ((-b + Math.Sqrt(disc)) / (2.0 * a))
                        let calc t =
                            let y = ray.origin.y + t * ray.direction.y
                            if cmin < y && y < cmax then
                                Some { t = t; obj = shape; }
                            else
                                None
                        let s1 = seq { calc t0; calc t1; } |> Seq.choose id 
                        let s2 = intersect_caps
                        s1 |> Seq.append s2
            | Cone (_,_,_,cmin,cmax,closed) ->
                let check_cap t (y:float) =
                    let x = ray.origin.x + t * ray.direction.x
                    let z = ray.origin.z + t * ray.direction.z
                    (x**2.0 + z**2.0) <= Math.Abs(y)
                let intersect_caps =
                    if not closed || (areEqualFloat ray.direction.y 0.0) then
                        Seq.empty<intersection>
                    else
                        let calc v =
                            let t = (v - ray.origin.y) / ray.direction.y
                            if check_cap t v then
                                Some { t = t; obj = shape;}
                            else
                                None
                        seq { calc cmin; calc cmax; } |> Seq.choose id
                let a = ray.direction.x**2.0 - ray.direction.y**2.0 + ray.direction.z**2.0
                let b = 2.0 * ray.origin.x * ray.direction.x - 2.0 * ray.origin.y * ray.direction.y + 2.0 * ray.origin.z * ray.direction.z
                let c = ray.origin.x**2.0 - ray.origin.y**2.0 + ray.origin.z**2.0
                match (areEqualFloat a 0.0),(areEqualFloat b 0.0) with
                | true, true -> Seq.empty<intersection>
                | true, false -> 
                    let s = seq { { t = -c/(2.0*b); obj = shape; } }
                    intersect_caps |> Seq.append s
                | false, _ ->
                    let disc = b**2.0 - 4.0 * a * c
                    if disc < 0.0 then
                        Seq.empty<intersection>
                    else
                        let (t0,t1) = swapIfGreater ((-b - Math.Sqrt(disc)) / (2.0 * a)) ((-b + Math.Sqrt(disc)) / (2.0 * a))
                        let calc t =
                            let y = ray.origin.y + t * ray.direction.y
                            if cmin < y && y < cmax then
                                Some { t = t; obj = shape; }
                            else
                                None
                        let s1 = seq { calc t0; calc t1; } |> Seq.choose id 
                        let s2 = intersect_caps
                        s1 |> Seq.append s2
            | Group (_,_,_,children) ->
                children
                |> Seq.map (fun (c) -> intersect c ray)
                |> Seq.collect (fun (s) -> s)
                |> Seq.sortBy (fun (i) -> i.t)
            
    and intersect shape ray =
        let st = shapeTransformation shape
        let local_ray = transform ray (inverse st)
        local_intersect shape local_ray

    let hit (intersections:seq<intersection>) : intersection option = 
        let filtered = intersections |> Seq.filter (fun i -> i.t > 0.0)
        if Seq.isEmpty filtered then
            None
        else 
            let lowest = Seq.minBy (fun (i:intersection) -> i.t) filtered
            Some lowest

    let rec world_to_object shape (point:tuple) =
        let mutable pt = point
        match shapeParent shape with
        | Some parent -> pt <- world_to_object parent point
        | None -> pt <- point
        (inverse (shapeTransformation shape)) * pt

    let rec normal_to_world shape (normal:tuple) = 
        let mutable n = (inverse (shapeTransformation shape)).Transpose * normal
        n <- { n with w = 0.0; }
        n <- n.normalize()
        match shapeParent shape with
        | Some parent -> n <- normal_to_world parent n
        | None -> n <- n
        n

    let normal_at shape (world_point:tuple) =
        let local_point = world_to_object shape world_point
        let local_normal = local_normal_at shape local_point
        normal_to_world shape local_normal

    //not happy with this, still an "imperative" algorithm, need to figure out how to do it functionally
    let rec private func (hit:intersection) pos (xs:seq<intersection>) (container:list<shape>) n1 n2 =
        let mutable c = container
        let mutable exit = false
        let i = xs |> Seq.item pos
        let isHit = i = hit
        let n1 = 
            if isHit then
                if Seq.isEmpty container then
                    1.0
                else
                    let sm = shapeMaterial (Seq.last c)
                    sm.refractive_index
            else
                n1
        c <- if c |> List.contains i.obj then
                List.filter (fun (s) -> (s = i.obj) |> not) c
             else
                List.append c [i.obj]
        let n2 = 
            if isHit then
                exit <- true
                if Seq.isEmpty c then
                    1.0
                else
                    let sm = shapeMaterial (Seq.last c)
                    sm.refractive_index
            else
                n2
        if exit |> not && pos < (Seq.length xs) then
            func hit (pos+1) xs c n1 n2
        else
            (n1, n2)

    let prepare_computations (hit:intersection) ray (xs:seq<intersection>) = 
        let p = position ray hit.t
        let zero_point = point 0.0 0.0 0.0
        let comps = {
            t = hit.t;
            obj = hit.obj;
            point = p;
            eyev = -ray.direction;
            normalv = normal_at hit.obj p;
            reflectv = vector 0.0 0.0 0.0;
            inside = false;
            over_point = zero_point;
            under_point = zero_point;
            n1 = 0.0;
            n2 = 0.0;
        }
        let newComps =
            if comps.normalv.dotProduct comps.eyev < 0.0 then
                { comps with inside = true; normalv = -comps.normalv; }
            else
                comps
        let over_point = newComps.point + newComps.normalv * epsilon
        let under_point = newComps.point - newComps.normalv * epsilon
        let reflectv = reflect ray.direction newComps.normalv
        let (n1,n2) = func hit 0 xs List.empty<shape> 1.0 1.0
        { newComps with over_point = over_point; under_point = under_point; reflectv = reflectv; n1 = n1; n2 = n2; }
    
    let schlick comps =
        let f cos comps = 
            let r0 = ((comps.n1 - comps.n2) / (comps.n1 + comps.n2))**2.0
            r0 + (1.0 - r0) * (1.0 - cos)**5.0
        
        let cos = comps.eyev.dotProduct(comps.normalv)
        if comps.n1 > comps.n2 then
            let n = comps.n1 / comps.n2
            let sin2_t = n**2.0 * (1.0 - cos**2.0)            
            if sin2_t > 1.0 then
                1.0
            else
                let cos_t = Math.Sqrt(1.0 - sin2_t)
                f cos_t comps
        else
            f cos comps

    let add_child g c =
        match g with
        | Group (_,_,_,children) -> children.Add c

    let has_child g c =
        match g with
        | Group (_,_,_,children) -> children.Contains c