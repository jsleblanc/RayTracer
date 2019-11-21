namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes

module PreparedComputations =

    type comps = {
        t:float;
        shape:shape;
        trail:shape list;
        point:tuple;
        under_point:tuple;
        over_point:tuple;
        eyev:tuple;
        normalv:tuple;
        reflectv:tuple;
        inside:bool;
        n1:float;
        n2:float;
    }

    let prepare (i:intersection) r xs =
        let point = Ray.position r i.t
        let eyev = -r.direction
        let normalv = Shapes.normal_at (Some i) i.obj i.trail point in
        let inside = normalv.dotProduct(eyev) < 0.0
        let normalv_p = if inside then -normalv else normalv
        let reflectv = Tuple.reflect r.direction normalv_p
        let over_point = point + normalv_p * epsilon
        let under_point = point + normalv_p * -epsilon
        let append_or_remove shape containers =
            let rec aux acc remaining found = 
                match remaining with
                | [] ->
                    let result = List.rev acc in
                    if found then result else shape :: result
                | el :: els ->
                    if el = shape then aux acc els true else aux (el :: acc) els found
                in
                aux [] containers false
            in
            let rec n1n2 (containers:shape list) n1 n2 xlist = 
                match xlist with
                | [] -> (n1, n2)
                | x :: xlist ->
                    let n1_p = 
                        if x = i then 
                            match containers with
                            | [] -> 1.
                            | s :: _ -> (Shapes.material i.trail s).refractive_index
                        else n1
                    in
                    let containers_p = 
                        append_or_remove x.obj containers in
                        if x = i then 
                            match containers_p with
                            | [] -> (n1_p, 1.)
                            | s :: _ -> (n1_p, (Shapes.material i.trail s).refractive_index)
                        else n1n2 containers_p n1_p n2 xlist
            let (n1, n2) = n1n2 [] 1. 1. xs in
            {
                t = i.t;
                shape = i.obj;
                trail = i.trail;
                point = point;
                under_point = under_point;
                over_point = over_point;
                eyev = eyev;
                normalv = normalv_p;
                reflectv = reflectv;
                inside = inside;
                n1 = n1;
                n2 = n2;
            }

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

    let material comps = 
        Shapes.material comps.trail comps.shape
    