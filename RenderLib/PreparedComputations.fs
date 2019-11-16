namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes2

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
        let normalv = Shapes2.normal_at i i.obj i.trail point in
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
                    let n1' = 
                        if x = i then 
                            match containers with
                            | [] -> 1.
                            | s :: _ -> (Shapes2.material i.trail s).refractive_index
                        else n1
                    in
                    let containers' = 
                        append_or_remove x.obj containers in
                        if x = i then 
                            match containers' with
                            | [] -> (n1', 1.)
                            | s :: _ -> (n1', (Shapes2.material i.trail s).refractive_index)
                        else n1n2 containers' n1' n2 xlist
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

    let material comps = 
        Shapes2.material comps.trail comps.shape
    