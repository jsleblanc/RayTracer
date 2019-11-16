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

    let prepare i r xs =
        let point = Ray.position r i.t
        let eyev = -r.direction
        let normalv = vector 0.0 0.0 0.0
        let inside = normalv.dotProduct(eyev) < 0.0
        let normalv_p = if inside then -normalv else normalv
        let reflectv = Tuple.reflect r.direction normalv_p
        let over_point = point + normalv_p * epsilon
        let under_point = point + normalv_p * -epsilon


    let material comps = 
        Shapes2.material comps.trail comps.shape
    