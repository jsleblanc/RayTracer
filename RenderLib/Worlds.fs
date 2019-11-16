namespace RenderLib

open System
open Common
open Tuple
open Color
open Matrix
open Ray
open Lights
open PreparedComputations
open Shapes2

module Worlds = 

    type world = {
        light: point_light; //TODO - support multiple lights
        objs: shape list
    }

    let build shapes lights =
        {
            objs = shapes;
            light = lights;
        }

    let build_default shapes =
        {
            objs = shapes;
            light = {
                position = point -10.0 10.0 -10.0;
                intensity = color 1.0 1.0 1.0
            }
        }

    let addShape shape world =
        let shapes = world.objs @ [shape]
        { world with objs = shapes }

    let intersect_world world (ray:ray) =
        let rec aux (xs:intersection list) shapes =
            match shapes with
            | [] -> xs
            | shape :: shapes ->
                let xs_p = Shapes2.intersect shape shapes ray
                match xs_p with
                | [] -> aux xs shapes
                | _ -> aux ((List.concat [xs;xs_p;]) |> List.sortBy (fun (i) -> i.t)) shapes
        in
        aux [] world.objs

    let is_shadowed (w:world) (p:tuple) =
        let v = w.light.position - p
        let distance = v.magnitude()
        let direction = v.normalize()
        let ray = {
            origin = p;
            direction = direction;
        }
        let intersections = intersect_world w ray
        match hit intersections with
        | Some i -> i.t < distance
        | None -> false

    let rec reflected_color world comps remaining =
        if remaining < 1 then
            black
        else
            let sm = PreparedComputations.material comps
            if sm.reflective = 0.0 then
                black
            else
                let reflect_ray = {
                    origin = comps.over_point;
                    direction = comps.reflectv;
                }
                let c = color_at world reflect_ray (remaining - 1)
                c * sm.reflective

    and shade_hit world (comps:comps) remaining = 
        let shadowed = is_shadowed world comps.over_point
        let sm = PreparedComputations.material comps
        let surface = lighting sm comps.shape world.light comps.over_point comps.eyev comps.normalv shadowed
        let reflected = reflected_color world comps remaining
        let refracted = refracted_color world comps remaining
        if sm.reflective > 0.0 && sm.transparency > 0.0 then
            let reflectance = schlick comps
            surface + reflected * reflectance + refracted * (1.0 - reflectance)
        else
            surface + reflected + refracted

    and color_at world ray remaining =
        let i = intersect_world world ray
        match hit i with 
        | Some hit -> 
            let comp = prepare hit ray i
            shade_hit world comp remaining
        | None -> black

    and refracted_color world comps remaining =
        let sm = PreparedComputations.material comps
        if remaining = 0 || sm.transparency = 0.0 then
            black
        else
            let n_ratio = comps.n1 / comps.n2
            let cos_i = comps.eyev.dotProduct(comps.normalv)
            let sin2_t = n_ratio**2.0 * (1.0 - cos_i**2.0)
            if sin2_t > 1.0 then
                black
            else
                let cos_t = Math.Sqrt(1.0 - sin2_t)
                let direction = comps.normalv * (n_ratio * cos_i - cos_t) - comps.eyev * n_ratio
                let refract_ray = {
                    origin = comps.under_point;
                    direction = direction;
                }
                let c = color_at world refract_ray (remaining - 1)
                c * sm.transparency
