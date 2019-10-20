namespace RenderLib

open System
open Common
open Tuple
open Color

module Lights =

    type point_light = {
        position: tuple;
        intensity: color;
    }

    type pattern = {
        a: color;
        b: color;
    }

    type material = {
        color: color;
        ambient: float;
        diffuse: float;
        specular: float;
        shininess: float;
        pattern: pattern option;
    } with static member Default = {
            color = white;
            ambient = 0.1;
            diffuse = 0.9;
            specular = 0.9;
            shininess = 200.0;
            pattern = None;
        }

    let point_light p i = {
        position = p;
        intensity = i;
    }

    let stripe_pattern a b =
        {
            a = a;
            b = b;
        }

    let stripe_at pattern pt =
        if Math.Floor(pt.x) % 2.0 = 0.0 then
            pattern.a
        else 
            pattern.b

    let lighting m light point eyev normalv inShadow =
        let c = match m.pattern with
                | Some p -> stripe_at p point
                | None -> m.color
        let effective_color = c * light.intensity
        let lightv = (light.position - point).normalize()
        let ambient = effective_color * m.ambient
        let light_dot_normal = lightv.dotProduct(normalv)
        let mutable diffuse = black
        let mutable specular = black
        if light_dot_normal < 0.0 then
            diffuse <- black
            specular <- black
        else 
            diffuse <- effective_color * m.diffuse * light_dot_normal
            let reflectv = reflect -lightv normalv
            let reflect_dot_eye = reflectv.dotProduct(eyev)
            if reflect_dot_eye < 0.0 || areEqualFloat reflect_dot_eye 0.0 then
                specular <- black
            else 
                let factor = Math.Pow(reflect_dot_eye, m.shininess)
                specular <- light.intensity * m.specular * factor
        if inShadow then
            ambient
        else
            ambient + diffuse + specular
