namespace RenderLib

open System
open Common
open Tuple
open Color
open Material

module Lights =

    type point_light = {
        position: tuple;
        intensity: color;
    }

    let point_light p i = {
        position = p;
        intensity = i;
    }

    let lighting m transform light point eyev normalv inShadow =
        let c = match m.pattern with
                | Some pattern -> Patterns.at_object pattern transform point
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
