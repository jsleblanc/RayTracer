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

    type material = {
        color: color;
        ambient: float;
        diffuse: float;
        specular: float;
        shininess: float;
    } with static member Default = {
            color = color 1.0 1.0 1.0;
            ambient = 0.1;
            diffuse = 0.9;
            specular = 0.9;
            shininess = 200.0;
        }

    let point_light p i = {
        position = p;
        intensity = i;
    }

    let lighting m light point eyev normalv =
        let black = color 0.0 0.0 0.0
        let effective_color = m.color * light.intensity
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
            if reflect_dot_eye <= 0.0 then
                specular <- black
            else 
                let factor = Math.Pow(reflect_dot_eye, m.shininess)
                specular <- light.intensity * m.specular * factor
        ambient + diffuse + specular