namespace RenderLib

open System
open Tuple
open Color
open Patterns

module Material =

    type material = {
        color: color;
        ambient: float;
        diffuse: float;
        specular: float;
        shininess: float;
        reflective: float;
        transparency: float;
        refractive_index: float;
        pattern: pattern_t option;
    } with static member Default = {
            color = white;
            ambient = 0.1;
            diffuse = 0.9;
            specular = 0.9;
            shininess = 200.0;
            reflective = 0.0;
            transparency = 0.0;
            refractive_index = 1.0;
            pattern = None;
        }

    let glass = 
        { material.Default with transparency = 1.0; refractive_index = 1.5; }

    let colored color =
        { material.Default with color = color }