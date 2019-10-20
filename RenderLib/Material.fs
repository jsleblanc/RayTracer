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
        pattern: pattern option;
    } with static member Default = {
            color = white;
            ambient = 0.1;
            diffuse = 0.9;
            specular = 0.9;
            shininess = 200.0;
            reflective = 0.0;
            pattern = None;
        }

