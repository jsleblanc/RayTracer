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

    let lighting m light position eyev normalv =
        color 1.0 1.0 1.0