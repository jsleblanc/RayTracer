namespace RenderLib

open System
open Common
open Tuple

module Ray = 
    
    type ray = {
        origin: tuple;
        direction: tuple;
    }

    let position r t =
        r.origin + r.direction * t