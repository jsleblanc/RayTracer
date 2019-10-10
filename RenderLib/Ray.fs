namespace RenderLib

open System
open Common
open Tuple
open Translations
open Matrix

module Ray = 
    
    type ray = {
        origin: tuple;
        direction: tuple;
    }

    let position r t =
        r.origin + r.direction * t

    let transform r (m:matrix) =
        { origin = m * r.origin; direction = m * r.direction; }
