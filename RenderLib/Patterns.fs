namespace RenderLib

open System
open Tuple
open Color
open Material

module Patterns =

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
