namespace RenderLib

open System
open Tuple
open Color
open Matrix
open Translations

module Patterns =

    type pattern = 
    | Stripe of transform:matrix * a:color * b:color
    | Gradient of transform:matrix * a:color * b:color
    | Ring of transform:matrix * a:color * b:color
    | Checker of transform:matrix * a:color * b:color
    | Blended of transform:matrix * a:pattern * b:pattern

    let patternTransform pattern =
        match pattern with
        | Stripe (t,_,_) -> t
        | Gradient (t,_,_) -> t
        | Ring (t,_,_) -> t
        | Checker (t,_,_) -> t
        | Blended (t,_,_) -> t
        
    let stripe_pattern transform a b =
        Stripe(transform, a, b)

    let stripe_pattern_default a b =
        stripe_pattern (identity_matrix()) a b

    let private stripe_at a b pt =
        if Math.Floor(pt.x) % 2.0 = 0.0 then
            a
        else 
            b

    //DOESN'T WORK
    let blendedStripesAtRightAngle t a b =
        let s1 = stripe_pattern (t * rotation_y(Math.PI/2.0)) a b
        let s2 = stripe_pattern t a b
        Blended(identity_matrix (),s1,s2)

    let rec pattern_at pattern point =
        match pattern with
        | Stripe (t,a,b) -> stripe_at a b point
        | Blended (t,a,b) -> 
            //DOESN't WORK YET
            let ca = pattern_at a point
            let cb = pattern_at b point
            (ca + cb) / 2.0