namespace RenderLib

open System
open Common
open Tuple
open Color
open Matrix
open Translations

module Patterns =

    type pattern = 
    | Solid of a:color
    | Stripe of transform:matrix * a:color * b:color
    | Gradient of transform:matrix * a:color * b:color
    | Ring of transform:matrix * a:color * b:color
    | Checkers of transform:matrix * a:color * b:color
    | Blended of a:pattern * b:pattern

    let rec patternTransform pattern =
        match pattern with
        | Solid (_) -> identity_matrix()
        | Stripe (t,_,_) -> t
        | Gradient (t,_,_) -> t
        | Ring (t,_,_) -> t
        | Checkers (t,_,_) -> t
        | Blended (a,b) -> 
            let at = patternTransform a
            let bt = patternTransform b
            at * bt
        
    let stripe_pattern transform a b =
        Stripe(transform, a, b)

    let stripe_pattern_default a b =
        stripe_pattern (identity_matrix()) a b

    let gradient_pattern transform a b =
        Gradient(transform, a, b)

    let gradient_pattern_default a b =
        gradient_pattern (identity_matrix()) a b

    let ring_pattern transform a b =
        Ring(transform, a, b)

    let ring_pattern_default a b =
        ring_pattern (identity_matrix()) a b

    let checkers_pattern transform a b =
        Checkers(transform, a, b)

    let checkers_pattern_default a b =
        checkers_pattern (identity_matrix()) a b

    let private stripe_at a b pt =
        if areEqualFloat (Math.Floor(pt.x) % 2.0) 0.0 then
            a
        else 
            b

    let private gradient_at (a:color) (b:color) pt =
        let distance = b - a
        let fraction = pt.x - Math.Floor(pt.x)
        a + distance * fraction

    let private ring_at (a:color) (b:color) pt =
        if areEqualFloat (Math.Floor(Math.Sqrt(pt.x**2.0 + pt.z**2.0)) % 2.0) 0.0 then
            a
        else
            b

    let private checkers_at (a:color) (b:color) pt =
        if (Math.Floor(pt.x) + Math.Floor(pt.y) + Math.Floor(pt.z)) % 2.0 = 0.0 then
            a
        else
            b

    //DOESN'T WORK
    let blendedStripesAtRightAngle a b =
        let s1 = stripe_pattern ((scaling 0.25 0.25 0.25) * rotation_y(Math.PI/2.0)) a b
        let s2 = stripe_pattern ((scaling 0.25 0.25 0.25) * rotation_y(Math.PI/4.0)) a b
        Blended(s1,s2)

    let rec pattern_at pattern point =
        match pattern with
        | Solid (a) -> a
        | Stripe (_,a,b) -> stripe_at a b point
        | Gradient (_,a,b) -> gradient_at a b point
        | Ring (_,a,b) -> ring_at a b point
        | Checkers (_,a,b) -> checkers_at a b point
        | Blended (a,b) -> 
            //DOESN't WORK YET
            let ca = pattern_at a point
            let cb = pattern_at b point
            (ca + cb) / 2.0