namespace RenderLib

open System
open Common
open Tuple
open Color
open Matrix
open Translations

module Patterns =

    [<CustomEquality; NoComparison>]
    type pattern_t = {
        fn: tuple -> texture_t;
        transform: matrix;
        inverse_transform: matrix;
    } with
        override this.Equals(other) =
            match other with
            | :? pattern_t as other ->
                this.transform = other.transform &&
                this.inverse_transform = other.inverse_transform
            | _ -> Object.Equals(this,other)
        override this.GetHashCode() = 0
    and texture_t = 
        | Solid of color
        | Pattern of pattern_t

    let build fn = { fn = fn; transform = identity_matrix(); inverse_transform = identity_matrix(); }

    let transform transform pattern =
        { pattern with transform = transform; inverse_transform = inverse transform; }

    let rec eval texture opoint =
        match texture with
        | Solid color -> color
        | Pattern pattern ->
            let ppoint = pattern.inverse_transform * opoint
            eval (pattern.fn ppoint) ppoint

    let at_object pattern (transform:tuple -> tuple) point =
        let opoint = transform point
        eval (Pattern pattern) opoint

    let solid r g b = Solid (color r g b)
    let solid_c color = Solid (color)

    let stripe a b =
        let fn point =
            let disc = areEqualFloat (Math.Floor(point.x) % 2.0) 0.0
            if disc then a else b
        build fn

    let gradient a b =
        let fn point =
            let ac = eval a point
            let bc = eval b point
            let distance = bc - ac
            let fraction = point.x - Math.Floor(point.x)
            Solid (ac + distance * fraction)
        build fn

    let ring a b =
        let fn point =
            let distance = Math.Floor(Math.Sqrt(point.x ** 2.0 + point.z ** 2.0))
            let disc = int distance % 2
            if disc = 0 then a else b
        build fn

    let checkers a b = 
        let fn point = 
            let distance = Math.Floor(point.x) + Math.Floor(point.y) + Math.Floor(point.z)
            let disc = int distance % 2
            if disc = 0 then a else b
        build fn

    type pattern = 
    | Test
    | Solid of a:color
    | Stripe of transform:matrix * a:color * b:color
    | Gradient of transform:matrix * a:color * b:color
    | Ring of transform:matrix * a:color * b:color
    | Checkers of transform:matrix * a:color * b:color
    | Blended of a:pattern * b:pattern

    let rec patternTransform pattern =
        match pattern with
        | Test -> identity_matrix()
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

    let private blended_at (a:color) (b:color) =
        (a + b) / 2.0 //average

    let rec pattern_at pattern object_point =
        let pattern_transform = patternTransform pattern
        let pattern_point = inverse pattern_transform * object_point
        match pattern with
        | Test -> color pattern_point.x pattern_point.y pattern_point.z
        | Solid (a) -> a
        | Stripe (_,a,b) -> stripe_at a b pattern_point
        | Gradient (_,a,b) -> gradient_at a b pattern_point
        | Ring (_,a,b) -> ring_at a b pattern_point
        | Checkers (_,a,b) -> checkers_at a b pattern_point
        | Blended (a,b) -> 
            let ca = pattern_at a pattern_point
            let cb = pattern_at b pattern_point
            blended_at ca cb