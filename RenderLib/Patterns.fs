namespace RenderLib

open System
open Common
open Tuple
open Color
open Matrix

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

    let solid_color c =
        let fn point = Solid(c)
        build fn