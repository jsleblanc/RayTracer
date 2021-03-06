﻿namespace RenderLib

open System
open System.Numerics

module Common = 

    [<Literal>] 
    let epsilon = 1.0e-8

    let private floatEquality (a:float) (b:float) (e:float) =
        let MinNormal = 2.2250738585072014E-308
        let absA = Math.Abs(a)
        let absB = Math.Abs(b)
        let diff = Math.Abs(a - b)
        if a.Equals(b) then 
            true
        else if a = 0.0 || b = 0.0 || absA + absB < MinNormal then
            diff < (e * MinNormal)
            else
                diff / (absA + absB) < e

    let areEqualFloat x y = floatEquality x y epsilon

    let compareFloat (a:float) (b:float) = 
        if areEqualFloat a b then 0
        else if a < b then -1 else 1    

    let areEqualComplex (a:Complex) (b:Complex) = 
        Math.Abs(a.Real - b.Real) < epsilon &&
        Math.Abs(a.Imaginary - b.Imaginary) < epsilon

    let isEven x = (x % 2) = 0

    let isOdd x = isEven x = false

    let round (x:float) =
        Math.Round(x, 12, MidpointRounding.AwayFromZero)

    let swapIfGreater min max =
        if min > max then
            (max, min)
        else
            (min, max)

    let degrees a =
        a * (Math.PI/180.0)