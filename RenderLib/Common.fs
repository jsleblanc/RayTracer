namespace RenderLib

open System

module Common = 

    [<Literal>] 
    let epsilon = 0.000001

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