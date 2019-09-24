namespace RenderLib

open System

module Common = 

    [<Literal>] 
    let epsilon = 0.000001

    let areEqualFloat (x:float) (y:float) =
        (Double.IsNaN(x) && Double.IsNaN(y)) ||
        (Double.IsInfinity(x) && Double.IsInfinity(y)) ||
        (Math.Abs(x - y) <= epsilon)
