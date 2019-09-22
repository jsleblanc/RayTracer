namespace RenderLib

open System

module Engine =

    [<Literal>] 
    let epsilon = 0.000001

    let areEqualFloat (x:float) (y:float) =        
        (Double.IsNaN(x) && Double.IsNaN(y)) ||
        (Double.IsInfinity(x) && Double.IsInfinity(y)) ||
        (Math.Abs(x - y) <= epsilon)

    [<CustomEquality; NoComparison>]
    type tuple = {
        x: float;
        y: float;
        z: float;
        w: int;
    } with
        override this.Equals(other) = 
            match other with 
            | :? tuple as other -> 
                (areEqualFloat this.x other.x) &&
                (areEqualFloat this.y other.y) &&
                (areEqualFloat this.z other.z) &&
                (this.w = other.w)
            | _ -> Object.Equals(this, other)
        override x.GetHashCode() = 0

    let point x y z = { x = x; y = y; z = z; w = 1; }
    let vector x y z = { x = x; y = y; z = z; w = 0; }
