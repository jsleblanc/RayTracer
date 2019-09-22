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
        w: float;
    } with
        member this.magnitude() = 
            Math.Sqrt(this.x**2.0 + this.y**2.0 + this.z**2.0 + this.w**2.0)
        member this.normalize() = {
            x = this.x / this.magnitude();
            y = this.y / this.magnitude();
            z = this.z / this.magnitude();
            w = this.w / this.magnitude();
        }
        override this.Equals(other) = 
            match other with 
            | :? tuple as other -> 
                (areEqualFloat this.x other.x) &&
                (areEqualFloat this.y other.y) &&
                (areEqualFloat this.z other.z) &&
                (areEqualFloat this.w other.w)
            | _ -> Object.Equals(this, other)
        override x.GetHashCode() = 0
        static member (+) (a,b) = {
            x = a.x + b.x;
            y = a.y + b.y;
            z = a.z + b.z;
            w = a.w + b.w;
        }
        static member (-) (a,b) = {
            x = a.x - b.x;
            y = a.y - b.y;
            z = a.z - b.z;
            w = a.w - b.w;
        }
        static member (~-) (a) = {
            x = -a.x;
            y = -a.y;
            z = -a.z;
            w = -a.w;
        }
        static member (*) (t,s) = {
            x = t.x * s;
            y = t.y * s;
            z = t.z * s;
            w = t.w * s;
        }
        static member (/) (t,s) = {
            x = t.x / s;
            y = t.y / s;
            z = t.z / s;
            w = t.w / s;
        }

    let point x y z = { x = x; y = y; z = z; w = 1.0; }
    let vector x y z = { x = x; y = y; z = z; w = 0.0; }
