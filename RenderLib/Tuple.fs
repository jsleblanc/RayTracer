namespace RenderLib

open System
open Common

module Tuple =

    [<CustomEquality; NoComparison>]
    type tuple = {
        x: float;
        y: float;
        z: float;
        w: float;
    } with
        member a.crossProduct(b) = {
            x = a.y * b.z - a.z * b.y;
            y = a.z * b.x - a.x * b.z;
            z = a.x * b.y - a.y * b.x;
            w = 0.0;
        }            
        member this.dotProduct(v) = 
            this.x * v.x + 
            this.y * v.y + 
            this.z * v.z + 
            this.w * v.w
        member this.magnitude() = 
            Math.Sqrt(this.x**2.0 + this.y**2.0 + this.z**2.0 + this.w**2.0)
        member this.normalize() = 
            let m = this.magnitude()
            {
                x = this.x / m;
                y = this.y / m;
                z = this.z / m;
                w = this.w / m;
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
