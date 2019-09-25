﻿namespace RenderLib

open System 
open Common

module Color =

    [<CustomEquality; NoComparison>]
    type color = {
        red: float;
        green: float;
        blue: float;
    } with
        override this.Equals(other) = 
            match other with 
            | :? color as other -> 
                (areEqualFloat this.red other.red) &&
                (areEqualFloat this.green other.green) &&
                (areEqualFloat this.blue other.blue)
            | _ -> Object.Equals(this, other)
        override x.GetHashCode() = 0
        static member (+) (a,b) = {
            red = a.red + b.red;
            green = a.green + b.green;
            blue = a.blue + b.blue;
        }
        static member (-) (a,b) = {
            red = a.red - b.red;
            green = a.green - b.green;
            blue = a.blue - b.blue;
        }
        static member (*) (c,s) = {
            red = c.red * s;
            green = c.green * s;
            blue = c.blue * s;
        }
        static member (*) (cl,cr) = {
            red = cl.red * cr.red;
            green = cl.green * cr.green;
            blue = cl.blue * cr.blue;
        }

    let color r g b = { red = r; green = g; blue = b; }
    let color_int c = 
        ((c.red * 255.0) |> int, 
         (c.green * 255.0) |> int, 
         (c.blue * 255.0) |> int)
    