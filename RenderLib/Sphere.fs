namespace RenderLib

open System
open Common
open Tuple
open Ray

module Sphere = 

    type sphere(id : Guid) = 
        member this.id = id
        new() = sphere(Guid.NewGuid())

    let intersect s r =
        [|1.0;2.0;3.0|]