namespace RenderLib

open System
open Matrix

module Translations =

    let translation x y z =
        let i = identity_matrix ()
        i.[0,3] <- x
        i.[1,3] <- y
        i.[2,3] <- z
        i

    let scaling x y z = 
        let i = identity_matrix ()
        i.[0,0] <- x
        i.[1,1] <- y
        i.[2,2] <- z
        i

    [<Measure>]
    type radians

    let rotation_x (r:float) =
        let t = matrix(4)
        t.[0,0] <- 1.0
        t.[1,1] <- Math.Cos r
        t.[1,2] <- -(Math.Sin r)
        t.[2,1] <- Math.Sin r
        t.[2,2] <- Math.Cos r
        t.[3,3] <- 1.0
        t