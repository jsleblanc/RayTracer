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

    let rotation_x (radians:float) =
        let t = matrix(4)
        t.[0,0] <- 1.0
        t.[1,1] <- Math.Cos radians
        t.[1,2] <- -(Math.Sin radians)
        t.[2,1] <- Math.Sin radians
        t.[2,2] <- Math.Cos radians
        t.[3,3] <- 1.0
        t

    let rotation_y (radians:float) =
        let t = matrix(4)
        t.[0,0] <- Math.Cos radians
        t.[0,2] <- Math.Sin radians
        t.[1,1] <- 1.0
        t.[2,0] <- -(Math.Sin radians)
        t.[2,2] <- Math.Cos radians
        t.[3,3] <- 1.0
        t

    let rotation_z (radians:float) =
        let t = matrix(4)
        t.[0,0] <- Math.Cos radians
        t.[0,1] <- -(Math.Sin radians)
        t.[1,0] <- Math.Sin radians
        t.[1,1] <- Math.Cos radians
        t.[2,2] <- 1.0
        t.[3,3] <- 1.0
        t