namespace RenderLib

open System
open Matrix
open Tuple

module Translations =

    type translation_t =
    | Translation of x:float * y:float * z:float
    | Scaling of x:float * y:float * z:float
    | Rotation_X of radians:float
    | Rotation_Y of radians:float
    | Rotation_Z of radians:float
    | Shearing of xy:float * xz:float * yx:float * yz:float * zx:float * zy:float
    | View of from_point:tuple * to_point:tuple * up_direction:tuple

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

    let shearing xy xz yx yz zx zy =
        let t = matrix(4)
        t.[0,0] <- 1.0
        t.[0,1] <- xy
        t.[0,2] <- xz
        t.[1,0] <- yx
        t.[1,1] <- 1.0
        t.[1,2] <- yz
        t.[2,0] <- zx
        t.[2,1] <- zy
        t.[2,2] <- 1.0
        t.[3,3] <- 1.0
        t

    let view_transform (from_point:tuple) (to_point:tuple) (up_direction:tuple) =
        let forward = (to_point - from_point).normalize()
        let upn = up_direction.normalize()
        let left = forward.crossProduct upn
        let true_up = left.crossProduct forward
        let orientation = matrix(4)
        orientation.[0,0] <- left.x;
        orientation.[0,1] <- left.y;
        orientation.[0,2] <- left.z;
        orientation.[0,3] <- 0.0;
        orientation.[1,0] <- true_up.x;
        orientation.[1,1] <- true_up.y;
        orientation.[1,2] <- true_up.z;
        orientation.[1,3] <- 0.0;
        orientation.[2,0] <- -forward.x;
        orientation.[2,1] <- -forward.y;
        orientation.[2,2] <- -forward.z;
        orientation.[2,3] <- 0.0;
        orientation.[3,0] <- 0.0;
        orientation.[3,1] <- 0.0;
        orientation.[3,2] <- 0.0;
        orientation.[3,3] <- 1.0;
        orientation * translation -from_point.x -from_point.y -from_point.z
       
    let combine transformations =
        let func t m =
            let tf = 
                match t with
                | Translation(x,y,z) -> translation x y z
                | Scaling(x,y,z) -> scaling x y z
                | Rotation_X(r) -> rotation_x r
                | Rotation_Y(r) -> rotation_y r
                | Rotation_Z(r) -> rotation_z r
                | Shearing(xy,xz,yx,yz,zx,zy) -> shearing xy xz yx yz zx zy
            m * tf
        List.foldBack func transformations (identity_matrix())