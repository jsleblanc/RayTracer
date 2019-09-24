namespace RenderLib

open System 
open Common

module Matrix =

    type matrix = float[,]

    let matrixEquality (a:matrix) (b:matrix) = 
        Array.compareWith compareFloat a b