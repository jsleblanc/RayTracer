namespace RenderLib

open System 
open System.Collections.Generic
open Common
open Tuple

module Matrix =

    type matrix(size:int) =
        let m:float[,] = Array2D.zeroCreate size size
        member _.Size = size
        member _.Item 
            with get(row,col) = m.[col,row] 
            and set (row,col) value = m.[col,row] <- value
        member this.Transpose =
            let t = matrix(this.Size)
            for col in 0 .. m.GetLength(0) - 1 do
                for row in 0 .. m.GetLength(1) - 1 do
                    t.[col,row] <- this.[row,col]
            t
        override _.GetHashCode() = 0
        override this.Equals(other) =
            match other with
            | :? matrix as other -> 
                seq { 
                    for col in 0 .. m.GetLength(0) - 1 do 
                        for row in 0 .. m.GetLength(1) - 1 do
                            if not (areEqualFloat m.[col,row] (other.[row,col])) then 
                                yield false
                }
                |> Seq.forall id
            | _ -> Object.Equals(this, other)
        static member (*) (a:matrix,b:matrix) =
            assert (a.Size = b.Size)
            let length = a.Size - 1
            let calculateCell row col =
                let mutable v = 0.0
                for x in 0 .. length do
                    v <- v + (a.[row, x] * b.[x, col])
                v
            let r = matrix(a.Size)
            for row in 0 .. length do
                for col in 0 .. length do
                    r.[row,col] <- calculateCell row col
            r
        static member (*) (a:matrix,b:tuple) =
            assert (a.Size = 4)
            let ta = [| b.x; b.y; b.z; b.w; |]
            let length = a.Size - 1
            let calculateCell row =
                let mutable v = 0.0
                for x in 0 .. length do
                    v <- v + (a.[row, x] * ta.[x])
                v
            {
                x = calculateCell 0;
                y = calculateCell 1;
                z = calculateCell 2;
                w = calculateCell 3;
            }

    let identity_matrix = 
        let a = matrix(4)
        a.[0,0] <- 1.0
        a.[0,1] <- 0.0
        a.[0,2] <- 0.0
        a.[0,3] <- 0.0
        a.[1,0] <- 0.0
        a.[1,1] <- 1.0
        a.[1,2] <- 0.0
        a.[1,3] <- 0.0
        a.[2,0] <- 0.0
        a.[2,1] <- 0.0
        a.[2,2] <- 1.0
        a.[2,3] <- 0.0
        a.[3,0] <- 0.0
        a.[3,1] <- 0.0
        a.[3,2] <- 0.0
        a.[3,3] <- 1.0
        a

    let determinant (a:matrix) = 
        assert (a.Size = 2)
        (a.[0,0] * a.[1,1]) - (a.[0,1] * a.[1,0])

    let submatrix (a:matrix) (row:int) (col:int) =
        let newSize = a.Size - 1
        let s = new Queue<float>(newSize * newSize)
        let sm = matrix(newSize)
        for r in 0 .. newSize do
            for c in 0 ..newSize do
                if not (r = row) && not (c = col) then
                    s.Enqueue(a.[r,c])
        for r in 0 .. sm.Size - 1 do
            for c in 0 ..sm.Size - 1 do
                let v = s.Dequeue()
                sm.[r,c] <- v
        sm

    let minor (a:matrix) (row:int) (col:int) = 
        let s = submatrix a row col
        determinant s

    let cofactor (a:matrix) (row:int) (col:int) = 
        let m = minor a row col
        if isOdd(row + col) then
            m * -1.0
        else m