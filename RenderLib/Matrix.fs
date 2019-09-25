namespace RenderLib

open System 
open Common
open Tuple

module Matrix =

    type matrix(size:int) =
        let m:float[,] = Array2D.zeroCreate size size
        member _.Size = size
        member _.Item 
            with get(row,col) = m.[col,row] 
            and set (row,col) value = m.[col,row] <- value
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