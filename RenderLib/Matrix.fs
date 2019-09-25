namespace RenderLib

open System 
open Common

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
        static member (*) (a,b) = matrix(4)
