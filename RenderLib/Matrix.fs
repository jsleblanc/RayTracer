namespace RenderLib

open System 
open Common

module Matrix =

    type matrix(size:int) =
        let m:float[,] = Array2D.zeroCreate size size
        member _.Size = size
        member _.Item 
            with get(x,y) = m.[x,y] 
            and set (x,y) value = m.[x,y] <- value
        override _.GetHashCode() = 0
        override this.Equals(other) =
            match other with
            | :? matrix as other -> 
                seq { 
                    for x in 0 .. m.GetLength(0) - 1 do 
                        for y in 0 .. m.GetLength(1) - 1 do
                            if not (areEqualFloat m.[x,y] (other.[x,y])) then 
                                yield false
                }
                |> Seq.forall id
            | _ -> Object.Equals(this, other)
