namespace RenderLib

open System 
open System.Text
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
        member this.PrintAssignment(name) =
            let size = this.Size - 1
            let sb = new StringBuilder()
            for row in 0 .. size do
                for col in 0 .. size do
                    let s = sprintf "%s.[%d,%d] <- %.18f" name row col this.[row,col]
                    sb.AppendLine s |> ignore
            sb.ToString()
        override _.GetHashCode() = 0
        override this.Equals(other) =
            match other with
            | :? matrix as other -> 
                seq { 
                    for col in 0 .. m.GetLength(0) - 1 do 
                        for row in 0 .. m.GetLength(1) - 1 do
                            let a = round m.[col,row]
                            let b = round other.[row,col]
                            if not (areEqualFloat a b) then 
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
        static member identity_matrix =
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

    let identity_matrix () = 
        matrix.identity_matrix

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

    let rec determinant (a:matrix) = 
        if a.Size = 2 then
            (a.[0,0] * a.[1,1]) - (a.[0,1] * a.[1,0])
        else 
            let mutable d = 0.0
            for col in 0 .. a.Size - 1 do
                d <- d + a.[0,col] * cofactor a 0 col
            d

    and cofactor (a:matrix) (row:int) (col:int) = 
        let m = minor a row col
        if isOdd(row + col) then
            m * -1.0
        else m

    and minor (a:matrix) (row:int) (col:int) = 
        let s = submatrix a row col
        determinant s

    let invertible (a:matrix) = 
        let d = determinant a
        areEqualFloat d 0.0 |> not

    let inverse (a:matrix) =        
        if a = matrix.identity_matrix then 
            matrix.identity_matrix
        else
            let i = invertible a
            if i then        
                let size = a.Size - 1
                let da = determinant a
                let r = matrix(a.Size)
                for row in 0 .. size do
                    for col in 0 .. size do
                        let c = cofactor a row col
                        r.[col,row] <- c/da
                r
            else raise (Exception("Non-invertible matrix"))