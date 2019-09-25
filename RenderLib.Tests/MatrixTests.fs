namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Matrix

module MatrixTests =

    [<Fact>]
    let ``matrix equality``() =
        let m1 = matrix(2)
        m1.[0,0] <- 1.0
        m1.[1,0] <- 2.0
        m1.[0,1] <- 3.0
        m1.[1,1] <- 4.0
        let m2 = matrix(2)
        m2.[0,0] <- 1.0
        m2.[1,0] <- 2.0
        m2.[0,1] <- 3.0
        m2.[1,1] <- 4.0
        Assert.True(m1.Equals m2)
        Assert.True(m2.Equals m1)
