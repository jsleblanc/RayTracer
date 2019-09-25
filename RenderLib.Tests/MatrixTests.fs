namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Matrix

module MatrixTests =

    [<Fact>]
    let ``matrix equality with identical matrices``() =
        let a = matrix(2)
        a.[0,0] <- 1.0
        a.[1,0] <- 2.0
        a.[0,1] <- 3.0
        a.[1,1] <- 4.0
        let b = matrix(2)
        b.[0,0] <- 1.0
        b.[1,0] <- 2.0
        b.[0,1] <- 3.0
        b.[1,1] <- 4.0
        Assert.True(a.Equals b)
        Assert.True(b.Equals a)

    [<Fact>]
    let ``matrix equality with different matrices``() =
        let m1 = matrix(2)
        m1.[0,0] <- 4.0
        m1.[1,0] <- 3.0
        m1.[0,1] <- 2.0
        m1.[1,1] <- 1.0
        let m2 = matrix(2)
        m2.[0,0] <- 1.0
        m2.[1,0] <- 2.0
        m2.[0,1] <- 3.0
        m2.[1,1] <- 4.0
        Assert.False(m1.Equals m2)
        Assert.False(m2.Equals m1)

    [<Fact>]
    let ``multiplying two matrices``() =
        let a = matrix(4)
        a.[0,0] <- 1.0
        a.[0,1] <- 2.0
        a.[0,2] <- 3.0
        a.[0,3] <- 4.0
        a.[1,0] <- 5.0
        a.[1,1] <- 6.0
        a.[1,2] <- 7.0
        a.[1,3] <- 8.0
        a.[2,0] <- 9.0
        a.[2,1] <- 8.0
        a.[2,2] <- 7.0
        a.[2,3] <- 6.0
        a.[3,0] <- 5.0
        a.[3,1] <- 4.0
        a.[3,2] <- 3.0
        a.[3,3] <- 2.0
        let b = matrix(4)
        b.[0,0] <- -2.0
        b.[0,1] <- 1.0
        b.[0,2] <- 2.0
        b.[0,3] <- 3.0
        b.[1,0] <- 3.0
        b.[1,1] <- 2.0
        b.[1,2] <- 1.0
        b.[1,3] <- -1.0
        b.[2,0] <- 4.0
        b.[2,1] <- 3.0
        b.[2,2] <- 6.0
        b.[2,3] <- 5.0
        b.[3,0] <- 1.0
        b.[3,1] <- 2.0
        b.[3,2] <- 7.0
        b.[3,3] <- 8.0
        let e = matrix(4)
        e.[0,0] <- 20.0
        e.[0,1] <- 22.0
        e.[0,2] <- 50.0
        e.[0,3] <- 48.0
        e.[1,0] <- 44.0
        e.[1,1] <- 54.0
        e.[1,2] <- 114.0
        e.[1,3] <- 108.0
        e.[2,0] <- 40.0
        e.[2,1] <- 58.0
        e.[2,2] <- 110.0
        e.[2,3] <- 102.0
        e.[3,0] <- 16.0
        e.[3,1] <- 26.0
        e.[3,2] <- 46.0
        e.[3,3] <- 42.0
        let r = a * b
        Assert.True(r.Equals e)

