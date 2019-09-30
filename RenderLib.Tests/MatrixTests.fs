namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Common
open RenderLib.Tuple
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

    [<Fact>]
    let ``matrix multiplied by a tuple``() =
        let a = matrix(4)
        a.[0,0] <- 1.0
        a.[0,1] <- 2.0
        a.[0,2] <- 3.0
        a.[0,3] <- 4.0
        a.[1,0] <- 2.0
        a.[1,1] <- 4.0
        a.[1,2] <- 4.0
        a.[1,3] <- 2.0
        a.[2,0] <- 8.0
        a.[2,1] <- 6.0
        a.[2,2] <- 4.0
        a.[2,3] <- 1.0
        a.[3,0] <- 0.0
        a.[3,1] <- 0.0
        a.[3,2] <- 0.0
        a.[3,3] <- 1.0
        let t = { x = 1.0; y = 2.0; z = 3.0; w = 1.0; }
        let e = { x = 18.0; y = 24.0; z = 33.0; w = 1.0; }
        let r = a * t
        Assert.True(r.Equals e)
        
    [<Fact>]
    let ``multiplying a matrix by the identity matrix``() =
        let a = matrix(4)
        a.[0,0] <- 0.0
        a.[0,1] <- 1.0
        a.[0,2] <- 2.0
        a.[0,3] <- 4.0
        a.[1,0] <- 1.0
        a.[1,1] <- 2.0
        a.[1,2] <- 4.0
        a.[1,3] <- 8.0
        a.[2,0] <- 2.0
        a.[2,1] <- 4.0
        a.[2,2] <- 8.0
        a.[2,3] <- 16.0
        a.[3,0] <- 4.0
        a.[3,1] <- 8.0
        a.[3,2] <- 6.0
        a.[3,3] <- 32.0
        let i = identity_matrix
        let r = a * i
        Assert.True(r.Equals a)
        
    [<Fact>]
    let ``transposing a matrix``() =
        let a = matrix(4)
        a.[0,0] <- 0.0
        a.[0,1] <- 9.0
        a.[0,2] <- 3.0
        a.[0,3] <- 0.0
        a.[1,0] <- 9.0
        a.[1,1] <- 8.0
        a.[1,2] <- 0.0
        a.[1,3] <- 8.0
        a.[2,0] <- 1.0
        a.[2,1] <- 8.0
        a.[2,2] <- 5.0
        a.[2,3] <- 3.0
        a.[3,0] <- 0.0
        a.[3,1] <- 0.0
        a.[3,2] <- 5.0
        a.[3,3] <- 8.0
        let e = matrix(4)
        e.[0,0] <- 0.0
        e.[0,1] <- 9.0
        e.[0,2] <- 1.0
        e.[0,3] <- 0.0
        e.[1,0] <- 9.0
        e.[1,1] <- 8.0
        e.[1,2] <- 8.0
        e.[1,3] <- 0.0
        e.[2,0] <- 3.0
        e.[2,1] <- 0.0
        e.[2,2] <- 5.0
        e.[2,3] <- 5.0
        e.[3,0] <- 0.0
        e.[3,1] <- 8.0
        e.[3,2] <- 3.0
        e.[3,3] <- 8.0
        let r = a.Transpose
        Assert.True(r.Equals e)

    [<Fact>]
    let ``transposing the identity matrix``() =
        let i = identity_matrix
        let r = i.Transpose
        Assert.True(i.Equals r)

    [<Fact>]
    let ``calculating the determinant of a 2x2 matrix``() =
        let a = matrix(2)
        a.[0,0] <- 1.0
        a.[0,1] <- 5.0
        a.[1,0] <- -3.0
        a.[1,1] <- 2.0
        let r = determinant a
        Assert.True(areEqualFloat r 17.0)

    [<Fact>]
    let ``a submatrix of a 3x3 matrix is a 2x2 matrix``() =
        let a = matrix(3)
        a.[0,0] <- 1.0
        a.[0,1] <- 5.0
        a.[0,2] <- 0.0
        a.[1,0] <- -3.0
        a.[1,1] <- 2.0
        a.[1,2] <- 7.0
        a.[2,0] <- 0.0
        a.[2,1] <- 6.0
        a.[2,2] <- -3.0
        let e = matrix(2)
        e.[0,0] <- -3.0
        e.[0,1] <- 2.0
        e.[1,0] <- 0.0
        e.[1,1] <- 6.0
        let r = submatrix a 0 2
        Assert.True(r.Equals e)

    [<Fact>]
    let ``a submatrix of a 4x4 matrix is a 3x3 matrix``() =
        let a = matrix(4)
        a.[0,0] <- -6.0
        a.[0,1] <- 1.0
        a.[0,2] <- 1.0
        a.[0,3] <- 6.0
        a.[1,0] <- -8.0
        a.[1,1] <- 5.0
        a.[1,2] <- 8.0
        a.[1,3] <- 6.0
        a.[2,0] <- -1.0
        a.[2,1] <- 0.0
        a.[2,2] <- 8.0
        a.[2,3] <- 2.0
        a.[3,0] <- -7.0
        a.[3,1] <- 1.0
        a.[3,2] <- -1.0
        a.[3,3] <- 1.0
        let e = matrix(3)
        e.[0,0] <- -6.0
        e.[0,1] <- 1.0
        e.[0,2] <- 6.0
        e.[1,0] <- -8.0
        e.[1,1] <- 8.0
        e.[1,2] <- 6.0
        e.[2,0] <- -7.0
        e.[2,1] <- -1.0
        e.[2,2] <- 1.0
        let r = submatrix a 2 1
        Assert.True(r.Equals e)

    [<Fact>]
    let ``Calculating a minor of a 3x3 matrix``() =
        let a = matrix(3)
        a.[0,0] <- 3.0
        a.[0,1] <- 5.0
        a.[0,2] <- 0.0
        a.[1,0] <- 2.0
        a.[1,1] <- -1.0
        a.[1,2] <- -7.0
        a.[2,0] <- 6.0
        a.[2,1] <- -1.0
        a.[2,2] <- 5.0
        let b = submatrix a 1 0
        let d = determinant b
        let m = minor a 1 0
        Assert.True(areEqualFloat m d)

    [<Fact>]
    let ``Calculating a cofactor of a 3x3 matrix``() =
        let a = matrix(3)
        a.[0,0] <- 3.0
        a.[0,1] <- 5.0
        a.[0,2] <- 0.0
        a.[1,0] <- 2.0
        a.[1,1] <- -1.0
        a.[1,2] <- -7.0
        a.[2,0] <- 6.0
        a.[2,1] <- -1.0
        a.[2,2] <- 5.0
        let c = cofactor a 0 0
        Assert.True(areEqualFloat c -12.0)
        let c = cofactor a 1 0
        Assert.True(areEqualFloat c -25.0)

    [<Fact>]
    let ``Calculating the determinant of a 3x3 matrix``() =
        let a = matrix(3)
        a.[0,0] <- 1.0
        a.[0,1] <- 2.0
        a.[0,2] <- 6.0
        a.[1,0] <- -5.0
        a.[1,1] <- 8.0
        a.[1,2] <- -4.0
        a.[2,0] <- 2.0
        a.[2,1] <- 6.0
        a.[2,2] <- 4.0
        let c = cofactor a 0 0
        Assert.True(areEqualFloat c 56.0)
        let c = cofactor a 0 1
        Assert.True(areEqualFloat c 12.0)
        let c = cofactor a 0 2
        Assert.True(areEqualFloat c -46.0)
        let d = determinant a
        Assert.True(areEqualFloat d -196.0)

    [<Fact>]
    let ``Calculating the determinant of a 4x4 matrix``() =
        let a = matrix(4)
        a.[0,0] <- -2.0
        a.[0,1] <- -8.0
        a.[0,2] <- 3.0
        a.[0,3] <- 5.0
        a.[1,0] <- -3.0
        a.[1,1] <- 1.0
        a.[1,2] <- 7.0
        a.[1,3] <- 3.0
        a.[2,0] <- 1.0
        a.[2,1] <- 2.0
        a.[2,2] <- -9.0
        a.[2,3] <- 6.0
        a.[3,0] <- -6.0
        a.[3,1] <- 7.0
        a.[3,2] <- 7.0
        a.[3,3] <- -9.0
        let c = cofactor a 0 0
        Assert.True(areEqualFloat c 690.0)
        let c = cofactor a 0 1
        Assert.True(areEqualFloat c 447.0)
        let c = cofactor a 0 2
        Assert.True(areEqualFloat c 210.0)
        let c = cofactor a 0 3
        Assert.True(areEqualFloat c 51.0)
        let d = determinant a
        Assert.True(areEqualFloat d -4071.0)

    [<Fact>]
    let ``Testing an invertible matrix for invertibility``() =
        let a = matrix(4)
        a.[0,0] <- 6.0
        a.[0,1] <- 4.0
        a.[0,2] <- 4.0
        a.[0,3] <- 4.0
        a.[1,0] <- 5.0
        a.[1,1] <- 5.0
        a.[1,2] <- 7.0
        a.[1,3] <- 6.0
        a.[2,0] <- 4.0
        a.[2,1] <- -9.0
        a.[2,2] <- 3.0
        a.[2,3] <- -7.0
        a.[3,0] <- 9.0
        a.[3,1] <- 1.0
        a.[3,2] <- 7.0
        a.[3,3] <- -6.0
        let i = invertible a
        Assert.Equal(true, i)

    [<Fact>]
    let ``Testing a noninvertible matrix for invertibility``() =
        let a = matrix(4)
        a.[0,0] <- -4.0
        a.[0,1] <- 2.0
        a.[0,2] <- -2.0
        a.[0,3] <- -3.0
        a.[1,0] <- 9.0
        a.[1,1] <- 6.0
        a.[1,2] <- 2.0
        a.[1,3] <- 6.0
        a.[2,0] <- 0.0
        a.[2,1] <- -5.0
        a.[2,2] <- 1.0
        a.[2,3] <- -5.0
        a.[3,0] <- 0.0
        a.[3,1] <- 0.0
        a.[3,2] <- 0.0
        a.[3,3] <- 0.0
        let i = invertible a
        Assert.Equal(false, i)

    [<Fact>]
    let ``Calculating the inverse of a matrix``() =
        let a = matrix(4)
        a.[0,0] <- -5.0
        a.[0,1] <- 2.0
        a.[0,2] <- 6.0
        a.[0,3] <- -8.0
        a.[1,0] <- 1.0
        a.[1,1] <- -5.0
        a.[1,2] <- 1.0
        a.[1,3] <- 8.0
        a.[2,0] <- 7.0
        a.[2,1] <- 7.0
        a.[2,2] <- -6.0
        a.[2,3] <- -7.0
        a.[3,0] <- 1.0
        a.[3,1] <- -3.0
        a.[3,2] <- 7.0
        a.[3,3] <- 4.0
        let d = determinant a
        Assert.True(areEqualFloat d 532.0)
        let c = cofactor a 2 3
        Assert.True(areEqualFloat c -160.0)
        let c = cofactor a 3 2
        Assert.True(areEqualFloat c 105.0)
        let e = matrix(4)
        e.[0,0] <- 0.218045112781954875
        e.[0,1] <- 0.451127819548872155
        e.[0,2] <- 0.240601503759398483
        e.[0,3] <- -0.045112781954887216
        e.[1,0] <- -0.808270676691729362
        e.[1,1] <- -1.456766917293233154
        e.[1,2] <- -0.443609022556390953
        e.[1,3] <- 0.520676691729323293
        e.[2,0] <- -0.078947368421052627
        e.[2,1] <- -0.223684210526315791
        e.[2,2] <- -0.052631578947368418
        e.[2,3] <- 0.197368421052631582
        e.[3,0] <- -0.522556390977443663
        e.[3,1] <- -0.813909774436090250
        e.[3,2] <- -0.300751879699248104
        e.[3,3] <- 0.306390977443609047        
        let r = inverse a
        match r with
        | Ok m -> Assert.True(m.Equals e)
        | Error s -> Assert.True(false, s)

    [<Fact>]
    let ``Calculating the inverse of another matrix``() =
        let a = matrix(4)
        a.[0,0] <- 8.0
        a.[0,1] <- -5.0
        a.[0,2] <- 9.0
        a.[0,3] <- 2.0
        a.[1,0] <- 7.0
        a.[1,1] <- 5.0
        a.[1,2] <- 6.0
        a.[1,3] <- 1.0
        a.[2,0] <- -6.0
        a.[2,1] <- 0.0
        a.[2,2] <- 9.0
        a.[2,3] <- 6.0
        a.[3,0] <- -3.0
        a.[3,1] <- 0.0
        a.[3,2] <- -9.0
        a.[3,3] <- -4.0
        let e = matrix(4)
        e.[0,0] <- -0.153846153846153855
        e.[0,1] <- -0.153846153846153855
        e.[0,2] <- -0.282051282051282048
        e.[0,3] <- -0.538461538461538436
        e.[1,0] <- -0.076923076923076927
        e.[1,1] <- 0.123076923076923084
        e.[1,2] <- 0.025641025641025640
        e.[1,3] <- 0.030769230769230771
        e.[2,0] <- 0.358974358974358976
        e.[2,1] <- 0.358974358974358976
        e.[2,2] <- 0.435897435897435903
        e.[2,3] <- 0.923076923076923128
        e.[3,0] <- -0.692307692307692291
        e.[3,1] <- -0.692307692307692291
        e.[3,2] <- -0.769230769230769273
        e.[3,3] <- -1.923076923076923128
        let r = inverse a
        match r with
        | Ok m -> Assert.True(m.Equals e)
        | Error s -> Assert.True(false, s)

    [<Fact>]
    let ``Calculating the inverse of a third matrix``() =
        let a = matrix(4)
        a.[0,0] <- 9.0
        a.[0,1] <- 3.0
        a.[0,2] <- 0.0
        a.[0,3] <- 9.0
        a.[1,0] <- -5.0
        a.[1,1] <- -2.0
        a.[1,2] <- -6.0
        a.[1,3] <- -3.0
        a.[2,0] <- -4.0
        a.[2,1] <- 9.0
        a.[2,2] <- 6.0
        a.[2,3] <- 4.0
        a.[3,0] <- -7.0
        a.[3,1] <- 6.0
        a.[3,2] <- 6.0
        a.[3,3] <- 2.0
        let e = matrix(4)
        e.[0,0] <- -0.040740740740740744
        e.[0,1] <- -0.077777777777777779
        e.[0,2] <- 0.144444444444444431
        e.[0,3] <- -0.222222222222222210
        e.[1,0] <- -0.077777777777777779
        e.[1,1] <- 0.033333333333333333
        e.[1,2] <- 0.366666666666666641
        e.[1,3] <- -0.333333333333333315
        e.[2,0] <- -0.029012345679012345
        e.[2,1] <- -0.146296296296296285
        e.[2,2] <- -0.109259259259259264
        e.[2,3] <- 0.129629629629629622
        e.[3,0] <- 0.177777777777777785
        e.[3,1] <- 0.066666666666666666
        e.[3,2] <- -0.266666666666666663
        e.[3,3] <- 0.333333333333333315
        let r = inverse a
        match r with
        | Ok m -> Assert.True(m.Equals e)
        | Error s -> Assert.True(false, s)
        
    [<Fact>]
    let ``Multiplying a product by its inverse``() =
        let a = matrix(4)
        a.[0,0] <- 3.0
        a.[0,1] <- -9.0
        a.[0,2] <- 7.0
        a.[0,3] <- 3.0
        a.[1,0] <- 3.0
        a.[1,1] <- -8.0
        a.[1,2] <- 2.0
        a.[1,3] <- -9.0
        a.[2,0] <- -4.0
        a.[2,1] <- 4.0
        a.[2,2] <- 4.0
        a.[2,3] <- 1.0
        a.[3,0] <- -6.0
        a.[3,1] <- 5.0
        a.[3,2] <- -1.0
        a.[3,3] <- -1.0
        let b = matrix(4)
        b.[0,0] <- 8.0
        b.[0,1] <- 2.0
        b.[0,2] <- 2.0
        b.[0,3] <- 2.0
        b.[1,0] <- 3.0
        b.[1,1] <- -1.0
        b.[1,2] <- 7.0
        b.[1,3] <- 0.0
        b.[2,0] <- 7.0
        b.[2,1] <- 0.0
        b.[2,2] <- 5.0
        b.[2,3] <- 4.0
        b.[3,0] <- 6.0
        b.[3,1] <- -2.0
        b.[3,2] <- 0.0
        b.[3,3] <- 5.0
        let c = a * b
        let r = inverse b
        match r with
        | Ok ib -> Assert.True(a.Equals (c * ib))
        | Error s -> Assert.True(false, s)
