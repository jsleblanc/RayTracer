namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Translations

module TranslationTests =

    [<Fact>]
    let ``Multiplying by a transformation matrix``() =
        let t = translation 5.0 -3.0 2.0
        let p = point -3.0 4.0 5.0
        let r = t * p
        let e = point 2.0 1.0 7.0
        Assert.Equal(e, r)

    [<Fact>]
    let ``Multiplying by the inverse of a transformation matrix``() =
        let t = translation 5.0 -3.0 2.0
        let i = inverse t
        let p = point -3.0 4.0 5.0
        let e = point -8.0 7.0 3.0
        Assert.Equal(e, i * p)

    [<Fact>]
    let ``Translation does not affect vectors``() =
        let t = translation 5.0 -3.0 2.0
        let v = vector -3.0 4.0 5.0
        let r = t * v
        Assert.Equal(v, r)

    [<Fact>]
    let ``A scaling matrix applied to a point``() =
        let t = scaling 2.0 3.0 4.0
        let p = point -4.0 6.0 8.0
        let r = t * p
        let e = point -8.0 18.0 32.0
        Assert.Equal(e, r)

    [<Fact>]
    let ``A scaling matrix applied to a vector`` () =
        let t = scaling 2.0 3.0 4.0
        let v = vector -4.0 6.0 8.0
        let r = t * v
        let e = vector -8.0 18.0 32.0
        Assert.Equal(e, r)

    [<Fact>]
    let ``Multiplying by the inverse of a scaling matrix``() =
        let t = scaling 2.0 3.0 4.0
        let v = vector -4.0 6.0 8.0
        let e = vector -2.0 2.0 2.0
        let invT = inverse t
        Assert.Equal(e, invT * v)

    [<Fact>]
    let ``Reflection is scaling by a negative value``() =
        let t = scaling -1.0 1.0 1.0
        let p = point 2.0 3.0 4.0
        let e = point -2.0 3.0 4.0
        let r = t * p
        Assert.Equal(e, r)

    [<Fact>]
    let ``Rotating a point around the x axis``() =
        let p = point 0.0 1.0 0.0
        let half_quarter = rotation_x (Math.PI / 4.0)
        let full_quarter = rotation_x (Math.PI / 2.0)
        let v = (Math.Sqrt 2.0) / 2.0
        Assert.Equal(point 0.0 v v, roundtuple(half_quarter * p))
        Assert.Equal(point 0.0 0.0 1.0, roundtuple(full_quarter * p))

    [<Fact>]
    let ``The inverse of an x-rotation rotates in the opposite direction``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 1.0 0.0
        let half_quarter = rotation_x (Math.PI / 4.0)
        let inv = inverse half_quarter
        Assert.Equal(point 0.0 v -v, inv * p)

    [<Fact>]
    let ``Rotating a point around the y axix``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 0.0 1.0
        let half_quarter = rotation_y (Math.PI / 4.0)
        let full_quarter = rotation_y (Math.PI / 2.0)
        Assert.Equal(point v 0.0 v, roundtuple(half_quarter * p))
        Assert.Equal(point 1.0 0.0 0.0, roundtuple(full_quarter * p))

    [<Fact>]
    let ``The inverse of an y-rotation rotates in the opposite direction``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 0.0 1.0
        let half_quarter = rotation_y (Math.PI / 4.0)
        let inv = inverse half_quarter
        Assert.Equal(point -v 0.0 v, inv * p)

    [<Fact>]
    let ``Rotating a point around the z axix``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 1.0 0.0
        let half_quarter = rotation_z (Math.PI / 4.0)
        let full_quarter = rotation_z (Math.PI / 2.0)
        Assert.Equal(point -v v 0.0, roundtuple(half_quarter * p))
        Assert.Equal(point -1.0 0.0 0.0, roundtuple(full_quarter * p))

    [<Fact>]
    let ``The inverse of an z-rotation rotates in the opposite direction``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 1.0 0.0 0.0
        let half_quarter = rotation_z (Math.PI / 4.0)
        let inv = inverse half_quarter
        Assert.Equal(point v -v 0.0, inv * p)

    [<Fact>]
    let ``A shearing transformation moves x in proportion to y``() =
        let t = shearing 1.0 0.0 0.0 0.0 0.0 0.0
        let p = point 2.0 3.0 4.0
        Assert.Equal(point 5.0 3.0 4.0, t * p)

    [<Fact>]
    let ``A shearing transformation moves x in proportion to z``() =
        let t = shearing 0.0 1.0 0.0 0.0 0.0 0.0
        let p = point 2.0 3.0 4.0
        Assert.Equal(point 6.0 3.0 4.0, t * p)

    [<Fact>]
    let ``A shearing transformation moves y in proportion to x``() =
        let t = shearing 0.0 0.0 1.0 0.0 0.0 0.0
        let p = point 2.0 3.0 4.0
        Assert.Equal(point 2.0 5.0 4.0, t * p)

    [<Fact>]
    let ``A shearing transformation moves y in proportion to z``() =
        let t = shearing 0.0 0.0 0.0 1.0 0.0 0.0
        let p = point 2.0 3.0 4.0
        Assert.Equal(point 2.0 7.0 4.0, t * p)

    [<Fact>]
    let ``A shearing transformation moves z in proportion to x``() =
        let t = shearing 0.0 0.0 0.0 0.0 1.0 0.0
        let p = point 2.0 3.0 4.0
        Assert.Equal(point 2.0 3.0 6.0, t * p)

    [<Fact>]
    let ``A shearing transformation moves z in proportion to y``() =
        let t = shearing 0.0 0.0 0.0 0.0 0.0 1.0
        let p = point 2.0 3.0 4.0
        Assert.Equal(point 2.0 3.0 7.0, t * p)

    [<Fact>]
    let ``Individual transformations are applied in sequence``() =
        let p = point 1.0 0.0 1.0
        let a = rotation_x (Math.PI / 2.0)
        let b = scaling 5.0 5.0 5.0
        let c = translation 10.0 5.0 7.0
        let p2 = point 1.0 -1.0 0.0
        Assert.Equal(p2, roundtuple (a * p))
        let p3 = point 5.0 -5.0 0.0
        Assert.Equal(p3, roundtuple(b * p2))
        let p4 = point 15.0 0.0 7.0
        Assert.Equal(p4, roundtuple(c * p3))

    [<Fact>]
    let ``Chained transformations must be applied in reverse order``() =
        let p = point 1.0 0.0 1.0
        let a = rotation_x (Math.PI / 2.0)
        let b = scaling 5.0 5.0 5.0
        let c = translation 10.0 5.0 7.0
        let t = c * b * a
        Assert.Equal(point 15.0 0.0 7.0, t * p)

    [<Fact>]
    let ``The transformation matrix for the default orientation``() =
        let from = point 0.0 0.0 0.0
        let to_point = point 0.0 0.0 -1.0
        let up = vector 0.0 1.0 0.0
        let t = view_transform from to_point up
        Assert.Equal(identity_matrix (), t)

    [<Fact>]
    let ``A view transformation matrix looking in the positive z direction``() =
        let from = point 0.0 0.0 0.0
        let to_point = point 0.0 0.0 1.0
        let up = vector 0.0 1.0 0.0
        let t = view_transform from to_point up
        Assert.Equal(scaling -1.0 1.0 -1.0, t)

    [<Fact>]
    let ``The view transformation moves the world``() =
        let from = point 0.0 0.0 8.0
        let to_point = point 0.0 0.0 0.0
        let up = vector 0.0 1.0 0.0
        let t = view_transform from to_point up
        Assert.Equal(translation 0.0 0.0 -8.0, t)

    [<Fact>]
    let ``An arbitrary view transformation``() = 
        let from = point 1.0 3.0 2.0
        let to_point = point 4.0 -2.0 8.0
        let up = vector 1.0 1.0 0.0
        let t = view_transform from to_point up
        let a = matrix(4)
        a.[0,0] <- -0.507092552837000055
        a.[0,1] <- 0.507092552837000055
        a.[0,2] <- 0.676123403782999954
        a.[0,3] <- -2.366431913240000018
        a.[1,0] <- 0.767715933860000033
        a.[1,1] <- 0.606091526730999974
        a.[1,2] <- 0.121218305346000005
        a.[1,3] <- -2.828427124746000221
        a.[2,0] <- -0.358568582799999980
        a.[2,1] <- 0.597614304667000051
        a.[2,2] <- -0.717137165601000048
        a.[2,3] <- 0.0
        a.[3,0] <- 0.0
        a.[3,1] <- 0.0
        a.[3,2] <- 0.0
        a.[3,3] <- 1.0
        Assert.True(a.Equals t)
