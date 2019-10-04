namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Color

module ColorTests = 

    [<Fact>]
    let ``color equality``() =
        let areEqual r g b w = 
            let left = { red = r; green = g; blue = b; }
            let right = { red = r; green = g; blue = b; }
            left = right
        Check.VerboseThrowOnFailure areEqual

    [<Fact>]
    let ``adding colors``() =
        let f r1 g1 b1 r2 g2 b2 =
            let c1 = color r1 g1 b1
            let c2 = color r2 g2 b2
            let s1 = c1 + c2
            let s2 = color (c1.red + c2.red) (c1.green + c2.green) (c1.blue + c2.blue)
            s1 = s2
        Check.VerboseThrowOnFailure f

    [<Fact>]
    let ``subtracting colors``() =
        let f r1 g1 b1 r2 g2 b2 =
            let c1 = color r1 g1 b1
            let c2 = color r2 g2 b2
            let s1 = c1 - c2
            let s2 = color (c1.red - c2.red) (c1.green - c2.green) (c1.blue - c2.blue)
            s1 = s2
        Check.VerboseThrowOnFailure f

    [<Fact>]
    let ``multiplying a color by a scalar``() =
        let f r g b s =
            let c = color r g b
            let s1 = c * s
            let s2 = color (c.red * s) (c.green * s) (c.blue * s)
            s1 = s2
        Check.VerboseThrowOnFailure f

    [<Fact>]
    let ``multiplying colors``() =
        let f r g b x y z =
            let c1 = color r g b
            let c2 = color x y z
            let s1 = c1 * c2
            let s2 = color (c1.red * c2.red) (c1.green * c2.green) (c1.blue * c2.blue)
            s1 = s2
        Check.VerboseThrowOnFailure f

    [<Fact>]
    let ``scale color to integer 0..255``() =
        let c = color 1.0 0.0 0.0
        let r,g,b = color_byte c
        Assert.Equal(255uy, r)
        Assert.Equal(0uy, g)
        Assert.Equal(0uy, b)