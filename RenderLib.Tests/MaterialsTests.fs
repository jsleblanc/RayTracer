namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Translations
open RenderLib.Color
open RenderLib.Lights

module MaterialsTests = 

    [<Fact>]
    let ``Lighting with the eye between the light and the surface``() =
        let m = material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 0.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m light position eyev normalv
        Assert.Equal(color 1.9 1.9 1.9, r)

    [<Fact>]
    let ``Lighting with the eye between light and surface, eye offset 45 degrees``() =
        let m = material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 (Math.Sqrt(2.0)/2.0) (-Math.Sqrt(2.0)/2.0)
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 0.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m light position eyev normalv
        Assert.Equal(color 1.0 1.0 1.0, r)

    [<Fact>]
    let ``Lighting with eye opposite surface, light offset 45 degrees``() =
        let m = material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 10.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m light position eyev normalv
        Assert.Equal(color 0.7364 0.7364 0.7364, r)

    [<Fact>]
    let ``Lighting with eye in the path of the reflection vector``() =
        let m = material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 (-Math.Sqrt(2.0)/2.0) (-Math.Sqrt(2.0)/2.0)
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 10.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m light position eyev normalv
        Assert.Equal(color 1.6364 1.6364 1.6364, r)

    [<Fact>]
    let ``Lighting with the light behind the surface``() =
        let m = material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 0.0 10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m light position eyev normalv
        Assert.Equal(color 0.1 0.1 0.1, r)

