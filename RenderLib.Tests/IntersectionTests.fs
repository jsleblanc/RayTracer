namespace RenderLib.Tests

open System
open Xunit
open FsCheck
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Ray
open RenderLib.Shapes

module IntersectionTests = 
    
    [<Fact>]
    let ``An intersection encapsulates t and object``() =
        let s = sphere()
        let i = {
            t = 3.5;
            obj = Sphere s;
        }
        Assert.Equal(Sphere s, i.obj)