namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material

module Shapes2 = 

    type boundingBox = {
        minimum: tuple;
        maximum: tuple;
    } with static member Default = {
            minimum = point Double.PositiveInfinity Double.PositiveInfinity Double.PositiveInfinity;
            maximum = point Double.NegativeInfinity Double.NegativeInfinity Double.NegativeInfinity;
        }

    type kind =
    | Plane
    | Sphere
    | Cube
    | Cylinder of Minimum:float * Maximum:float * Closed:bool
    | Cone of Minimum:float * Maximum:float * Closed:bool
    | Group

    type shape = {
        kind:kind;
        material:material;
        transformation:matrix;
        children:shape seq;
        boundingBox:unit -> boundingBox;
        intersect:shape -> ray -> intersection seq;
        normal_at:shape -> tuple -> tuple;
    }
    and intersection = {
        t: float;
        obj: shape;
    } 
