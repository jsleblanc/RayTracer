﻿namespace RenderLib

open System
open Common
open Tuple
open Ray
open Shapes

module ShapeConic =

    let parameters_of (shape:shape) =
        match shape.shape with
        | Cylinder (minimum,maximum,closed) -> (minimum,maximum,closed)
        | Cone (minimum,maximum,closed) -> (minimum,maximum,closed)
        | _ -> raise (Exception("expected a cylinder or a cone"))
    
    let intersect_caps trail shape min max closed ray radius_at xs =
        let check_cap t radius =
            let x = ray.origin.x + t * ray.direction.x
            let z = ray.origin.z + t * ray.direction.z
            (x ** 2.0 + z ** 2.0) <= radius ** 2.0
        if (not closed) || ((Math.Abs ray.direction.y) < epsilon) then
            xs
        else
        let xs_p =
            let t = (min - ray.origin.y) / ray.direction.y
            if check_cap t (radius_at min) then (build_intersection t shape trail) :: xs else xs
        let t = (max - ray.origin.y) / ray.direction.y
        if check_cap t (radius_at max) then (build_intersection t shape trail) :: xs_p else xs_p
    
    let intersect (shape:shape) trail r (afn:ray->float) (bfn:ray->float) (cfn:ray->float) radius_at =
      let (minimum, maximum, closed) = parameters_of shape
      let a = afn r
      let b = bfn r
      let xs =
        if (Math.Abs(a)) < epsilon then
          if (Math.Abs(b)) < epsilon then
            []
          else
            let c = cfn r
            let t = (-c) / (2.0 * b)
            [ build_intersection t shape trail ]
        else
          let c = cfn r
          let disc = b ** 2.0 - 4.0 * a * c
          if disc < 0.0 then
            []
          else
            let root = sqrt disc
            let t0 = (-b - root) / (2.0 * a)
            let t1 = (-b + root) / (2.0 * a)
            let xs =
              let y = r.origin.y + t1 * r.direction.y
              if minimum < y && y < maximum then [ build_intersection t1 shape trail ] else []
            let y = r.origin.y + t0 * r.direction.y
            if minimum < y && y < maximum then
              (build_intersection t0 shape trail) :: xs
            else xs
      sort_intersection (intersect_caps trail shape minimum maximum closed r radius_at xs)
    
    let normal_at shape (point:tuple) nfn =
      let (minimum, maximum, _) = parameters_of shape
      let dist = point.x ** 2.0 + point.z ** 2.0
      if dist < 1.0 && point.y >= (maximum - epsilon) then
        vector 0.0 1.0 0.0
      else if dist < 1.0 && point.y <= (minimum + epsilon) then
        vector 0.0 -1.0 0.0
      else
        nfn point