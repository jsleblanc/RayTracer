﻿namespace RenderLib

open System
open System.IO
open Tuple
open Shapes

module ObjectFiles = 
    
    type parsed_obj_t = {
        defaultGroup:shape list;
        namedGroups:Map<string,shape list>;
        vertices:tuple[];
        normals:tuple[];
        inGroup:bool;
        currentGroup:string;
    }

    type face_t = {
        vertice_indexes:int[];
        normal_indexes:int[];
        smooth:bool;
    }

    let private parse_vertice (line:string) =
        if line.StartsWith "v" then
            let i = line.Split ' '
            Some (point (float i.[1]) (float i.[2]) (float i.[3]))
        else None

    let private parse_normal (line:string) =
        if line.StartsWith "vn" then
            let i = line.Split ' '
            Some (vector (float i.[1]) (float i.[2]) (float i.[3]))
        else None

    let private parse_vertices lines state =
        let vertices = 
            lines 
            |> Array.map parse_vertice
            |> Array.choose id
        { state with vertices = Array.concat [| state.vertices; vertices |]; }

    let private parse_vertex_normals lines state =
        let normals =
            lines
            |> Array.map parse_normal
            |> Array.choose id
        { state with normals = Array.concat [| state.normals; normals |]; }

    let private fan_triangulation face state =
        let vertice x = state.vertices.[face.vertice_indexes.[x]]
        let normal x = state.normals.[face.normal_indexes.[x]]
        let triangle i =
            match face.smooth with
            | true -> ShapeTriangle.build_smooth (vertice 0) (vertice i) (vertice (i+1)) (normal 0) (normal i) (normal (i+1))
            | false -> ShapeTriangle.build (vertice 0) (vertice i) (vertice (i+1))
        [| 1 .. (Array.length face.vertice_indexes) - 2 |]
        |> Array.map triangle

    let private merge left right =
        right @ left

    let private add_shapes_to_default_group shapes state =
        { state with defaultGroup = state.defaultGroup @ shapes; }

    let private add_shapes_to_named_group shapes state = 
        let mergedShapes = 
            match state.namedGroups.TryFind state.currentGroup with
            | Some group -> group
            | None -> []
            |> merge shapes
        let updatedMap = state.namedGroups.Add(state.currentGroup, mergedShapes)
        { state with namedGroups = updatedMap; }

    let private parse_face (line:string) (state:parsed_obj_t) =
        let face (line:string) =
            let empty = {
                vertice_indexes = Array.empty;
                normal_indexes = Array.empty;
                smooth = false;
            }
            let func state (v,n) =
                let vi = Array.append state.vertice_indexes [|v|]
                let (ni, s) = 
                    match n with
                    | n when n > 0 -> (Array.append state.normal_indexes [|n|], true)
                    | _ -> (state.normal_indexes, false)
                { state with vertice_indexes = vi; normal_indexes = ni; smooth = s; }
            line.Split ' '
            |> Array.skip 1
            |> Array.map (fun (s) -> s.Split '/')
            |> Array.map (fun (entries) ->
                match entries with
                | [|v|] -> (int v, 0)
                | [|v;_;n|] -> (int v, int n)
                | _ -> (0, 0))
            |> Array.fold func empty
        if line.StartsWith "f" then
            let f = face line 
            let triangles = fan_triangulation f state |> Array.toList
            if state.inGroup then add_shapes_to_named_group triangles state
            else add_shapes_to_default_group triangles state
        else state

    let private parse_group_enter (line:string) state =
        if line.StartsWith "g" then
            let name = (line.Split ' ').[1]
            { state with inGroup = true; currentGroup = name; }
        else state

    let private parse_lines (lines:string[]) =
        let empty = {
            defaultGroup = []; 
            namedGroups = Map.empty; 
            vertices = [|(point 0.0 0.0 0.0)|];
            normals = [|(vector 0.0 0.0 0.0)|];
            inGroup = false;
            currentGroup = "";
        }
        let lines_filtered = 
            lines |> Array.map (fun (s) -> s.Trim())
        let state = 
            empty
            |> parse_vertices lines_filtered
            |> parse_vertex_normals lines_filtered
        let parse_groups state (line:string) = 
            state
            |> parse_group_enter line
            |> parse_face line
        lines_filtered |> Array.fold parse_groups state

    let parse_text (text:string) =
        let lines = text.Split (Environment.NewLine.ToCharArray())
        parse_lines lines

    let result_to_group result =
        let default_group_children = ShapeGroup.build result.defaultGroup
        let named_groups = result.namedGroups |> Map.toSeq |> Seq.map snd |> Seq.map ShapeGroup.build |> Seq.toList
        match named_groups with
        | [] -> default_group_children
        | g -> ShapeGroup.build ([default_group_children] @ g) //TODO add testing for unnecessary nesting

    let parse_file fileName  =
        File.ReadAllLines fileName 
        |> parse_lines 
        |> result_to_group 
        |> ShapeGroup.divide
