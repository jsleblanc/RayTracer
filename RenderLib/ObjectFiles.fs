namespace RenderLib

open System
open System.IO
open Tuple
open Shapes

module ObjectFiles = 
    
    type parsed_obj_t = {
        defaultGroup:shape list;
        namedGroups:Map<string,shape list>;
        vertices:tuple[];
        inGroup:bool;
        currentGroup:string;
    }

    let private parse_vertice (line:string) =
        if line.StartsWith "v" then
            let i = line.Split ' '
            Some (point (float i.[1]) (float i.[2]) (float i.[3]))
        else None

    let private parse_vertices lines state =
        let vertices = 
            lines 
            |> Array.map parse_vertice
            |> Array.choose id
        { state with vertices = Array.concat [| state.vertices; vertices |]; }

    let private fan_triangulation vertices =
        [| 1 .. (Array.length vertices) - 2 |]
        |> Array.map (fun (i) -> ShapeTriangle.build vertices.[0] vertices.[i] vertices.[i+1] false)

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
        if line.StartsWith "f" then
            let triangles = 
                line.Split ' '
                |> Array.skip 1
                |> Array.map (fun (i) -> state.vertices.[int i])
                |> fan_triangulation
                |> Array.toList
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
            inGroup = false;
            currentGroup = "";
        }
        let lines_filtered = 
            lines |> Array.map (fun (s) -> s.Trim())
        let state = parse_vertices lines_filtered empty
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
        File.ReadAllLines fileName |> parse_lines |> result_to_group
