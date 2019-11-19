namespace RenderLib

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Tuple
open Shapes

module ObjectFiles = 
    
    type parsed_obj_t = {
        defaultGroup:shape;
        namedGroups:Map<string,shape>;
        vertices:tuple list;
        ignoredLines:int;
        inGroup:bool;
        currentGroup:string;
    }

    let private parse_vertice (line:string) state =
        if line.StartsWith "v" then
            let i = line.Split ' '
            let p = point (float i.[1]) (float i.[2]) (float i.[3])
            { state with vertices = state.vertices @ [p]; }
        else state

    let private fan_triangulation vertices =
        [| 1 .. (Array.length vertices) - 2 |]
        |> Array.map (fun (i) -> ShapeTriangle.build vertices.[0] vertices.[i] vertices.[i+1] false)

    let private add_shapes_to_group shapes group =
        let current = ShapeGroup.get_children group
        ShapeGroup.build (current @ shapes);

    let private add_shapes_to_default_group shapes state =
        { state with defaultGroup = add_shapes_to_group shapes state.defaultGroup; }

    let private add_shapes_to_named_group shapes state = 
        let group = 
            match state.namedGroups.TryFind state.currentGroup with
            | Some group -> group
            | None -> ShapeGroup.build []
            |> add_shapes_to_group shapes
        let updatedMap = state.namedGroups.Add(state.currentGroup,group)
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
        if line.StartsWith "g" && state.inGroup = false then
            let name = (line.Split ' ').[1]
            { state with inGroup = true; currentGroup = name; }
        else state

    let private parse_group_leave (line:string) state =
        if not (line.StartsWith "g") && state.inGroup = true then
            { state with inGroup = false; currentGroup = ""; }
        else state

    let private parse_lines lines =
        let empty = {
            defaultGroup = ShapeGroup.build []; 
            namedGroups = Map.empty; 
            vertices = [(point 0.0 0.0 0.0)]; 
            ignoredLines = 0;
            inGroup = false;
            currentGroup = "";
        }        
        let func state (line:string) = 
            state
            |> parse_group_enter line
            |> parse_vertice line
            |> parse_face line
            |> parse_group_leave line
        lines |> Array.except [|""|] |> Array.map (fun (s) -> s.Trim()) |> Array.fold func empty

    let parse_text (text:string) =
        let lines = text.Split (Environment.NewLine.ToCharArray())
        parse_lines lines

    let result_to_group result =
        let default_group_children = ShapeGroup.get_children result.defaultGroup
        let named_groups = result.namedGroups |> Map.toSeq |> Seq.map snd |> Seq.toList
        ShapeGroup.build (default_group_children @ named_groups)

    let parse_to_group text =
        parse_text text |> result_to_group

    let parse_file fileName  =
        File.ReadAllLines fileName |> parse_lines |> result_to_group
