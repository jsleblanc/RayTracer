namespace RenderLib

open System
open System.IO
open Canvas
open Matrix
open Tuple
open Ray
open Translations
open Shapes
open Material
open Lights
open Camera
open Worlds
open Legivel.Parser
open Legivel.RepresentationGraph
open Legivel.TagResolution
open System.Text

module Scenes = 

    type shape_definition_t = {
        shape:shape_template_t;
        material:material option;
        shadow:bool;        
        transformations:translation_t list;
    }
    and shape_template_t =
    | Plane
    | Sphere
    | Cube
    | Cylinder of Minimum:float * Maximum:float * Closed:bool
    | Cone of Minimum:float * Maximum:float * Closed:bool
    | Group of Children:shape_definition_t list
    | ObjectFile of shapeObj:string
    //| Union of shape_definition_t * shape_definition_t
    //| Intersect of shape_definition_t * shape_definition_t
    //| Difference of shape_definition_t * shape_definition_t
    
    type camera_definition_t = {
        width:int;
        height:int;
        field_of_view:float;
        from_point:tuple;
        to_point:tuple;
        up:tuple;
    }

    type scene_t = {
        camera:camera_definition_t;
        lights:point_light list;
        materials:Map<string,material>;
        transformations:Map<string,translation_t list>;
        shapes:shape_definition_t list;
        shape_templates:Map<string,shape_definition_t>;
    }




    let parse_transform (items:string list) =
        match items with
        | ["translate"; x; y; z;] -> Some (Translation(float x,float y,float z))
        | ["scale"; x; y; z;] -> Some (Scaling(float x,float y,float z))
        | ["rotate-x"; r;] -> Some (Rotation_X(float r))
        | ["rotate-y"; r;] -> Some (Rotation_Y(float r))
        | ["rotate-z"; r;] -> Some (Rotation_Z(float r))
        | _ -> None
        
        
    let parse_material_attribute (key:string,value:string list) material =
        match (key,value) with
        | ("color", [r;g;b;]) -> { material with color = Color.color (float r) (float g) (float b); }
        | ("ambient", [v;]) -> { material with ambient = float v; }
        | ("diffuse", [v;]) -> { material with diffuse = float v; }
        | ("specular", [v;]) -> { material with specular = float v; }
        | ("reflective", [v;]) -> { material with reflective = float v; }
        | ("refractive-index", [v;]) -> { material with refractive_index = float v; }
        | ("shininess", [v;]) -> { material with shininess = float v; }
        | ("transparency", [v;]) -> { material with transparency = float v; }
        | _ -> material

        (*
    let parse_color (items:string list) =
        match items with
        | r; g; b;] -> Some (Color (float r) (float g) (float z)
        *)


    let parse_yaml s =
        let parser = Yaml12Parser(YamlExtended.Schema)
        let repr = (parser.``l-yaml-stream`` s)
        match repr.Head with
        | CompleteRepresentaton cr -> cr.Document
        | NoRepresentation nr -> failwith "error!"
        | _ -> failwith "Unexpected return type"

    let list_to_coords l =
        match l with
        | [x;y;z;] -> (x,y,z)
        | _ -> failwith "expected list of only 3 values"

    let handle_seq_numbers nodes = 
        let func node =
            match node with
            | ScalarNode n -> float n.Data
            | _ -> failwith "expected a ScalarNode containing a float"
        nodes |> List.map func

    let handle_camera state cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,ScalarNode a) when c.Data = "width" -> { state with width = int a.Data }
        | (ScalarNode c,ScalarNode a) when c.Data = "height" -> { state with height = int a.Data } 
        | (ScalarNode c,ScalarNode a) when c.Data = "field-of-view" -> { state with field_of_view = float a.Data } 
        | (ScalarNode c,SeqNode s) when c.Data = "from" -> 
            let (x,y,z) = s.Data |> handle_seq_numbers |> list_to_coords
            { state with from_point = point x y z; }
        | (ScalarNode c,SeqNode s) when c.Data = "to" -> 
            let (x,y,z) = s.Data |> handle_seq_numbers |> list_to_coords
            { state with to_point = point x y z; }
        | (ScalarNode c,SeqNode s) when c.Data = "up" -> 
            let (x,y,z) = s.Data |> handle_seq_numbers |> list_to_coords
            { state with up = vector x y z; }
        | _ -> failwith "unexpected camera attribute"

    let handle_light state cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,SeqNode s) when c.Data = "at" -> 
            let (x,y,z) = s.Data |> handle_seq_numbers |> list_to_coords
            { state with position = point x y z; }
        | (ScalarNode c,SeqNode s) when c.Data = "intensity" -> 
            let (r,g,b) = s.Data |> handle_seq_numbers |> list_to_coords
            { state with intensity = Color.color r g b; }
        | _ -> failwith "unexpected light attribute"

    let handle_command cmd arg nodes state = 
        match (cmd,arg) with
        | (ScalarNode c,ScalarNode a) when c.Data = "add" && a.Data = "camera" -> 
            let empty = {
                width = 0;
                height = 0;
                field_of_view = 0.0;
                from_point = point 0.0 0.0 0.0;
                to_point = point 0.0 0.0 0.0;
                up = vector 0.0 0.0 0.0;
            }
            let camera = nodes |> List.fold (fun s (l,r) -> handle_camera s l r) empty
            { state with camera = camera; }
        | (ScalarNode c,ScalarNode a) when c.Data = "add" && a.Data = "light" -> 
            let empty = {
                position = point 0.0 0.0 0.0;
                intensity = Color.color 0.0 0.0 0.0;
            }
            let light = nodes |> List.fold (fun s (l,r) -> handle_light s l r) empty        
            { state with lights = state.lights @ [light;] }
        | _ -> state

    let rec parse_node n (state:scene_t) =
        match n with
        | ScalarNode sn -> 
            state
        | SeqNode sn -> 
            sn.Data |> List.fold (fun s np -> parse_node np s) state
        | MapNode mn -> 
            (*let func l r s = 
                match (l,r) with
                | (ScalarNode c, ScalarNode n) ->                     
                    printfn "%s %s" (c.Data) (n.Data)
                    s
                | _ -> s*)
            match mn.Data with
            | (left,right) :: nodes -> handle_command left right nodes state                
            | _ -> failwith "unexpected"
            //mn.Data |> List.fold (fun s (l,r) -> func l r s) state
            
        

    let parse_text (text:string) =
        let camera = {
            width = 0;
            height = 0;
            field_of_view = 0.0;
            from_point = point 0.0 0.0 0.0;
            to_point = point 0.0 0.0 0.0;
            up = vector 0.0 1.0 0.0;
        }
        let empty = {
            camera = camera;
            lights = [];
            materials = Map.empty;
            transformations = Map.empty;
            shapes = [];
            shape_templates = Map.empty;
        }
        let x = parse_yaml text
        parse_node x empty

        
    let parse_file file =
        File.ReadAllText(file) |> parse_text
        
    let scene_to_world scene =
        Worlds.build_default []
