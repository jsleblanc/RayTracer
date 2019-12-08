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

(*
Commands

Add-camera
    returns a camera_definition_t

Add-light
    returns a point_light

Add-(shape) - returns a shape_definition_t
    plane
    cube
    cylinder
    cone
    sphere
    group
    obj
    <previously defined shape via macro>

define
    material
    transforms
    add

sub-Commands
    extend - lookup existing value
        materials
        transforms
        shapes



could create a bunch of functions for handling individual small attributes
    ie: color, transform, diffuse/ambient/specular/etc 

make some functions for handling sequences of small attributes
    ie: colors, transforms 

make a shape creation function, constructed as attributes are scanned
    ie: sphere has no properties so function is simple, cylinder/cone have min/max/closed, so look for those and update the function, return the function to be used later


scan all nodes
    when an "add" is found        
        run all little functions on all sub-nodes, which will return option types
        fold attributes to create shape-building function
        initialize empty state objects (material, shape)
        apply any Some values to material or shape as appropriate
        if any sub-attribute is "children" recurse

*)



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

    let to_color l = 
        match l with
        | [r;g;b;] -> Color.color r g b
        | _ -> failwith "expected sequence of 3 floats!"

    let to_tuple f pf =
        match f with
        | [x;y;z] -> pf x y z
        | _ -> failwith "expected sequence of 3 floats!"

    let get_node_children node = 
        match node with
        | SeqNode n -> n.Data
        | _ -> List.empty

    let extract field cmd arg (parse_f:string -> 'y -> 'z) (process_f:'c -> 'T -> 'U) value =
        parse_f field cmd arg 
        |> Option.map (process_f value)
        |> Option.defaultValue value

    let parse_field_bool field cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,ScalarNode a) when c.Data = field -> 
            let (success,value) = Boolean.TryParse a.Data
            match (success,value) with
            | (true,value) -> Some value
            | (false,_) -> None
        | _ -> None

    let parse_field_int field cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,ScalarNode a) when c.Data = field -> Some (int (a.Data))
        | _ -> None

    let parse_field_float field cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,ScalarNode a) when c.Data = field -> Some (float (a.Data))
        | _ -> None

    let parse_field_string field cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,ScalarNode a) when c.Data = field -> Some (a.Data)
        | _ -> None

    let parse_field_float_seq field cmd arg =
        let func node =
            match node with
            | ScalarNode n -> float n.Data
            | _ -> failwith "expected a ScalarNode containing a float"
        match (cmd,arg) with
        | (ScalarNode c,SeqNode s) when c.Data = field -> Some (List.map func s.Data)
        | _ -> None


    let parse_shadow = parse_field_bool "shadow"
    let parse_diffuse = parse_field_float "diffuse"
    let parse_ambient = parse_field_float "ambient"
    let parse_specular = parse_field_float "specular"
    let parse_reflective = parse_field_float "reflective"
    let parse_color cmd arg = 
        let result = parse_field_float_seq "color" cmd arg
        match result with
        | Some [r;g;b;] -> Some (Color.color r g b)
        | Some _ -> None
        | None -> None

    let parse_transform node =
        match node with
        | [ScalarNode c;ScalarNode v;] when c.Data = "rotate-x" -> Some (Rotation_X(float v.Data))
        | [ScalarNode c;ScalarNode v;] when c.Data = "rotate-y" -> Some (Rotation_Y(float v.Data))
        | [ScalarNode c;ScalarNode v;] when c.Data = "rotate-z" -> Some (Rotation_Z(float v.Data))
        | [ScalarNode c;ScalarNode x;ScalarNode y;ScalarNode z;] when c.Data = "scale" -> Some (Scaling(float x.Data,float y.Data,float z.Data))
        | [ScalarNode c;ScalarNode x;ScalarNode y;ScalarNode z;] when c.Data = "translate" -> Some (Translation(float x.Data,float y.Data,float z.Data))
        | _ -> None

    let parse_transforms cmd arg scene =
        match (cmd,arg) with
        | ScalarNode c, SeqNode a when c.Data = "transform" ->
            a.Data
            |> List.map get_node_children
            |> List.map parse_transform
            |> List.choose id
        | ScalarNode c, ScalarNode a when c.Data = "transform" -> //macro
            match scene.transformations.TryGetValue a.Data with
            | (true,value) -> value
            | _ -> List.empty
        | _ -> List.empty

    let parse_colors cmd arg scene =
        let func nodes =
            match nodes with
            | [ScalarNode r;ScalarNode g;ScalarNode b;] -> Some [float r.Data;float g.Data;float b.Data;]
            | _ -> None
        match (cmd,arg) with
        | ScalarNode c, SeqNode a when c.Data = "colors" ->
            a.Data
            |> List.map get_node_children
            |> List.map func
            |> List.choose id
            |> List.map to_color
        //| ScalarNode c, ScalarNode a when c.Data = "colors" -> None //macro //TODO
        | _ -> List.empty


    let parse_pattern nodes material = 
        material

    let parse_material nodes scene =
        scene

    let parse_light nodes scene = 
        let empty = {
            position = point 0.0 0.0 0.0;
            intensity = Color.color 1.0 1.0 1.0;
        }
        let func state (cmd,arg) =
            state
            |> extract "at" cmd arg parse_field_float_seq (fun l f -> { l with position = to_tuple f point; })
            |> extract "intensity" cmd arg parse_field_float_seq (fun l f -> { l with intensity = to_color f; })
        let light = nodes |> List.fold func empty
        { scene with lights = scene.lights @ [light;] }

    let parse_camera nodes scene =        
        let func state (cmd,arg) = 
            let camera = 
                state.camera
                |> extract "width" cmd arg parse_field_int (fun c i -> { c with width = i; })
                |> extract "height" cmd arg parse_field_int (fun c i -> { c with height = i; })
                |> extract "field-of-view" cmd arg parse_field_float (fun c f -> { c with field_of_view = f; })
                |> extract "from" cmd arg parse_field_float_seq (fun c f -> { c with from_point = to_tuple f point; })
                |> extract "to" cmd arg parse_field_float_seq (fun c f -> { c with to_point = to_tuple f point; })
                |> extract "up" cmd arg parse_field_float_seq (fun c f -> { c with up = to_tuple f vector; })
            { state with camera = camera; }
        nodes |> List.fold func scene

    let parse_shape shape nodes scene =
        scene

    let parse_add cmd nodes (scene:scene_t) =
        match cmd with
        | ScalarNode c when c.Data = "camera" -> parse_camera nodes scene
        | ScalarNode c when c.Data = "light" -> parse_light nodes scene
        | ScalarNode c -> parse_shape c.Data nodes scene
        | _ -> scene


    let parse_define cmd nodes (scene:scene_t) =
        match cmd with
        | ScalarNode c -> scene
        | _ -> scene




    let parse_scene node =
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
        let rec func node (scene:scene_t) =
            match node with
            | SeqNode s -> s.Data |> List.fold (fun s np -> func np s) scene
            | MapNode mn -> 
                match mn.Data with
                | (left,right) :: nodes -> 
                    match left with
                    | ScalarNode n when n.Data = "add" -> parse_add right nodes scene
                    | ScalarNode n when n.Data = "define" -> parse_define right nodes scene
                    | _ -> failwith "unrecognized command!"
                | _ -> scene
            | _ -> scene
        func node empty
        






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

    let handle_nodes_to_numbers nodes = 
        nodes |> handle_seq_numbers |> list_to_coords

    let handle_camera camera cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,ScalarNode a) when c.Data = "width" -> { camera with width = int a.Data }
        | (ScalarNode c,ScalarNode a) when c.Data = "height" -> { camera with height = int a.Data } 
        | (ScalarNode c,ScalarNode a) when c.Data = "field-of-view" -> { camera with field_of_view = float a.Data } 
        | (ScalarNode c,SeqNode s) when c.Data = "from" -> 
            let (x,y,z) = handle_nodes_to_numbers s.Data
            { camera with from_point = point x y z; }
        | (ScalarNode c,SeqNode s) when c.Data = "to" -> 
            let (x,y,z) = handle_nodes_to_numbers s.Data
            { camera with to_point = point x y z; }
        | (ScalarNode c,SeqNode s) when c.Data = "up" -> 
            let (x,y,z) = handle_nodes_to_numbers s.Data
            { camera with up = vector x y z; }
        | _ -> failwith "unexpected camera attribute"

    let handle_transform cmd =
        match cmd with
        | [ScalarNode c;ScalarNode v;] when c.Data = "rotate-x" -> Rotation_X(float v.Data)
        | [ScalarNode c;ScalarNode v;] when c.Data = "rotate-y" -> Rotation_Y(float v.Data)
        | [ScalarNode c;ScalarNode v;] when c.Data = "rotate-z" -> Rotation_Z(float v.Data)
        | [ScalarNode c;ScalarNode x;ScalarNode y;ScalarNode z;] when c.Data = "scale" -> Scaling(float x.Data,float y.Data,float z.Data)
        | [ScalarNode c;ScalarNode x;ScalarNode y;ScalarNode z;] when c.Data = "translate" -> Translation(float x.Data,float y.Data,float z.Data)
        | _ -> failwith "unexpected transformation"


    let handle_light light cmd arg = 
        match (cmd,arg) with
        | (ScalarNode c,SeqNode s) when c.Data = "at" -> 
            let (x,y,z) = handle_nodes_to_numbers s.Data
            { light with position = point x y z; }
        | (ScalarNode c,SeqNode s) when c.Data = "intensity" -> 
            let (r,g,b) = handle_nodes_to_numbers s.Data
            { light with intensity = Color.color r g b; }
        | _ -> failwith "unexpected light attribute"    

    let handle_material cmd arg material =
        match (cmd,arg) with
        | (ScalarNode c,SeqNode a) when c.Data = "color" -> 
            let (r,g,b) = handle_nodes_to_numbers a.Data
            { material with color = Color.color r g b; }
        | (ScalarNode c,ScalarNode a) when c.Data = "ambient" -> { material with ambient = float a.Data; }
        | (ScalarNode c,ScalarNode a) when c.Data = "diffuse" -> { material with diffuse = float a.Data; }
        | (ScalarNode c,ScalarNode a) when c.Data = "specular" -> { material with specular = float a.Data; }
        | (ScalarNode c,ScalarNode a) when c.Data = "shininess" -> { material with shininess = float a.Data; }
        | (ScalarNode c,ScalarNode a) when c.Data = "reflective" -> { material with reflective = float a.Data; }
        | (ScalarNode c,ScalarNode a) when c.Data = "transparency" -> { material with transparency = float a.Data; }
        | (ScalarNode c,ScalarNode a) when c.Data = "refractive-index" -> { material with refractive_index = float a.Data; }
        | _ -> material

    let handle_shape (shape:shape_definition_t) nodes state =
        let func (s:shape_definition_t) (cmd,arg) =
            match (cmd,arg) with
            | (ScalarNode c,SeqNode a) when c.Data = "transform" ->
                let func node = 
                    match node with
                    | SeqNode n -> handle_transform n.Data
                let transformations = a.Data |> List.map func
                { s with transformations = transformations; }
            | (ScalarNode c,ScalarNode a) when c.Data = "shadow" ->
                { s with shadow = Boolean.Parse a.Data; }
            | (ScalarNode c,MapNode a) when c.Data = "material" ->
                let m = a.Data |> List.fold (fun m (cmd,arg) -> handle_material cmd arg m) Material.material.Default
                { s with material = Some m; }
            | _ -> failwith "unexpected shape property"
        nodes |> List.fold func shape        

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
        | (ScalarNode c,ScalarNode a) when c.Data = "add" && a.Data = "cube" -> 
            let shape = {
                shape = Cube;
                material = None;
                shadow = true;
                transformations = [];
            }
            let shape = nodes |> List.fold (fun s (l,r) -> handle_shape s nodes state) shape
            { state with shapes = state.shapes @ [shape;] }
        | _ -> state

    let rec parse_node n (state:scene_t) =
        match n with
        | MapNode mn -> 
            match mn.Data with
            | (left,right) :: nodes -> handle_command left right nodes state                
            | _ -> failwith "unexpected"
            










    let parse_yaml s =
        let parser = Yaml12Parser(YamlExtended.Schema)
        let repr = (parser.``l-yaml-stream`` s)
        match repr.Head with
        | CompleteRepresentaton cr -> cr.Document
        | NoRepresentation nr -> failwith "error!"
        | _ -> failwith "Unexpected return type"

    let parse_text (text:string) =
        let node = parse_yaml text
        parse_scene node
        
    let parse_file file =
        File.ReadAllText(file) |> parse_text
        
    let private shape_definition_to_shape s =
        let sf = 
            match s.shape with 
            | Cube -> ShapeCube.build
        let transforms = combine s.transformations
        sf |> Shapes.transform transforms |> Shapes.shadow s.shadow

    let scene_to_world (s:scene_t) =
        let vt = view_transform s.camera.from_point s.camera.to_point s.camera.up
        let camera = create_camera s.camera.height s.camera.width s.camera.field_of_view
        let camera = { camera with transform = vt; }
        let light = s.lights.[0]
        let shapes = s.shapes |> List.map shape_definition_to_shape
        (camera,Worlds.build shapes light)
