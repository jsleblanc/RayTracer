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
open YamlDotNet.Core;
open YamlDotNet.Serialization;

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
    
    type scene_t = {
        camera:camera_t;
        lights:point_light list;
        materials:Map<string,material>;
        transformations:Map<string,translation_t list>;
        shapes:shape_definition_t list;
        shape_templates:Map<string,shape_definition_t>;
    }

    let private yaml_deserializer = 
        let d = new YamlDotNet.Serialization.DeserializerBuilder()
        d.Build()

    let parse_text (text:string) =
        let deserializer = yaml_deserializer
        let x = deserializer.Deserialize(text)
        {
            camera = create_default_camera 100 100;
            lights = [];
            materials = Map.empty;
            transformations = Map.empty;
            shapes = [];
            shape_templates = Map.empty;
        }
        
    let parse_file file =
        File.ReadAllText(file) |> parse_text
        
    let scene_to_world scene =
        Worlds.build_default []