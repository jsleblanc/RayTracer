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

module Scenes = 

    type shape_definition_t = {
        //shape:shape_t;
        material:material option;
        shadow:bool;
        shapeObj:string;
        transformations:translation_t list;
    }

    type scene_t = {
        camera:camera_t;
        lights:point_light list;
        materials:Map<string,material>;
        transformations:Map<string,translation_t list>;
        shapes:shape_definition_t list;
        shape_templates:Map<string,shape_definition_t>;
    }

    let parse_text text =
        {
            camera = create_default_camera 100 100;
            lights = [];
            materials = Map.empty;
            transformations = Map.empty;
            shapes = [];
            shape_templates = Map.empty;
        }
        (*
    let parse_file file =
        null
        *)
    let scene_to_world scene =
        Worlds.build_default []