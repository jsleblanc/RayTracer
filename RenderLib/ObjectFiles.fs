namespace RenderLib

open System
open System.IO
open System.Text
open Tuple
open Shapes

module ObjectFiles = 
    
    type parsed_obj_t = {
        defaultGroup:shape;
        namedGroups:Map<string,shape>;
        vertices:tuple list;
        ignoredLines:int;
    }

    let parse (sr:StreamReader) =
        false

    let parse_file fileName  =
        false

    let parse_text text =
        {
            defaultGroup = ShapeGroup.build [];
            namedGroups = Map.empty;
            vertices = [];
            ignoredLines = 0;
        }

    let result_to_group result =
        ShapeGroup.build []

    let parse_to_group text =
        ShapeGroup.build []
