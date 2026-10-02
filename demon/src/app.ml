(* The whole application: configuration, resources, scenes and global
   components. *)

open Messenger

let virtual_size : Ui.size = { width = 1280.; height = 720. }

(* MAXWELL_LEVEL=1-15 starts straight in that level (handy for testing). *)
let init_scene : Scene.target =
  let index id =
    Array.find_index (String.equal id) Levels.ids
  in
  match Option.bind (Sys.getenv_opt "MAXWELL_LEVEL") index with
  | Some index -> By_key (Scenes.Play_params.key, { index })
  | None -> By_name "Title"

let config : User_data.t Ui.user_config =
  {
    init_scene;
    virtual_size;
    fbo_num = 5;
    max_assets_per_frame = 8;
    enabled_program = Ui.AllBuiltinProgram;
    time_interval = Ml_regl_core.Regl_proto.AnimationFrame;
    default_global_data =
      {
        user_data = User_data.default;
        camera = Camera.default ~width:virtual_size.width ~height:virtual_size.height;
        volume = 1.;
      };
    app_name = Some "Maxwells Puzzling Demon";
    init_window =
      {
        Ml_regl_core.Regl_proto.default_window_config with
        title = Some "Maxwell's Puzzling Demon";
        (* Resizing refits the 16:9 picture. Tiling compositors (niri) tile a
           resizable window into a column; a window rule with open-floating
           keeps it at its 1280x720 start size. *)
        resizable = Some true;
      };
  }

let texture path =
  Resources.Texture_res
    ( path,
      Some
        {
          Ml_regl_core.Regl_proto.default_texture_options with
          mag = Some MagLinear;
          min = Some LinearMipmapLinear;
        } )

let font name = Resources.Font_res ("assets/fonts/" ^ name ^ ".png", "assets/fonts/" ^ name ^ ".json")

(* Paths are relative to the working directory: run from the project root. *)
let resources : Resources.resource_defs =
  [
    ("display", font "display");
    ("mono", font "mono");
    ("mono_bold", font "mono_bold");
  ]
  @ List.map
      (fun n -> (n, texture ("assets/img/" ^ n ^ ".png")))
      [ "demon"; "demon_cold"; "demon_hot"; "demon_big"; "flame"; "snow" ]
  @ Sfx.resources @ Levels.resources

let scenes =
  Scene.table
    [
      Scene.named "Title" Scenes.Title.Model.scene;
      Scene.named "Select" Scenes.Select.Model.scene;
      Scene.entry Scenes.Play_params.key Scenes.Play.Model.scene;
    ]

let input : User_data.t Ui.input =
  {
    config;
    resources;
    scenes;
    (* the loading screen last: drawn over the settings gear until done *)
    global_components = [ Progress.gc; Settings.gc; Messenger_extra.Asset_loading.gen_gc () ];
  }
