(* A global component that loads the saved progress into the user data: it
   asks for the stored value on the first event and fills the user data when
   the reply arrives. Scenes save it themselves when a level is solved. *)

open Ml_regl_core
open Messenger

type msg = |

let key : msg Global_component.key = Global_component.key "progress"

type data = { asked : bool }

let component : (data, msg, User_data.t) Scene.concrete_global_component =
  {
    init =
      (fun _runtime _env ->
        ({ asked = false }, { Scene.dead = false; post_processor = Fun.id }));
    update =
      (fun _runtime env evnt data bdata ->
        match evnt with
        | Regl_proto.ValueRead { key; value = Some v } when key = User_data.storage_key ->
            let saved = User_data.deserialize v in
            (* keep anything solved before the reply came *)
            let user_data =
              List.fold_left
                (fun u (id, m) -> User_data.record u id m)
                saved env.global_data.user_data.User_data.best
            in
            ((data, bdata), [], ({ env with global_data = { env.global_data with user_data } }, false))
        | _ when not data.asked ->
            (({ asked = true }, bdata), [ Scene.SOMReadValue User_data.storage_key ], (env, false))
        | _ -> ((data, bdata), [], (env, false)));
    updaterec = (fun _runtime _env (msg : msg) _data _bdata -> match msg with _ -> .);
    view = (fun _runtime _env _data _bdata -> Regl_builtin_programs.empty);
    key;
  }

let gc = Global_component.make component
