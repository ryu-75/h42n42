open Types

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let setup_drag_system canvas creets =
  let current_dragging = ref None in
  let offset_x = ref 0. in
  let offset_y = ref 0. in

  canvas##.onmousedown := Html.handler (fun ev ->
    let mouse_event = (Js.Unsafe.coerce ev : Html.mouseEvent Js.t) in
    let rect = canvas##getBoundingClientRect () in
    let mouse_x = float_of_int mouse_event##.clientX -. rect##.left in 
    let mouse_y = float_of_int mouse_event##.clientY -. rect##.top in

    (* Find which creature is clicked *)
    List.iter (fun creet ->
      if abs_float (mouse_x -. creet.x) <= creet.radius && 
         abs_float (mouse_y -. creet.y) <= creet.radius 
      then begin
        current_dragging := Some creet;
        offset_x := mouse_x -. creet.x;
        offset_y := mouse_y -. creet.y;
      end
    ) creets;

    Js._true
  );

  Html.window##.onmousemove := Html.handler (fun ev ->
    match !current_dragging with
    | Some creet ->
      let mouse_event = (Js.Unsafe.coerce ev : Html.mouseEvent Js.t) in
      let rect = canvas##getBoundingClientRect () in
      creet.x <- float_of_int mouse_event##.clientX -. rect##.left -. !offset_x;
      creet.y <- float_of_int mouse_event##.clientY -. rect##.top -. !offset_y;
      Js._true
    | None -> Js._true
  );

  Html.window##.onmouseup := Html.handler (fun _ ->
    current_dragging := None;
    Js._true
  )
