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
      creet.is_dragging <- true;
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
    List.iter (fun creet ->
      creet.is_dragging <- false;
    ) creets;
    Js._true
  )

module RandomChance = struct 
  let chance p = Random.float 1.0 < p

  let two_percent () = chance 0.02
end


let distance x1 y1 x2 y2 =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  sqrt(dx *. dx +. dy *. dy)

let is_colliding creet1 creet2 =
  distance creet1.x creet1.y creet2.x creet2.y <= creet1.radius +. creet2.radius

let handle_collision creet1 creet2 =
  if is_colliding creet1 creet2 && creet1.is_infected && not creet2.is_infected && not creet1.is_dragging then begin
    if RandomChance.two_percent () then begin
      creet2.is_infected <- true;
      creet2.color <- "rgb(126, 33, 226)";
    end
  end
;;

let is_colliding_with_death_line creet height =
  if creet.y < height *. 0.17 && not creet.has_bounced then begin
    creet.color <- "rgb(126, 33, 226)";
    creet.is_infected <- true
  end
;;

let is_colliding_with_hospital creet height =
  if creet.y > (height *. 0.95 -. 20.) && creet.y < (height *. 0.95 +. 20.) then begin
    creet.color <- "rgb(33, 226, 126)";
    creet.is_infected <- false
  end;
;;
