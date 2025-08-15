module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js

let canvas_width = 1800.  (* Added a dot to make it a float *)
let canvas_height = 900.
  
let draw_the_hospital context =
  context##.fillStyle := Js.string "rgb(226, 33, 33)";
  context##fillRect (canvas_width *. 0.1) 0. 20. canvas_height

let draw_warzone context =
  context##.fillStyle := Js.string "rgba(184, 184, 184, 0.6)";
  context##fillRect (canvas_width *. 0.1 +. 20.) 0. (canvas_width *. 0.75 -. 20.) canvas_height

let draw_death_line context =
  context##.fillStyle := Js.string "rgb(27, 176, 133)";
  context##fillRect (canvas_width *. 0.85) 0. 60. canvas_height

(* TODO: Draw a Creet *)

(* Load the canvas and draw the elements *)
let onload _ =
  let canvas = Html.getElementById "canvas" in
  (match Js.Opt.to_option (Html.CoerceTo.canvas canvas) with
   | Some canvas_element ->
       let c = canvas_element##getContext Html._2d_ in
       draw_the_hospital c;
       draw_death_line c;
       draw_warzone c
   | None -> ());
  Js._true


let () = Html.window##.onload := Html.handler onload 