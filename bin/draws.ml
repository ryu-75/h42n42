open Types

module Js = Js_of_ocaml.Js

let draw_the_hospital context width height =
  context##.fillStyle := Js.string "rgb(226, 33, 33)";
  context##fillRect (width *. 0.1) 0. 20. height

let draw_warzone context width height =
  context##.fillStyle := Js.string "rgba(184, 184, 184, 0.6)";
  context##fillRect (width *. 0.1 +. 20.) 0. (width *. 0.75 -. 20.) height

let draw_death_line context width height =
  context##.fillStyle := Js.string "rgb(27, 176, 133)";
  context##fillRect (width *. 0.85) 0. 60. height

let display_creet_pos ctx creet = 
  ctx##.font := Js.string "12px Arial";
  ctx##.fillStyle := Js.string "black";
  ctx##fillText (Js.string (Printf.sprintf "x: %.1f, y: %.1f" creet.x creet.y)) 10. 30.

let draw_a_creet context color creet radius = 
  context##.fillStyle := Js.string color;
  context##arc creet.x creet.y radius 0. (2. *. Float.pi) (Js.bool false); (* x, y, radius, startAngle, endAngle, counterclockwise *)
  context##fill