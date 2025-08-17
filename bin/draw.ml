open Types

module Js = Js_of_ocaml.Js

let draw_the_hospital context width height =
  context##.fillStyle := Js.string "rgb(226, 33, 33)";
  context##fillRect 0. (height *. 0.99) width 30.

let draw_warzone context width height =
  context##.fillStyle := Js.string "rgba(184, 184, 184, 0.6)";
  context##fillRect 0. (height *. 0.1) width (height *. 0.98)

let draw_death_line context width height =
  context##.fillStyle := Js.string "rgb(27, 176, 133)";
  context##fillRect 0. (height *. 0.1) width 30. (* x1, y1, x2, y2 *)

let display_creet_pos ctx creet = 
  ctx##.font := Js.string "12px Arial";
  ctx##.fillStyle := Js.string "black";
  ctx##fillText (Js.string (Printf.sprintf "x: %.1f, y: %.1f" creet.x creet.y)) 10. 30.

let draw_a_creet context color creet = 
  context##.fillStyle := Js.string color;
  context##arc creet.x creet.y creet.radius 0. (2. *. Float.pi) (Js.bool false); (* x, y, radius, startAngle, endAngle, counterclockwise *)
  context##fill