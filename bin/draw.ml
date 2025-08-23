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

let display_total_creet ctxt creet =
  ctxt##.fillStyle := Js.string "black";
  ctxt##.font := Js.string "16px Arial serif";
  ctxt##fillText (Js.string (Printf.sprintf "Total: %d" (List.length creet))) 10. 30.

let display_game_over ctx living_creets healthy_creets height width game_over =
   if List.length healthy_creets = 0 && List.length living_creets > 0 then begin
    (* Toutes les créatures vivantes sont malades - GAME OVER *)
    ctx##clearRect 0. 0. (width *. 2.) height;
    ctx##.fillStyle := Js.string "rgb(0, 0, 0)";
    ctx##fillRect 0. 0. (width *. 2.) height;
    
    ctx##.fillStyle := Js.string "red";
    ctx##.font := Js.string "48px Arial";
    ctx##.textAlign := Js.string "center";
    ctx##fillText (Js.string "GAME OVER") width (height /. 4.);
    game_over := true;
    ()
  end
  else if List.length living_creets = 0 then begin
    (* Toutes les créatures sont mortes - GAME OVER aussi *)
    ctx##clearRect 0. 0. (width *. 2.) height;
    ctx##.fillStyle := Js.string "rgb(0, 0, 0)";
    ctx##fillRect 0. 0. (width *. 2.) height;

    
    ctx##.fillStyle := Js.string "red";
    ctx##.font := Js.string "48px Arial";
    ctx##.textAlign := Js.string "center";
    ctx##fillText (Js.string "GAME OVER") width (height /. 4.);
    game_over := true;
    ()
  end