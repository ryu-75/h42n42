open Types
open Draw
open Utils

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let canvas_width = 1800.
let canvas_height = 900.
let radius = 60.

let update_creet_pos creet =
  creet.x <- creet.x +. creet.vx;
  creet.y <- creet.y +. creet.vy;
  if creet.x -. creet.radius <= 0. || creet.x +. creet.radius >= canvas_width then creet.vx <- -.creet.vx;
  if creet.y -. creet.radius <= 0. || creet.y +. creet.radius >= canvas_height then creet.vy <- -.creet.vy

let rec animate ctx creets =
  (* Delete the canvas *)
  ctx##clearRect 0. 0. canvas_width canvas_height;
  ctx##.fillStyle := Js.string "white";
  ctx##fillRect 0. 0. canvas_width canvas_height;

  (* Redraw the decor *)
  draw_the_hospital ctx canvas_width canvas_height;
  draw_warzone ctx canvas_width canvas_height;
  draw_death_line ctx canvas_width canvas_height;

  (* Update and draw all creatures *)
  List.iter (fun creet ->
    update_creet_pos creet;
    ctx##beginPath;
    draw_a_creet ctx "rgb(226, 33, 33)" creet radius;
    ctx##stroke;
  ) creets;

  (* Restart the animation loop *)
  let _ = Html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> animate ctx creets)) in
  ()
;;


let onload _ =
  let canvas = Html.getElementById "canvas" in
  (match Js.Opt.to_option (Html.CoerceTo.canvas canvas) with
   | Some canvas_element ->
       let ctx = canvas_element##getContext Html._2d_ in

       (* 1. Create the list of creatures *)
       let creets = [
         { Types.x = 350.; y = 300.; vx = 1.; vy = 1.; radius = radius };
         { Types.x = 400.; y = 500.; vx = 1.; vy = 1.; radius = radius };
         { Types.x = 450.; y = 100.; vx = 1.; vy = 1.; radius = radius };
         { Types.x = 500.; y = 700.; vx = 1.; vy = 1.; radius = radius };
       ] in

               (* 2. System of drag for all creatures *)
        setup_drag_system (Js.Unsafe.coerce canvas_element) creets;
        
        (* 3. Start the animation for all creatures *)
        animate (Js.Unsafe.coerce ctx) creets;

   | None -> ());
  Js._true
;;


let () = Html.window##.onload := Html.handler onload


(* TODO: 
  - Handle the drag when clicking on a creature ✅
  - Added a second type of creature
  - Handle collision between creatures
  - Add a way to poison the creatures
  - Add a way to heal the creatures
  - Add a way to speed up the creatures
  - Add a way to slow down the creatures
  - Add a way to increase the radius of the creatures
  - Add a way to decrease the radius of the creatures
  - Add a way to increase the speed of the creatures
  - Add a way to decrease the speed of the creatures
**)