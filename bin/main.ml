open Types
open Draws

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

let rec animate ctx creet =
  (* Efface le canvas *)
  ctx##.fillStyle := Js.string "white";
  ctx##fillRect 0. 0. canvas_width canvas_height;

  (* Redessine le décor *)
  draw_the_hospital ctx canvas_width canvas_height;
  draw_warzone ctx canvas_width canvas_height;
  draw_death_line ctx canvas_width canvas_height;
  display_creet_pos ctx creet;

  (* Met à jour la position *)
  update_creet_pos creet;

  (* Redessine la créature *)
  ctx##beginPath;
  draw_a_creet ctx "rgb(226, 33, 33)" creet radius;
  ctx##stroke;

  (* Relance la boucle d’animation *)
  let _ = Html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> animate ctx creet)) in
  ()
;;


let onload _ =
  let canvas = Html.getElementById "canvas" in
  (match Js.Opt.to_option (Html.CoerceTo.canvas canvas) with
   | Some canvas_element ->
       let c = canvas_element##getContext Html._2d_ in
       let random_x = Random.float (canvas_width -. 40.) in
       let random_y = Random.float (canvas_height -. 40.) in
       let radius = radius +. Random.float 0. in
       let creet = { Types.x = random_x; y = random_y; vx = 1.; vy = 1.; radius = radius } in
       animate c creet;
   | None -> ());
  Js._true
;;

let () = Html.window##.onload := Html.handler onload


(* TODO: 
  - Handle the drag when clicking on a creature
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