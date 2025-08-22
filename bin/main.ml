open Types
open Draw
open Utils

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Lwt_js = Js_of_ocaml_lwt.Lwt_js


let canvas_width = 1800.
let canvas_height = 900.
let radius = 60.
let speed = 2.

let update_creet_pos creet =
  let speed = 
    if creet.is_infected then speed *. 0.75
    else speed
  in

  let magnitude = sqrt (creet.vx *. creet.vx +. creet.vy *. creet.vy) in
  if magnitude <> 0. then begin
    let normalized_vx = creet.vx /. magnitude in
    let normalized_vy = creet.vy /. magnitude in
    creet.vx <- normalized_vx *. speed;
    creet.vy <- normalized_vy *. speed;
  end;

  creet.x <- creet.x +. creet.vx;
  creet.y <- creet.y +. creet.vy;
  if creet.x -. creet.radius <= 0. || creet.x +. creet.radius >= canvas_width then creet.vx <- -.creet.vx;
  if creet.y -. creet.radius <= 0. || creet.y +. creet.radius >= canvas_height then creet.vy <- -.creet.vy
;;

(* Boucle asynchrone séparée pour chaque créature *)
let rec create_loop creet =
  let%lwt () = Lwt_js.sleep 0.016 in
  update_creet_pos creet;
  let%lwt () = Lwt.return () in
  create_loop creet
;;

let rec animate ctx creets =
  (* Delete the canvas *)
  ctx##clearRect 0. 0. canvas_width canvas_height;
  ctx##.fillStyle := Js.string "white";
  ctx##fillRect 0. 0. canvas_width canvas_height;

  (* Redraw the decor *)
  draw_the_hospital ctx canvas_width canvas_height;
  draw_warzone ctx canvas_width canvas_height;
  draw_death_line ctx canvas_width canvas_height;

  List.iteri (fun i creet1 ->
    for j = i + 1 to List.length creets - 1 do
      let creet2 = List.nth creets j in
      handle_collision creet1 creet2;
    done
  ) creets;

  
  (* Update and draw all creatures *)
  Lwt.async (fun () ->
    Lwt_list.iter_s (fun creet ->
      is_colliding_with_hospital creet canvas_height;
      if not creet.is_dragging then begin
        is_colliding_with_death_line creet canvas_height;
      end;
      ctx##beginPath;
      draw_a_creet ctx creet.color creet radius;

      Lwt.return_unit
    ) creets;
  );
    
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
         { Types.x = 500.; y = 500.; vx = speed ; vy = speed; radius = radius; color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; has_bounced = false; berserk = false; mean = false; is_dragging = false };
         { Types.x = 800.; y = 700.; vx = speed; vy = speed; radius = radius; color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; has_bounced = false; berserk = false; mean = false; is_dragging = false };
         { Types.x = 400.; y = 650.; vx = speed; vy = speed; radius = radius; color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; has_bounced = false; berserk = false; mean = false; is_dragging = false };
         { Types.x = 400.; y = 700.; vx = speed; vy = speed; radius = radius; color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; has_bounced = false; berserk = false; mean = false; is_dragging = false };
         { Types.x = 550.; y = 750.; vx = speed; vy = speed; radius = radius; color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; has_bounced = false; berserk = false; mean = false; is_dragging = false };
       ] in

        (* 2. System of drag for all creatures *)
        setup_drag_system (Js.Unsafe.coerce canvas_element) creets;
        
        (* 3. Start the animation for all creatures *)
        animate (Js.Unsafe.coerce ctx) creets;
        
        (* 4. Start async loops for each creature *)
        List.iter (fun creet -> Lwt.async (fun () -> create_loop creet)) creets;

   | None -> ());
  Js._true
;;


let () = Html.window##.onload := Html.handler onload


(* TODO: 
  - Handle the drag when clicking on a creature ✅
  - Added a second type of creature ✅
  - Handle collision between creatures ✅
  - Add a way to poison the creatures ✅
  - Add a way to heal the creatures ✅
  - Add a way to speed up the creatures ✅
  - Add a way to slow down the creatures ✅
  - Add a way to increase the radius of the creatures
  - Add a way to decrease the radius of the creatures
  - Add a way to increase the speed of the creatures
  - Add a way to decrease the speed of the creatures
**)