open Types
open Draw
open Utils

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Lwt_js = Js_of_ocaml_lwt.Lwt_js


let canvas_width = 1800.
let canvas_height = 900.
let radius = 50.
let speed = 2.
let game_over = ref false

let update_creet_pos creet =
  let speed = 
    if creet.is_infected then speed *. 0.75
    else speed
  in

  if creet.berserk && creet.is_infected then begin
    creet.infection_time <- creet.infection_time +. 0.016;
    let max_time = 10.0 in
    let progress = min 1.0 (creet.infection_time /. max_time) in
    creet.radius <- (radius *. 4. *. progress);
    if progress >= 1.0 then begin
      creet.is_dead <- true;
    end;
  end;
  if creet.mean && creet.is_infected then begin
    creet.infection_time <- creet.infection_time +. 0.016;
    let max_time = 10.0 in
    let progress = min 1.0 (creet.infection_time /. max_time) in
    creet.radius <- (radius *. 0.85 *. progress);

  end;

  if creet.x -. creet.radius < 0. then creet.x <- creet.radius;
  if creet.x +. creet.radius > canvas_width then creet.x <- canvas_width -. creet.radius;
  if creet.y -. creet.radius < 0. then creet.y <- creet.radius;
  if creet.y +. creet.radius > canvas_height then creet.y <- canvas_height -. creet.radius;

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
  if !game_over then () (* Si game over, ne rien faire *)
  else
    let living_creets = List.filter (fun creet -> not creet.is_dead) creets in
    let healthy_creets = List.filter (fun creet -> not creet.is_infected) living_creets in
    
    (* Vérifier game over en premier *)
    if List.length healthy_creets = 0 then begin
      display_game_over ctx living_creets healthy_creets canvas_width canvas_height game_over;
      (* NE PAS continuer après game over *)
    end
    else begin
      (* Seulement si pas game over, faire l'animation normale *)
      ctx##clearRect 0. 0. canvas_width canvas_height;
      ctx##.fillStyle := Js.string "white";
      ctx##fillRect 0. 0. canvas_width canvas_height;

      (* Redraw the decor *)
      draw_the_hospital ctx canvas_width canvas_height;
      draw_warzone ctx canvas_width canvas_height;
      draw_death_line ctx canvas_width canvas_height;
      
      (* Collisions avec living_creets *)
      List.iteri (fun i creet1 ->
        for j = i + 1 to List.length living_creets - 1 do
          let creet2 = List.nth living_creets j in
          handle_collision creet1 creet2;
          display_total_creet ctx living_creets;
        done
      ) living_creets;

      (* Update and draw avec living_creets *)
      Lwt.async (fun () ->
        Lwt_list.iter_s (fun creet ->
          is_colliding_with_hospital creet canvas_height;
          if not creet.is_dragging then begin
            is_colliding_with_death_line creet canvas_height;
          end;
          ctx##beginPath;
          draw_a_creet ctx creet.color creet radius;
          Lwt.return_unit
        ) living_creets;  (* ICI: utiliser living_creets au lieu de creets *)
      );
        
      (* Restart animation *)
      let _ = Html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> animate ctx living_creets)) in
      ()
    end
;;
let onload _ =
  let canvas = Html.getElementById "canvas" in
  (match Js.Opt.to_option (Html.CoerceTo.canvas canvas) with
   | Some canvas_element ->
       let ctx = canvas_element##getContext Html._2d_ in

       (* 1. Create the list of creatures *)
       Random.self_init ();
       let creets = 
         let create_random_creet _ = 
           let margin = 100. in
           let x = margin +. Random.float (canvas_width -. 2. *. margin) in
           let y = margin +. Random.float (canvas_height -. 2. *. margin) in
           let vx = if Random.bool () then speed else -.speed in
           let vy = if Random.bool () then speed else -.speed in
           { Types.x = x; y = y; vx = vx; vy = vy; radius = radius; 
             color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; 
             has_bounced = false; berserk = false; mean = false; is_dragging = false; 
             infection_time = 0. }
         in
         Array.to_list (Array.init 18 create_random_creet) in

        (* 2. System of drag for all creatures *)
        setup_drag_system (Js.Unsafe.coerce canvas_element) creets canvas_width canvas_height;
        
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
  - Add a way to increase the radius of the creatures ✅
  - Add a way to decrease the radius of the creatures ✅
  - Add a way to increase the speed of the creatures ✅
  - Add a way to decrease the speed of the creatures ✅
  - Add a way to follow the heat of the creatures 
**)