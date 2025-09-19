open Types
open Draw
open Utils

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js
module Lwt_js = Js_of_ocaml_lwt.Lwt_js


let canvas_width = 1800.
let canvas_height = 900.
let radius = 50.
let speed = ref 1.
let game_over = ref false

let update_creet_pos creet =
  let current_speed = 
    if creet.is_infected then !speed *. 0.75
    else !speed
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
    creet.vx <- normalized_vx *. current_speed;
    creet.vy <- normalized_vy *. current_speed;
  end;

  creet.x <- creet.x +. creet.vx;
  creet.y <- creet.y +. creet.vy;
  if creet.x -. creet.radius <= 0. || creet.x +. creet.radius >= canvas_width then creet.vx <- -.creet.vx;
  if creet.y -. creet.radius <= 0. || creet.y +. creet.radius >= canvas_height then creet.vy <- -.creet.vy
;;

let rec create_loop creet =
  let%lwt () = Lwt_js.sleep 0.016 in
  update_creet_pos creet;
  let%lwt () = Lwt.return () in
  create_loop creet
;;

let rec animate ctx creets_ref =
  if !game_over then ()
  else
    let creets = !creets_ref in
    let living_creets = List.filter (fun creet -> not creet.is_dead) creets in
    let healthy_creets = List.filter (fun creet -> not creet.is_infected) living_creets in
    if List.length healthy_creets = 0 then begin
      display_game_over ctx living_creets healthy_creets canvas_width canvas_height game_over;
    end
    else begin
      ctx##clearRect 0. 0. canvas_width canvas_height;
      ctx##.fillStyle := Js.string "white";
      ctx##fillRect 0. 0. canvas_width canvas_height;

      draw_the_hospital ctx canvas_width canvas_height;
      draw_warzone ctx canvas_width canvas_height;
      draw_death_line ctx canvas_width canvas_height;
      display_creet_speed ctx (List.nth creets 0);
      display_total_creet ctx living_creets;
      
      List.iteri (fun i creet1 ->
        for j = i + 1 to List.length living_creets - 1 do
          let creet2 = List.nth living_creets j in
          handle_collision creet1 creet2;
        done
      ) living_creets;

      List.iter (fun creet ->
        mean_purchase_creet creet living_creets !speed;
      ) living_creets;

      manage_speed_increase speed;

      let new_creets = manage_automatic_reproduction creets canvas_width canvas_height radius speed in
      if List.length new_creets > 0 then begin
        creets_ref := creets @ new_creets;
        List.iter (fun creet -> Lwt.async (fun () -> create_loop creet)) new_creets;
      end;

      Lwt.async (fun () ->
        Lwt_list.iter_s (fun creet ->
          if not creet.is_dragging then begin
            is_colliding_with_death_line creet canvas_height;
          end;
          if creet.is_dragging then begin
            is_colliding_with_hospital creet canvas_height;
          end;
          ctx##beginPath;
          draw_a_creet ctx creet.color creet radius;
          Lwt.return_unit
        ) living_creets;
      );
        
        let _ = Html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> animate ctx creets_ref)) in
      ()
    end
;;
let onload _ =
  let canvas = Html.getElementById "canvas" in
  (match Js.Opt.to_option (Html.CoerceTo.canvas canvas) with
   | Some canvas_element ->
       let ctx = canvas_element##getContext Html._2d_ in

       Random.self_init ();
       let initial_creets = 
         let create_random_creet _ = 
           let margin = 100. in
           let x = margin +. Random.float (canvas_width -. 2. *. margin) in
           let y = margin +. Random.float (canvas_height -. 2. *. margin) in
           let vx = if Random.bool () then !speed else -.(!speed) in
           let vy = if Random.bool () then !speed else -.(!speed) in
           { Types.x = x; y = y; vx = vx; vy = vy; radius = radius; 
             color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; 
             has_bounced = false; berserk = false; mean = false; is_dragging = false; 
             infection_time = 0. }
         in
         Array.to_list (Array.init 18 create_random_creet) in

        let creets_ref = ref initial_creets in
        setup_drag_system (Js.Unsafe.coerce canvas_element) initial_creets canvas_width canvas_height;
        
        animate (Js.Unsafe.coerce ctx) creets_ref;
         
        List.iter (fun creet -> Lwt.async (fun () -> create_loop creet)) initial_creets;

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
  - Add a way to display game over ✅
  - Add a way to display the number of creatures alive and dead ✅
  - Add a way to follow the heat of the creatures ✅
  - Add a way to change randomly the creet direction ✅
  - Add a way to reproduce the creatures ✅
  - Add a way to increase the game speed ✅
**)