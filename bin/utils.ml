open Types

module Html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

let setup_drag_system canvas creets width height =
  let current_dragging = ref None in
  let offset_x = ref 0. in
  let offset_y = ref 0. in

  canvas##.onmousedown := Html.handler (fun ev ->
    let mouse_event = (Js.Unsafe.coerce ev : Html.mouseEvent Js.t) in
    let rect = canvas##getBoundingClientRect () in
    let mouse_x = float_of_int mouse_event##.clientX -. rect##.left in 
    let mouse_y = float_of_int mouse_event##.clientY -. rect##.top in

    List.iter (fun creet ->
      if not creet.berserk && not creet.mean then begin
        creet.is_dragging <- true;
        if abs_float (mouse_x -. creet.x) <= creet.radius && 
           abs_float (mouse_y -. creet.y) <= creet.radius 
        then begin
          current_dragging := Some creet;
          offset_x := mouse_x -. creet.x;
          offset_y := mouse_y -. creet.y;
        end
      end
    ) creets;

    Js._true
  );

  Html.window##.onmousemove := Html.handler (fun ev ->
    match !current_dragging with
    | Some creet ->
      let mouse_event = (Js.Unsafe.coerce ev : Html.mouseEvent Js.t) in
      let rect = canvas##getBoundingClientRect () in
      let new_x = float_of_int mouse_event##.clientX -. rect##.left -. !offset_x in
      let new_y = float_of_int mouse_event##.clientY -. rect##.top -. !offset_y in

      creet.x <- max creet.radius (min (width -. creet.radius) new_x);
      creet.y <- max creet.radius (min (height -. creet.radius) new_y);

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
  let ten_percent () = chance 0.1
end


let distance x1 y1 x2 y2 =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  sqrt(dx *. dx +. dy *. dy)

let is_colliding_with_death_line creet height =
  if creet.y < height *. 0.17 && not creet.has_bounced && not creet.berserk && not creet.mean then begin
    creet.color <- "rgb(126, 33, 226)";
    creet.is_infected <- true
  end
;;

let is_colliding_with_hospital creet height =
  if creet.y > (height *. 0.95 -. 20.) && creet.y < (height *. 0.95 +. 20.) && creet.is_dragging then begin
    creet.color <- "rgb(33, 226, 126)";
    creet.is_infected <- false;
    creet.mean <- false;
    creet.berserk <- false;
    creet.infection_time <- 0.;
    creet.radius <- 50.
  end;
;;

let transform_creature_to_mean creet =
  creet.color <- "rgb(250, 143, 71)";
  creet.is_infected <- true;
  creet.mean <- true

let transform_creature_to_berserk creet =
  creet.color <- "rgba(0, 0, 0, 0.65)";
  creet.is_infected <- true;
  creet.berserk <- true

let is_colliding creet1 creet2 =
  distance creet1.x creet1.y creet2.x creet2.y <= creet1.radius +. creet2.radius

let handle_collision creet1 creet2 =
  if is_colliding creet1 creet2 && creet1.is_infected && not creet2.is_infected && not creet1.is_dragging then begin
    if RandomChance.two_percent () then begin
      creet2.is_infected <- true;
      creet2.color <- "rgb(126, 33, 226)";

      let random_val = Random.float 1.0 in
      if random_val < 0.1 then begin
        transform_creature_to_berserk creet2;
      end
      else if random_val < 0.2 then begin
        transform_creature_to_mean creet2;
      end
    end
  end
  else if is_colliding creet1 creet2 && not creet1.is_infected && creet2.is_infected && not creet1.is_dragging then begin
    if RandomChance.two_percent () then begin
      creet1.is_infected <- true;
      creet1.color <- "rgb(126, 33, 226)";

      let random_val = Random.float 1.0 in
      if random_val < 0.1 then begin
        transform_creature_to_berserk creet1;
      end
      else if random_val < 0.2 then begin
        transform_creature_to_mean creet1;
      end
    end
  end

let find_closest_mean_target mean_creet creets =
  let non_infected_targets = List.filter (fun c -> 
    not c.mean && not c.is_infected && c != mean_creet
  ) creets in
  
  match non_infected_targets with
  | [] -> None
  | _ -> 
    let closest = List.fold_left (fun acc creet ->
      let dist_acc = distance mean_creet.x mean_creet.y acc.x acc.y in
      let dist_creet = distance mean_creet.x mean_creet.y creet.x creet.y in
      if dist_creet < dist_acc then creet else acc
    ) (List.hd non_infected_targets) (List.tl non_infected_targets) in
    Some closest

let mean_pursue_target mean_creet target_creet speed =
  let dx = target_creet.x -. mean_creet.x in
  let dy = target_creet.y -. mean_creet.y in
  let dist = distance mean_creet.x mean_creet.y target_creet.x target_creet.y in
  
  if dist > 0. then begin
    let norm_dx = dx /. dist *. speed in
    let norm_dy = dy /. dist *. speed in
    
    mean_creet.x <- mean_creet.x +. norm_dx;
    mean_creet.y <- mean_creet.y +. norm_dy;
    
    mean_creet.vx <- norm_dx;
    mean_creet.vy <- norm_dy;
  end

let mean_purchase_creet mean_creet creets speed =
  if mean_creet.mean && mean_creet.is_infected then begin
    match find_closest_mean_target mean_creet creets with
    | Some target -> 
      let reduced_speed = speed *. 0.85 in
      mean_pursue_target mean_creet target reduced_speed
    | None -> ()
  end
;;

let last_speed_increase_time = ref (Js.to_float (Js.date##now) /. 1000.0)
let last_reproduction_time = ref (Js.to_float (Js.date##now) /. 1000.0)

let manage_speed_increase speed_ref =
  let current_time = Js.to_float (Js.date##now) /. 1000.0 in
  let time_since_last_increase = current_time -. !last_speed_increase_time in
  
  if time_since_last_increase >= 10.0 then begin
    speed_ref := !speed_ref +. 0.2;
    last_speed_increase_time := current_time;
  end

let create_random_creet canvas_width canvas_height radius speed_ref =
  let margin = 100. in
  let x = margin +. Random.float (canvas_width -. 2. *. margin) in
  let y = margin +. Random.float (canvas_height -. 2. *. margin) in
  let vx = if Random.bool () then !speed_ref else -.(!speed_ref) in
  let vy = if Random.bool () then !speed_ref else -.(!speed_ref) in
  { Types.x = x; y = y; vx = vx; vy = vy; radius = radius; 
    color = "rgb(33, 226, 126)"; is_dead = false; is_infected = false; 
    has_bounced = false; berserk = false; mean = false; is_dragging = false; 
    infection_time = 0. }

let manage_automatic_reproduction creets canvas_width canvas_height radius speed_ref =
  let current_time = Js.to_float (Js.date##now) /. 1000.0 in
  let time_since_last_reproduction = current_time -. !last_reproduction_time in
  let living_creets = List.filter (fun creet -> not creet.is_dead) creets in
  
  let reproduction_interval = 25.0 +. Random.float 10.0 in
  if time_since_last_reproduction >= reproduction_interval && List.length living_creets >= 1 then begin
    let num_new_creets = 1 + Random.int 2 in
    let new_creets = ref [] in
    for _ = 1 to num_new_creets do
      let new_creet = create_random_creet canvas_width canvas_height radius speed_ref in
      new_creets := new_creet :: !new_creets
    done;
    last_reproduction_time := current_time;
    !new_creets
  end else
    []
