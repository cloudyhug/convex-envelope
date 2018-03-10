type point = Point of (float * float)
type graph = point list
type line = Line of (point * point)

let x (Point (a, b)) = a
let y (Point (a, b)) = b

(* val graph_of_list : (float * float) list -> graph *)
let rec graph_of_list pointlist : graph =
    match pointlist with
    | [] -> []
    | point :: r -> (Point point) :: graph_of_list r

(* val minimise_on_graph : (point -> 'a) -> graph -> point *)
let minimise_on_graph f (graph : graph) =
    match graph with
    | [] -> failwith "minimise_on_graph - invalid argument : empty graph"
    | firstpoint :: r ->
        let rec minimise graph memorised_point =
            match graph with
            | [] -> memorised_point
            | point :: r -> if f point < f memorised_point then minimise r point else minimise r memorised_point
        in minimise r firstpoint

(* val maximise_on_graph : (point -> 'a) -> graph -> point *)
let maximise_on_graph f (graph : graph) =
    match graph with
    | [] -> failwith "maximise_on_graph - invalid argument : empty graph"
    | firstpoint :: r ->
        let rec maximise graph memorised_point = match graph with
            | [] -> memorised_point
            | point :: r -> if f point > f memorised_point then maximise r point else maximise r memorised_point
        in maximise r firstpoint

(* logical coordinates of the window's physical (0;0) point *)
let originx = ref 0 and originy = ref 0

(* physical size of a segment of logical length 1, in pixels *)
let ssize = ref 40

(* val physical_x : float -> int *)
let physical_x x = !originx + int_of_float (x *. float_of_int !ssize)

(* val physical_y : float -> int *)
let physical_y y = !originy + int_of_float (y *. float_of_int !ssize)

(* val create_window : int -> int -> int -> int -> unit *)
let create_window minx maxx miny maxy =
    let width = (maxx - minx) * !ssize in
    let height = (maxy - miny) * !ssize in
    Graphics.open_graph (Printf.sprintf " %dx%d" width height);
    originx := (-minx) * !ssize;
    originy := (-miny) * !ssize;
    Graphics.set_color (Graphics.rgb 180 180 180);
    for x = 0 to maxx - minx do
        let a = x * !ssize in
        Graphics.moveto a 0; Graphics.lineto a height
    done;
    for y = 0 to maxy - miny do
        let b = y * !ssize in
        Graphics.moveto 0 b; Graphics.lineto width b
    done;
    Graphics.set_color (Graphics.black);
    Graphics.moveto !originx 0;
    Graphics.lineto !originx height;
    Graphics.moveto 0 !originy;
    Graphics.lineto width !originy

(* val open_window : graph -> unit *)
let open_window graph =
    let minx = int_of_float (ceil (x (minimise_on_graph x graph))) - 1 in
    let miny = int_of_float (ceil (y (minimise_on_graph y graph))) - 1 in
    let maxx = int_of_float (x (maximise_on_graph x graph)) + 1 in
    let maxy = int_of_float (y (maximise_on_graph y graph)) + 1 in
    create_window minx maxx miny maxy

(* val close_window : unit -> unit *)
let close_window () = Graphics.close_graph()

(* val draw_point : point -> unit *)
let draw_point (Point (x, y)) =
    Graphics.set_line_width 2;
    let delta = int_of_float (float_of_int !ssize /. 10.0) in
    let rx = physical_x x and ry = physical_y y in
    Graphics.moveto (rx - delta) (ry + delta);
    Graphics.lineto (rx + delta) (ry - delta);
    Graphics.moveto (rx - delta) (ry - delta);
    Graphics.lineto (rx + delta) (ry + delta)

(* val draw_graph : graph -> unit *)
let draw_graph (graph : graph) =
    Graphics.set_color (Graphics.red);
    let rec draw_points pointlist =
        match pointlist with
        | [] -> ()
        | point :: r ->
            draw_point point;
            draw_points r
    in draw_points graph

(* val draw_polygon : point list -> unit *)
let draw_polygon pointlist =
    Graphics.set_line_width 1;
    match pointlist with
    | [] -> ()
    | firstpoint :: r ->
        let rec draw_lines pointlist =
            match pointlist with
            | [] -> ()
            | (Point (a, b)) :: r ->
                Graphics.lineto (physical_x a) (physical_y b);
                draw_lines r
        in
        Graphics.moveto (physical_x (x firstpoint)) (physical_y (y firstpoint));
        draw_lines r;
        Graphics.lineto (physical_x (x firstpoint)) (physical_y (y firstpoint))

(* val isleft : point -> line -> bool *)
let isleft p (Line(p1, p2)) =
    if p = p1 || p = p2 then false else
    let x1, x2, x3 = x p1, x p2, x p in
    let y1, y2, y3 = y p1, y p2, y p in
    (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) > 0.

(* TO REMOVE OR EDIT *)
(* val mostleft : point list -> point -> point option -> point option *)
let rec mostleft graph point mostleftpoint =
    match graph with
    | [] -> mostleftpoint
    | p::r -> begin
        match mostleftpoint with
        | None -> mostleft r point (Some p)
        | Some mlp ->
            if p <> mlp && isleft p (Line(point, mlp)) then
                mostleft r point (Some p)
            else
                mostleft r point (Some mlp)
    end

(* val jarvis : graph -> point list *)
let jarvis graph =
    let p0 = minimise_on_graph x graph in
    let rec iterate graph point envelope =
        match point with
        | None -> iterate graph (mostleft graph p0 None) (p0 :: envelope)
        | Some p ->
            if p = p0 then
                envelope
            else
                iterate graph (mostleft graph p None) (p :: envelope)
    in iterate graph None []

(* val quicksort_angles : graph -> point -> graph *)
let quicksort_angles (graph : graph) p0 : graph =
    let atan2point (Point (x, y)) (Point (x0, y0)) = atan2 (y -. y0) (x -. x0) in
    let graph_with_angles = List.map (fun point -> (point, atan2point point p0)) graph in
    let rec qsort l =
        match l with
        | [] -> []
        | (point, angle) :: r ->
            let l1 = List.filter (fun (_, a) -> a < angle) r in
            let l2 = List.filter (fun (_, a) -> a > angle) r in
            let compare_x (p1, a1) (p2, a2) = compare (x p1) (x p2) in
            let leq = List.sort compare_x (List.filter (fun (_, a) -> a = angle) l) in
            (qsort l1) @ leq @ (qsort l2)
    in List.map fst (qsort graph_with_angles)

(* val graham : graph -> point list *)
let graham graph =
    let rec iterate graph stack =
        match graph with
        | [] -> stack
        | point :: r -> begin
            match stack with
            | firstpoint :: secondpoint :: s ->
                if isleft point (Line(secondpoint, firstpoint)) then
                    iterate r (point :: stack)
                else
                    iterate graph (secondpoint :: s)
            | _ -> iterate r (point :: stack)
        end
    in
    let p0 = minimise_on_graph x graph in
    let ordered_graph = quicksort_angles (List.filter (fun point -> point <> p0) graph) p0 in
    match ordered_graph with
    | [] -> [p0]
    | firstpoint :: r -> iterate r [firstpoint; p0]