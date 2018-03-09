type point = Point of (float * float)
type graph = point list
type line = Line of (point * point)

let x (Point (a, b)) = a
let y (Point (a, b)) = b

(* val graph_of_list : (float * float) list -> graph = <fun> *)
let rec graph_of_list pointlist : graph =
    match pointlist with
    | [] -> []
    | point :: r -> (Point point) :: graph_of_list r

(* val minimise_on_graph : (point -> 'a) -> graph -> point = <fun> *)
let minimise_on_graph f (graph : graph) =
    match graph with
    | [] -> failwith "minimise_on_graph - invalid argument : empty graph"
    | firstpoint :: r ->
        let rec minimise graph memorised_point =
            match graph with
            | [] -> memorised_point
            | point :: r -> if f point < f memorised_point then minimise r point else minimise r memorised_point
        in minimise r firstpoint

(* val maximise_on_graph : (point -> 'a) -> graph -> point = <fun> *)
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

(* val physical_x : float -> int = <fun> *)
let physical_x x = !originx + int_of_float (x *. float_of_int !ssize)

(* val physical_y : float -> int = <fun> *)
let physical_y y = !originy + int_of_float (y *. float_of_int !ssize)

(* val create_window : int -> int -> int -> int -> unit = <fun> *)
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

(* val open_window : graph -> unit = <fun> *)
let open_window graph =
    let minx = int_of_float (ceil (x (minimise_on_graph x graph))) - 1 in
    let miny = int_of_float (ceil (y (minimise_on_graph y graph))) - 1 in
    let maxx = int_of_float (x (maximise_on_graph x graph)) + 1 in
    let maxy = int_of_float (y (maximise_on_graph y graph)) + 1 in
    create_window minx maxx miny maxy

(* val close_window : unit -> unit = <fun> *)
let close_window () = Graphics.close_graph()

(* val draw_point : point -> unit = <fun> *)
let draw_point (Point (x, y)) =
    Graphics.set_line_width 2;
    let delta = int_of_float (float_of_int !ssize /. 10.0) in
    let rx = physical_x x and ry = physical_y y in
    Graphics.moveto (rx - delta) (ry + delta);
    Graphics.lineto (rx + delta) (ry - delta);
    Graphics.moveto (rx - delta) (ry - delta);
    Graphics.lineto (rx + delta) (ry + delta)

(* val draw_graph : graph -> unit = <fun> *)
let draw_graph (graph : graph) =
    Graphics.set_color (Graphics.red);
    let rec draw_points pointlist =
        match pointlist with
        | [] -> ()
        | point :: r ->
            draw_point point;
            draw_points r
    in draw_points graph

(* val draw_polygon : point list -> unit = <fun> *)
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

(* TO REMOVE OR EDIT *)
(* val above : point -> line -> bool = <fun> *)
let above p (Line(p1, p2))=
    let x1, x2, y1, y2 = x p1, x p2, y p1, y p2 in
    let a = (y2 -. y1) /. (x2 -. x1) in
    let b = y2 -. a *. x2 in
    a *. x p +. b < y p

(* TO REMOVE OR EDIT *)
(* val isleft2 : point -> line -> bool = <fun> *)
let isleft2 p (Line(p1, p2)) =
    if p = p1 || p = p2 then false else
    let x1, x2 = x p1, x p2 in
    if x1 < x2 then above p (Line(p1, p2))
    else not (above p (Line(p1, p2)))

(* TO REMOVE OR EDIT *)
(* val mostleft : point list -> point -> point option -> point option = <fun> *)
let rec mostleft graph point mostleftpoint =
    match graph with
    | [] -> mostleftpoint
    | p::r -> begin
        match mostleftpoint with
        | None -> mostleft r point (Some p)
        | Some mlp ->
            if p <> mlp && isleft2 p (Line(point, mlp)) then
                mostleft r point (Some p)
            else
                mostleft r point (Some mlp)
    end

(* TO EDIT *)
(* val jarvis : graph -> point list = <fun> *)
let jarvis graph =
    let p0 = minimise_on_graph x graph in
    let p = ref p0 in
    let env = ref [] in
    try
        while true do
            env := !p :: !env;
            match mostleft graph !p None with
            | None -> failwith "mostleft - None result";
            | Some mlp -> p := mlp;
            if !p = p0 then raise Exit
        done;
        []
    with Exit -> !env

(* val isleft : point -> line -> bool = <fun> *)
let isleft p (Line(p1, p2)) =
    let x1, x2, x3 = x p1, x p2, x p in
    let y1, y2, y3 = y p1, y p2, y p in
    (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) > 0.

(* val atan2point : point -> point -> float = <fun> *)
let atan2point (Point (x, y)) (Point (x0, y0)) = atan2 (y -. y0) (x -. x0)

(* val quicksort_angles : graph -> point -> graph = <fun> *)
let quicksort_angles (graph : graph) p0 : graph =
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

(* val graham : graph -> point list = <fun> *)
let graham graph =
    let p0 = minimise_on_graph x graph in
    let ordered_graph = List.filter (fun point -> point <> p0) (quicksort_angles graph p0) in
    let rec grahamrec p1 p2 graph envelope =
        match graph with
        | [] -> begin
            match p2 with
            | None -> []
            | Some point -> point :: p1 :: envelope
        end
        | p3 :: r -> begin
            match p2 with
            | None -> grahamrec p1 (Some p3) r envelope
            | Some point ->
                if isleft p3 (Line(p1, point)) then
                    grahamrec point (Some p3) r (p1 :: envelope)
                else
                    grahamrec p1 (Some p3) r envelope
        end
    in grahamrec p0 None ordered_graph []

let g1 = graph_of_list [(0.,2.);(1.,5.);(-4.,1.);(-1.,3.);(2.,4.);(-2.,4.);(0.5,3.5);(-1.,-2.2)]
let g2 = graph_of_list [(3.5,-6.1);(6.,-4.);(-4.,0.);(-1.5,3.5);(2.5,-6.1);(-2.3,4.5);(0.1,0.7);(-1.5,-4.9)]
let g3 = graph_of_list [(0.,0.);(1.,1.);(3.,3.);(-1.3,1.);(0.2,6.);(0.3,-4.);(0.6,-4.);(3.,-0.9)]

let () =
    open_window g1;
    draw_graph g1;
    draw_polygon (graham g1);
    read_line();
    ()