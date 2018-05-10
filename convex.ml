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

let randomgraph n limit =
    Random.self_init();
    let f () = Random.float (2. *. limit) -. limit in
    Array.init n (fun _ -> (f(), f()))
    |> Array.to_list
    |> graph_of_list

(* val minimise_on_graph : (point -> 'a) -> graph -> point
 * Finds the point in the graph that minimises the function.
 *)
let minimise_on_graph f (graph : graph) =
    match graph with
    | [] -> failwith "minimise_on_graph - invalid argument : empty graph"
    | firstpoint :: r ->
        let rec minimise graph memorised_point =
            match graph with
            | [] -> memorised_point
            | point :: r -> if f point < f memorised_point then minimise r point else minimise r memorised_point
        in minimise r firstpoint

(* val maximise_on_graph : (point -> 'a) -> graph -> point
 * Finds the point in the graph that maximises the function.
 *)
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

(* val open_window : graph -> unit
 * Creates a window that is big enough to display the graph.
 *)
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

(* val isleft : point -> line -> bool
 * Tells whether a point is on the left of a line.
 *)
let isleft p (Line(p1, p2)) =
    if p = p1 || p = p2 then false else
    let x1, x2, x3 = x p1, x p2, x p in
    let y1, y2, y3 = y p1, y p2, y p in
    (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) > 0.

(* val mostleft : point list -> point -> point option -> point option
 * (Auxiliary function)
 * Iterates on the point list and finds the farthest point on the left of the point given as a parameter.
 *)
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

(* val jarvis : graph -> point list
 * Returns the graph's convex envelope, computed with the Jarvis algorithm.
 *)
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

(* val quicksort_angles : graph -> point -> graph
 * Sorts the graph depending on the angles the points make with the horizontal axis.
 * The parameter point p0 is considered as the origin for the computation.
 *)
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

(* val graham : graph -> point list
 * Returns the graph's convex envelope, computed with the Jarvis algorithm.
 *)
let graham graph =
    let rec iterate graph stack =
        match graph with
        | [] -> stack
        | point :: r -> begin
            match stack with
            | firstpoint :: secondpoint :: s ->
                if isleft point (Line (secondpoint, firstpoint)) then
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

(* val insert_after : 'a -> 'a -> 'a list -> 'a list
 * Inserts b after a in list l.
 *)
let rec insert_after a b l =
    match l with
    | [] -> failwith "insert_after - empty list"
    | e :: r ->
        if e = a then
            e :: b :: r
        else
            e :: (insert_after a b r)

(* val equation : point -> point -> float * float * float
 * Returns the (a,b,c) parameters of a 2-dimensional 'ax+by+c=0' line that contains both points.
 *)
let equation (Point (x1, y1)) (Point (x2, y2)) =
    let a = y2 -. y1 in
    let b = x1 -. x2 in
    let c = -. a *. x1 -. b *. y1 in
    (a, b, c)

(* val farthestpoint : graph -> point -> point -> point
 * Finds the point in the graph that is the farthest one from the (a,b) line.
 *)
let farthestpoint graph a b =
    let distance (Line (p1, p2)) (Point (x, y)) =
        let a, b, c = equation p1 p2 in
        (abs_float (a *. x +. b *. y +. c)) /. (sqrt (a *. a +. b *. b))
    in maximise_on_graph (distance (Line (a, b))) graph

(* val quickhull : graph -> point list
 * Returns the graph's convex envelope, computed with the Quickhull algorithm.
 *)
let quickhull graph =
    let rec qhull graph a b envelope =
        let g = List.filter (fun point -> point <> a && point <> b) graph in
        if g = [] then () else
        let s = farthestpoint g a b in
        envelope := insert_after a s !envelope;
        let g1 = List.filter (fun point -> isleft point (Line (a, s))) g in
        let g2 = List.filter (fun point -> isleft point (Line (s, b))) g in
        qhull g1 a s envelope;
        qhull g2 s b envelope
    in
    let p1 = minimise_on_graph x graph in
    let p2 = maximise_on_graph x graph in
    if p1 = p2 then [p1] else
    let g = List.filter (fun point -> point <> p1 && point <> p2) graph in
    let g1, g2 = List.partition (fun point -> isleft point (Line (p1, p2))) g in
    let env = ref [p1; p2] in
    qhull g1 p1 p2 env;
    qhull g2 p2 p1 env;
    !env

(*

Jarvis makes an infinite loop on this...

[(-4.73,-4.92);(2.58,1.77);(-3.17,-3.02);(-4.86,-0.70);(1.86,-4.95);(3.45,1.84);(1.33,4.56);(1.82,1.99);(-1.87,-0.11);(-3.62,1.80);(2.52,-2.80);(1.97,4.37);(-3.38,1.11);(4.33,2.51);(2.56,-4.48);(-4.41,-4.05);(2.72,2.83);(4.81,3.18);(-2.52,4.54);(4.60,4.95);(1.79,4.35);(3.91,3.05);(0.04,-1.87);(3.02,0.72);(-2.48,3.17);(-4.63,4.40);(3.77,-4.96);(1.15,2.33);(-0.16,-4.61);(-4.09,0.48);(1.43,-2.60);(-4.76,-3.50);(4.68,-0.85);(-1.31,-4.96);(4.99,-2.84);(-3.50,-1.71);(-3.66,1.78);(4.91,-4.59);(1.13,1.37);(-0.46,2.93);(2.33,3.66);(3.70,-0.09);(-2.76,1.10);(1.49,-0.15);(-4.66,2.08);(4.49,2.39);(3.89,1.77);(-4.08,4.72);(4.77,3.57);(-4.27,-0.39);(2.90,4.76);(1.54,-2.77);(-2.36,4.24);(0.61,1.45);(-0.62,3.78);(2.43,-4.85);(0.0,0.0)]

*)