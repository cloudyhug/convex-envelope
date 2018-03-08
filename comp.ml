type point = Point of float * float
type graph = point list
type line = Line of point * point

let x (Point (a,b)) = a
let y (Point (a,b)) = b

let rec graph_of_list l : graph = match l with
    | [] -> []
    | p::r -> (Point p)::graph_of_list r

let minimise_on_graph f (g : graph) = match g with
    | [] -> failwith "minimise_on_graph - invalid argument : empty graph"
    | fp::rp ->
        let rec aux gr pacc = match gr with
            | [] -> pacc
            | p::r -> if f p < f pacc then aux r p else aux r pacc
        in aux rp fp

let maximise_on_graph f (g : graph) = match g with
    | [] -> failwith "maximise_on_graph - invalid argument : empty graph"
    | fp::rp ->
        let rec aux gr pacc = match gr with
            | [] -> pacc
            | p::r -> if f p > f pacc then aux r p else aux r pacc
        in aux rp fp

let ox = ref 0 and oy = ref 0 and ssize = ref 40

let real_x x = !ox + int_of_float (x *. float_of_int !ssize)
let real_y y = !oy + int_of_float (y *. float_of_int !ssize)

let create_window minx maxx miny maxy =
    let w = (maxx - minx) * !ssize and h = (maxy - miny) * !ssize in
    Graphics.open_graph (Printf.sprintf " %dx%d" w h);
    ox := (-minx) * !ssize;
    oy := (-miny) * !ssize;
    Graphics.set_color (Graphics.rgb 180 180 180);
    for x = 0 to maxx - minx do
        let a = x * !ssize in
        Graphics.moveto a 0; Graphics.lineto a h
    done;
    for y = 0 to maxy - miny do
        let b = y * !ssize in
        Graphics.moveto 0 b; Graphics.lineto w b
    done;
    Graphics.set_color (Graphics.black);
    Graphics.moveto !ox 0; Graphics.lineto !ox h;
    Graphics.moveto 0 !oy; Graphics.lineto w !oy

let open_window g =
    let minx = int_of_float (ceil (x (minimise_on_graph x g))) - 1 in
    let miny = int_of_float (ceil (y (minimise_on_graph y g))) - 1 in
    let maxx = int_of_float (x (maximise_on_graph x g)) + 1 in
    let maxy = int_of_float (y (maximise_on_graph y g)) + 1 in
    create_window minx maxx miny maxy

let close_window () = Graphics.close_graph()

let draw_point (Point(x,y)) =
    Graphics.set_line_width 2;
    let delta = int_of_float (float_of_int !ssize /. 10.0) in
    let rx = real_x x and ry = real_y y in
    Graphics.moveto (rx - delta) (ry + delta);
    Graphics.lineto (rx + delta) (ry - delta);
    Graphics.moveto (rx - delta) (ry - delta);
    Graphics.lineto (rx + delta) (ry + delta)

let draw_graph g =
    Graphics.set_color (Graphics.red);
    let rec draw_graph_aux gr =
        match gr with
        | [] -> ()
        | p::r ->
            draw_point p;
            draw_graph_aux r
    in
    draw_graph_aux g;
    Graphics.set_line_width 1

let draw_polygon pl =
    match pl with
    | [] -> ()
    | p1::r ->
        let fp = p1 in
        let rec polyrec plist =
            match plist with
            | [] -> () (* never happens *)
            | (Point (a,b))::r ->
                Graphics.lineto (real_x a) (real_y b);
                polyrec r
        in
        Graphics.moveto (real_x (x fp)) (real_y (y fp));
        polyrec r;
        Graphics.lineto (real_x (x fp)) (real_y (y fp))

let above p (Line(p1, p2))=
    let x1, x2, y1, y2 = x p1, x p2, y p1, y p2 in
    let a = (y2 -. y1) /. (x2 -. x1) in
    let b = y2 -. a *. x2 in
    a *. x p +. b < y p

let isleft2 p (Line(p1, p2)) =
    if p = p1 || p = p2 then false else
    let x1, x2 = x p1, x p2 in
    if x1 < x2 then above p (Line(p1, p2))
    else not (above p (Line(p1, p2)))

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

let jarvis g =
    let p0 = minimise_on_graph x g in
    let p = ref p0 in
    let l = ref [] in
    try
        while true do
            l := !p :: !l;
            let m = mostleft g !p None in
            match m with
            | None -> failwith "mostleft - None result";
            | Some mlp -> p := mlp;
            if !p = p0 then raise Exit
        done;
        []
    with Exit -> !l

let isleft p (Line(p1, p2)) =
    let x1, x2, x3 = x p1, x p2, x p in
    let y1, y2, y3 = y p1, y p2, y p in
    (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) > 0.

let atan2point (Point (x, y)) (Point (x0, y0)) = atan2 (y -. y0) (x -. x0)

let quicksort_isleft g p0 =
    let gwa = List.map (fun p -> (p, atan2point p p0)) g in
    let rec qsort gr =
        match gr with
        | [] -> []
        | (p, a) :: r ->
            let g1 = List.filter (fun (point, angle) -> angle < a) r in
            let g2 = List.filter (fun (point, angle) -> angle > a) r in
            let cmp_x a b = compare (x (fst a)) (x (fst b)) in
            let geq = List.sort cmp_x (List.filter (fun (point, angle) -> angle = a) gr) in
            (qsort g1) @ geq @ (qsort g2)
    in List.map fst (qsort gwa)

let graham g =
    let p0 = minimise_on_graph x g in
    let g_ord = List.filter (fun p -> p <> p0) (quicksort_isleft g p0) in
    let rec grahamrec p1 p2 gr env =
        match gr with
        | [] -> begin
            match p2 with
            | None -> failwith "graham - problem"
            | Some p -> p :: p1 :: env
        end
        | p3 :: r -> begin
            match p2 with
            | None -> grahamrec p1 (Some p3) r env
            | Some p ->
                if isleft p3 (Line(p1, p)) then
                    grahamrec p (Some p3) r (p1 :: env)
                else
                    grahamrec p1 (Some p3) r env
        end
    in grahamrec p0 None g_ord []

let g1 = graph_of_list [(0.,2.);(1.,5.);(-4.,1.);(-1.,3.);(2.,4.);(-2.,4.);(0.5,3.5);(-1.,-2.2)]
let g2 = graph_of_list [(3.5,-6.1);(6.,-4.);(-4.,0.);(-1.5,3.5);(2.5,-6.1);(-2.3,4.5);(0.1,0.7);(-1.5,-4.9)]
let g3 = graph_of_list [(0.,0.);(1.,1.);(3.,3.);(-1.3,1.);(0.2,6.);(0.3,-4.);(0.6,-4.);(3.,-0.9)]