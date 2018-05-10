# convex-envelope

---

An OCaml implementation of several well-known algorithms that determine a graph's convex envelope.

---

### Algorithms used
- Jarvis (infinite loop on some cases, I still haven't figured why)
- Graham
- Quickhull

### How to use

Just include the file in the OCaml toplevel with `#use "convex.ml"` after loading the Graphics module (`#load graphics.cma`). From this point you can use several functions.

Example :

```ocaml
(* In the ocaml toplevel *)
let g = randomgraph 10 8.;; (* Random graph : 10 points with coordinates in [-8;8]. You may also use a point list as (x,y) tuples and call graph_of_list to create the graph. *)
open_window g;; (* Opens a window big enough to draw the graph. *)
draw_graph g;; (* Draws the graph on the window. *)
draw_polygon (quickhull g) (* Draws the convex hull on the window, you can replace quickhull with jarvis or graham to change the algorithm used. *)
close_window();;

```