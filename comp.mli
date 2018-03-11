(* Creates a graph from a list of coordinates *)
val graph_of_list : (float * float) list -> graph

(* Opens a window big enough to contain all the points in the graph *)
val open_window : graph -> unit
(* Calls Graphics.close_graph() *)
val close_window : unit -> unit

(* Plots each point of the graph *)
val draw_graph : graph -> unit

(* Returns the convex envelope of a graph, following the Jarvis algorithm *)
val jarvis : graph -> point list
(* Returns the convex envelope of a graph, following the Graham algorithm *)
val graham : graph -> point list
(* Returns the convex envelope of a graph, following the Quickhull algorithm *)
val quickhull : graph -> point list