(* Types defining a 2D point, a line between 2 points and a graph, which is a list of points *)
type point = Point of (float * float)
type graph = point list
type line = Line of (point * point)

(* Functions to get the coordinates of a point *)
val x : point -> float
val y : point -> float

(* Creates a graph from a list of coordinates *)
val graph_of_list : (float * float) list -> graph

(* minimise_on_graph f g : Calls f on every point of g and returns the point that minimises f *)
val minimise_on_graph : (point -> 'a) -> graph -> point
(* maximise_on_graph f g : Calls f on every point of g and returns the point that maximises f *)
val maximise_on_graph : (point -> 'a) -> graph -> point

(* Opens a window big enough to contain all the points in the graph *)
val open_window : graph -> unit
(* Calls Graphics.close_graph() *)
val close_window : unit -> unit

(* Draws a red cross to represent the point *)
val draw_point : point -> unit
(* Plots each point of the graph *)
val draw_graph : graph -> unit
(* Draws lines between the points to make a polygon *)
val draw_polygon : point list -> unit

(* Returns the convex envelope of a graph, following the Jarvis algorithm *)
val jarvis : graph -> point list
(* Returns the convex envelope of a graph, following the Graham algorithm *)
val graham : graph -> point list