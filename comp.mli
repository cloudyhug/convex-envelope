type point = Point of (float * float)
type graph = point list
type line = Line of (point * point)
val x : point -> float
val y : point -> float
val graph_of_list : (float * float) list -> graph
val minimise_on_graph : (point -> 'a) -> graph -> point
val maximise_on_graph : (point -> 'a) -> graph -> point
val open_window : graph -> unit
val close_window : unit -> unit
val draw_point : point -> unit
val draw_graph : graph -> unit
val draw_polygon : point list -> unit
val jarvis : graph -> point list
val graham : graph -> point list