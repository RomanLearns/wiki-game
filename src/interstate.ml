open! Core

let rec tuples ~locations ~highway ~accumulator =
  match locations with
  | city_1 :: city_2 :: tail ->
    tuples
      ~highway
      ~locations:([ city_2 ] @ tail)
      ~accumulator:(accumulator @ [ city_1, city_2, highway ])
  | _ -> accumulator
;;

let of_file input_file =
  In_channel.read_lines (File_path.to_string input_file)
  |> List.concat_map ~f:(fun line ->
       let new_line =
         String.substr_replace_all line ~pattern:"." ~with_:""
       in
       let list = String.split new_line ~on:',' in
       let highway = List.hd_exn list in
       let cities = List.tl_exn list in
       tuples ~locations:cities ~highway ~accumulator:[])
;;

module MyString = struct
  include String

  let default = ""
end

(* In order to visualize the social network, we use the ocamlgraph library to
   create a [Graph] structure whose vertices are of type [Person.t].

   The ocamlgraph library exposes lots of different ways to construct
   different types of graphs. Take a look at
   https://github.com/backtracking/ocamlgraph/blob/master/src/imperative.mli
   for documentation on other types of graphs exposed by this API. *)
module G = Graph.Imperative.Graph.ConcreteLabeled (String) (MyString)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes e = [ `Dir `Both; `Label (G.E.label e) ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = of_file input_file in
        print_s [%sexp (network : (string * string * string) list)]]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = of_file input_file in
        let graph = G.create () in
        List.iter network ~f:(fun (city1, city2, highway) ->
          let new_edge = G.E.create city1 highway city2 in
          G.add_edge_e graph new_edge);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}/n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
