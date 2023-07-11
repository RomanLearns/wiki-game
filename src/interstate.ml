open! Core
module Location = String
(* We separate out the [Network] module to represent our highway connections in OCaml types. *)
module Network = struct
  (* We can represent our interstate as a set of connections, where a connection
     represents a road between two places. *)
  module Connection = struct
    module T = struct
      type t = Location.t * Location.t [@@deriving compare, sexp]
    end

  type t = Connection.Set.t [@@deriving sexp_of]

let of_file input_file =
  let connections =
    In_channel.read_lines (File_path.to_string input_file)
    |> List.concat_map ~f:(fun s ->
      match Connection.of_string s with
      | Some (a, b) ->
        (* Friendships are mutual; a connection between a and b means we should also
           consider the connection between b and a. *)
        [ a, b; b, a ]
      | None ->
        printf "ERROR: Could not parse line as connection; dropping. %s\n" s;
        [])
  in
  Connection.Set.of_list connections
;;

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing the highway \
       network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file listing all interstates and the cities they go through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
