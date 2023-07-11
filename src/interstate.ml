open! Core

let rec tuples ~locations ~highway ~accumulator =
  match locations with
  | city_1 :: city_2 :: tail ->
    tuples
      ~highway
      ~locations:([ city_2 ] @ tail)
      ~accumulator:
        (accumulator
         @ [ city_1, city_2, highway ]
         @ [ city_2, city_1, highway ])
  | _ -> accumulator
;;

let of_file input_file =
  In_channel.read_lines (File_path.to_string input_file)
  |> List.concat_map ~f:(fun line ->
       let list = String.split line ~on:',' in
       let highway = List.hd_exn list in
       let cities = List.tl_exn list in
       tuples ~locations:cities ~highway ~accumulator:[])
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
        ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
