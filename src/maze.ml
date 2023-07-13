open! Core

module Maze_element = struct
  type t =
    | Wall
    | Path
    | Start
    | End
end

(* what we need to do: read the file and turn it into a graph, traverse the
   graph starting from "S" and ending at "E", and then output the first
   solution that is found by our DFS algorithm *)

(* how to read the file and turn it into a graph: read each line, and then
   construct a list of lists using an adjacency matrix of 1s and 0s to
   represent walls and open spaces *)

(* read a file and create a list of lists of strings *)

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let of_file input_file =
  In_channel.read_lines (File_path.to_string input_file)
  |> List.foldi ~init:Position.Map.empty ~f:(fun y map line ->
       String.to_list line
       |> List.foldi ~init:map ~f:(fun x map value ->
            Map.add_exn
              map
              ~key:(x, y)
              ~data:
                (match value with
                 | '#' -> Maze_element.Wall
                 | '.' -> Path
                 | 'S' -> Start
                 | 'E' -> End
                 | _ -> raise_s [%message "invalid character" (value : char)])))
;;

let valid_neighbors ~map ~current_position =
  let x, y = current_position in
  let offsets = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ] in
  List.filter offsets ~f:(fun position ->
    match Map.find map position with
    | Some value ->
      (match value with
       | Maze_element.Wall | Start -> false
       | Path | End -> true)
    | None -> false)
;;

let rec dfs ~maze ~start ~path =
  match Map.find maze start with
  | Some Maze_element.End -> path @ [ start ]
  | _ ->
    let neighbors =
      valid_neighbors ~map:maze ~current_position:start
      |> List.filter ~f:(fun position ->
           not (List.mem path position ~equal:Position.equal))
    in
    if List.length neighbors = 0
    then []
    else (
      let paths =
        List.filter
          (List.map neighbors ~f:(fun pos ->
             dfs ~maze ~start:pos ~path @ [ start ]))
          ~f:(fun possible_path -> List.length possible_path > 0)
      in
      if List.length paths = 0 then [] else List.hd_exn paths)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let matrix = of_file input_file in
        let start =
          Map.filter matrix ~f:(fun value ->
            match value with Maze_element.Start -> true | _ -> false)
          |> Map.keys
          |> List.hd_exn
        in
        let result = dfs ~maze:matrix ~start ~path:[] in
        print_s [%message "" (result : Position.T.t list)]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
