let _xspf_header =
{|<?xml version="1.0" encoding="UTF-8"?>
<playlist xmlns="http://xspf.org/ns/0/" xmlns:vlc="http://www.videolan.org/vlc/playlist/ns/0/" version="1">
	<title>Playlist</title>
	<trackList>
|}

(* I still can't believe they made stderr buffered *)
let warn fmt =
  Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let process_file path =
  match Av.open_input path with
  | exception (Avutil.Error e) ->
    warn "%s: error %s" path (Avutil.string_of_error e)
  | handle ->
    (match Av.get_input_duration handle with
     | Some duration when duration > 0L ->
       Printf.printf "%s: duration %Lu\n%!" path duration
     | Some duration when duration <= 0L ->
       warn "%s: insane duration %Lu, using 10" path duration
     | _ ->
       warn "%s: NO DURATION !!!" path;
       Av.close handle)

let traverse path =
  let rec loop path =
    match (Unix.LargeFile.stat path).st_kind with
    | exception Unix.Unix_error (errno, _, _) ->
      warn "%s: %s" path (Unix.error_message errno)
    | S_REG -> process_file path
    | S_DIR ->
      (match Sys.readdir path with
       | children ->
         Array.fast_sort String.compare children;
         Array.iter (fun child ->
             (loop [@tailcall]) (Printf.sprintf "%s/%s" path child)) children
       | exception (Sys_error e) -> warn "%s" e)
    | _ -> ()
  in
  loop path

let paths_of_stdin () =
  let rec loop () =
    match input_line stdin with
    | path -> traverse path; loop ()
    | exception End_of_file -> ()
  in
  loop ()
  
let main paths =
  let paths = if paths = [] then ["-"] else paths in
  List.iter
    (function
      | "-" -> paths_of_stdin ()
      | path -> traverse path)
    paths

let () =
  let open Cmdliner in
  let cmd =
    let paths =
      Arg.(value & pos_all string [] & info []
             ~doc:"Sei la caraleo.")
    in
    Cmd.v
      (Cmd.info "xspfmaker" ~doc:"xspfmaker whatever")
      Term.(const main $ paths)
  in
  exit @@ Cmd.eval cmd
