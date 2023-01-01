let _xspf_header =
{|<?xml version="1.0" encoding="UTF-8"?>
<playlist xmlns="http://xspf.org/ns/0/" xmlns:vlc="http://www.videolan.org/vlc/playlist/ns/0/" version="1">
	<title>Playlist</title>
	<trackList>
|}

(* I still can't believe they made stderr buffered *)
let warn fmt =
  Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let process_file oc path =
  match Av.open_input path with
  | exception (Avutil.Error e) ->
    warn "%s: error %s" path (Avutil.string_of_error e)
  | handle ->
    (match Av.get_input_duration handle with
     | Some duration when duration > 0L ->
       Printf.fprintf oc "%s: duration %Lu\n%!" path duration
     | Some duration when duration <= 0L ->
       warn "%s: insane duration %Lu, using 10" path duration
     | _ ->
       warn "%s: NO DURATION !!!" path;
       Av.close handle)

let traverse oc path =
  let rec loop path =
    match (Unix.LargeFile.stat path).st_kind with
    | exception Unix.Unix_error (errno, _, _) ->
      warn "%s: %s" path (Unix.error_message errno)
    | S_REG -> process_file oc path
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

let paths_of_filelist ic =
  let rec loop l =
    match input_line ic with
    | path -> loop @@ path :: l
    | exception End_of_file -> l
  in
  loop [] |> List.rev
  
let xspfmaker oc paths =
  List.iter (fun path -> traverse oc path) paths

let main outputfile paths =
  let outputfile = match outputfile with
    | "" -> stdout
    | outputfile -> open_out outputfile
  in
  let paths = match paths with
    | "-" :: [] | [] -> paths_of_filelist stdin
    | paths -> paths
  in
  xspfmaker outputfile paths;
  if outputfile <> stdout then
    close_out_noerr outputfile

let () =
  let open Cmdliner in
  let cmd =
    let outputfile =
      Arg.(value & opt string "" & info ["o" ; "output"]
             ~doc:"Output file, stdout if not specified.")
    in
    let paths =
      Arg.(value & pos_all string [] & info []
             ~doc:"Sei la caraleo.")
    in
    Cmd.v
      (Cmd.info "xspfmaker" ~doc:"xspfmaker whatever")
      Term.(const main $ outputfile $ paths)
  in
  exit @@ Cmd.eval cmd
