let _xspf_header =
{|<?xml version="1.0" encoding="UTF-8"?>
<playlist xmlns="http://xspf.org/ns/0/" xmlns:vlc="http://www.videolan.org/vlc/playlist/ns/0/" version="1">
	<title>Playlist</title>
	<trackList>
|}

let consider_file path =
  match Av.open_input path with
  | exception (Avutil.Error e) ->
    Printf.eprintf "%s: error %s%!\n" path (Avutil.string_of_error e)
  | handle ->
    (match Av.get_input_duration handle with
     | Some duration ->
       Printf.printf "%s: duration %Lu\n%!" path duration
     | None ->
       Printf.eprintf "%s: NO DURATION !!!\n%!" path;
       Av.close handle)

let traverse path queue =
  let rec loop path =
    match (Unix.LargeFile.stat path).st_kind with
    | S_REG -> Queue.add path queue
    | S_DIR ->
      (match Sys.readdir path with
       | children ->
         Array.fast_sort String.compare children;
         Array.iter (fun child -> loop (Printf.sprintf "%s/%s" path child)) children
       | exception (Sys_error e) -> Printf.eprintf "%s%!\n" e)
    | _ -> ()
  in
  loop path

let rec dump_queue oc queue =
  match Queue.take_opt queue with
  | None -> ()
  | Some path ->
    consider_file path;
    (* Printf.fprintf oc "%s\n%!" path; *)
    dump_queue oc queue

let paths_of_filelist ic =
  let rec loop l =
    match input_line ic with
    | path -> loop @@ path :: l
    | exception End_of_file -> l
  in
  loop [] |> List.rev
  
let xspfmaker oc paths =
  let queue = Queue.create () in
  List.iter (fun path -> traverse path queue) paths;
  dump_queue oc queue

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
