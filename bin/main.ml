let _xspf_header =
{|<?xml version="1.0" encoding="UTF-8"?>
<playlist xmlns="http://xspf.org/ns/0/" xmlns:vlc="http://www.videolan.org/vlc/playlist/ns/0/" version="1">
	<title>Playlist</title>
	<trackList>
|}

let traverse path =
  let queue = Queue.create () in
  let rec loop path =
    let typ = Unix.LargeFile.stat path in
    match typ.st_kind with
    | S_REG -> Queue.add path queue
    | S_DIR ->
      let () = 
        match Sys.readdir path with
        | children ->
          Array.fast_sort String.compare children;
          Array.iter (fun child -> loop (Printf.sprintf "%s/%s" path child)) children
        | exception (Sys_error e) -> Printf.eprintf "%s%!\n" e
      in ()
    | _ -> ()
  in
  loop path;
  queue

let rec dump_queue oc queue =
  match Queue.take_opt queue with
  | None -> ()
  | Some _path ->
    (* Printf.fprintf oc "%s\n%!" path; *)
    dump_queue oc queue

let xspfmaker oc =
  dump_queue oc (traverse "/etc")
  (* Out_channel.output_string oc xspf_header *)
  
let main outputfile =
  let outputfile = match outputfile with
    | "" -> stdout
    | outputfile -> open_out outputfile
  in
  xspfmaker outputfile;
  if outputfile <> stdout then
    close_out_noerr outputfile

let () =
  let open Cmdliner in
  let cmd =
    let outputfile =
      Arg.(value & opt string "" & info ["o" ; "output"]
             ~doc:"Output file, stdout if not specified.")
    in
    
    Cmd.v
      (Cmd.info "xspfmaker" ~doc:"xspfmaker whatever")
      Term.(const main $ outputfile)
  in
  exit @@ Cmd.eval cmd
