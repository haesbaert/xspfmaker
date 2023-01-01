(*
 * Copyright (c) 2023 Christiano Haesbaert <haesbaert@haesbaert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let output_header () =
  output_string stdout
{|<?xml version="1.0" encoding="UTF-8"?>
<playlist xmlns="http://xspf.org/ns/0/" xmlns:vlc="http://www.videolan.org/vlc/playlist/ns/0/" version="1">
	<title>Playlist</title>
	<trackList>
|}

let output_footer num_ids =
  output_string stdout
{|	</trackList>
	<extension application="http://www.videolan.org/vlc/playlist/0">
|};
  for i = 0 to num_ids - 1 do
    Printf.printf "\t\t<vlc:item tid=\"%d\"/>\n" i
  done;
  output_string stdout
{|	</extension>
</playlist>
 |}

let _xml_escape s = s

let output_track location duration id =
  Printf.printf "\t\t<track>\n";
  Printf.printf "\t\t\t<location>file://%s</location>\n" location;
  Printf.printf "\t\t\t<duration>%Lu</duration>\n" duration;
  Printf.printf "\t\t\t<extension application=\"http://www.videolan.org/vlc/playlist/0\">\n";
  Printf.printf "\t\t\t\t<vlc:id>%d</vlc:id>\n" id;
  Printf.printf "\t\t\t</extension>\n";
  Printf.printf "\t\t</track>\n";
  flush stdout

(* I still can't believe they made stderr buffered *)
let warn fmt = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let process_file path id =
  match Av.open_input path with
  | exception Avutil.Error e ->
    warn "%s: error %s" path (Avutil.string_of_error e);
    id
  | handle ->
    (match Av.get_input_duration handle with
     | Some duration when duration > 0L ->
       output_track path duration id;
       Av.close handle;
       succ id
     | Some duration when duration <= 0L ->
       warn "%s: insane duration %Lu, using 10" path duration;
       output_track path duration id;
       Av.close handle;
       succ id
     | _ ->
       warn "%s: no duration, skipped" path;
       Av.close handle;
       id)

let traverse path id =
  let rec loop path id : int =
    match (Unix.LargeFile.stat path).st_kind with
    | exception Unix.Unix_error (errno, _, _) ->
      warn "%s: %s" path (Unix.error_message errno); id
    | S_REG -> process_file path id
    | S_DIR ->
      (match Sys.readdir path with
       | children ->
         Array.fast_sort String.compare children;
         Array.fold_left
           (fun id child ->
              (loop [@tailcall]) (Printf.sprintf "%s/%s" path child) id)
           id children
       | exception (Sys_error e) -> warn "%s" e; id)
    | _ -> id
  in
  loop path id

let paths_of_stdin id =
  let rec loop id =
    match input_line stdin with
    | path -> traverse path id |> loop
    | exception End_of_file -> id
  in
  loop id
  
let main paths =
  let paths = if paths = [] then ["-"] else paths in
  output_header ();
  List.fold_left
    (fun id path ->
       if path = "-" then
         paths_of_stdin id
       else
         traverse path id)
    0 paths
  |>
  output_footer

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
