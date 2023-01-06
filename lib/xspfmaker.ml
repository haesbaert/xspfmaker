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

type title_fmt = Path | Filename

type config = {
  input : in_channel;
  output: out_channel;
  title_fmt : title_fmt;
}

let output_header output =
  output_string output
{|<?xml version="1.0" encoding="UTF-8"?>
<playlist xmlns="http://xspf.org/ns/0/" xmlns:vlc="http://www.videolan.org/vlc/playlist/ns/0/" version="1">
	<title>Playlist</title>
	<trackList>
|}

let output_footer output num_ids =
  output_string output
{|	</trackList>
	<extension application="http://www.videolan.org/vlc/playlist/0">
|};
  for i = 0 to num_ids - 1 do
    Printf.fprintf output "\t\t<vlc:item tid=\"%d\"/>\n" i
  done;
  output_string output
{|	</extension>
</playlist>
|}

(* standard xml escaping *)
let output_escaped output og =
  for i = 0 to (String.length og) - 1 do
    match String.get og i with
    | '<' -> output_string output "&lt;"
    | '>' -> output_string output "&gt;"
    | '&' -> output_string output "&amp;"
    | '"' -> output_string output "&quot;"
    | '\'' -> output_string output "&apos;"
    | c -> output_char output c
  done

let output_title output fmt location =
  let title = match fmt with
    | Path -> location
    | Filename -> Filename.basename location
  in
  Printf.fprintf output "\t\t\t<title>";
  output_escaped output title;
  Printf.fprintf output "</title>\n"

let output_track output title_fmt location duration id =
  Printf.fprintf output "\t\t<track>\n";
  Printf.fprintf output "\t\t\t<location>file://";
  output_escaped output location;
  Printf.fprintf output "</location>\n";
  output_title output title_fmt location;
  Printf.fprintf output "\t\t\t<duration>%Lu</duration>\n" duration;
  Printf.fprintf output "\t\t\t<extension application=\"http://www.videolan.org/vlc/playlist/0\">\n";
  Printf.fprintf output "\t\t\t\t<vlc:id>%d</vlc:id>\n" id;
  Printf.fprintf output "\t\t\t</extension>\n";
  Printf.fprintf output "\t\t</track>\n";
  flush output

(* I still can't believe they made stderr buffered *)
let warn fmt = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let process_file cf path id =
  match Av.open_input path with
  | exception Avutil.Error e ->
    warn "%s: error %s" path (Avutil.string_of_error e);
    id
  | handle ->
    (match Av.get_input_duration handle with
     | Some duration when duration > 0L ->
       output_track cf.output cf.title_fmt path duration id;
       Av.close handle;
       succ id
     | Some duration when duration <= 0L ->
       warn "%s: insane duration %Lu, using 10" path duration;
       output_track cf.output cf.title_fmt path duration id;
       Av.close handle;
       succ id
     | _ ->
       warn "%s: no duration, skipped" path;
       Av.close handle;
       id)

let check_path path =
  if String.get path 0 <> '/' then
    invalid_arg @@ Printf.sprintf
      "\'%s\' is not a full path, must start with /" path

(* traverse path, bumping id for each valid file and returning next_id *)
let traverse cf path id : int =
  let rec loop path id : int =
    match (Unix.LargeFile.stat path).st_kind with
    | exception Unix.Unix_error (errno, _, _) ->
      warn "%s: %s" path (Unix.error_message errno); id
    | S_REG -> process_file cf path id
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
  check_path path;
  loop path id

(* maybe strip trailing / from path *)
let rec path_without_slash path =
  let last = pred @@ String.length path in
  if last = 0 then
    path
  else
    match String.rindex_opt path '/' with
    | None -> path
    | Some idx ->
      if idx = last then
        path_without_slash (String.sub path 0 last)
      else
        path

(* parse and traverse each path from input *)
let paths_of_input cf id : int =
  let rec loop id =
    match input_line cf.input with
    | path -> traverse cf (path_without_slash path) id |> loop
    | exception End_of_file -> id
  in
  loop id

let xspfmaker1 title_fmt input output paths () =
  let cf = { title_fmt; input; output } in
  output_header cf.output;
  let paths = if paths = [] then ["-"] else paths in
  List.fold_left
    (fun id path ->
       let path = path_without_slash path in
       if path = "-" then
         paths_of_input cf id
       else
         traverse cf path id)
    0 paths |>
  output_footer cf.output

let xspfmaker title_fmt input output paths =
  Fun.protect ~finally:flush_all @@ xspfmaker1 title_fmt input output paths
