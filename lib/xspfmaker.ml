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
  title_fmt : title_fmt;
}

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

(* standard xml escaping *)
let output_escaped og =
  for i = 0 to (String.length og) - 1 do
    match String.get og i with
    | '<' -> print_string "&lt;"
    | '>' -> print_string "&gt;"
    | '&' -> print_string "&amp;"
    | '"' -> print_string "&quot;"
    | '\'' -> print_string "&apos;"
    | c -> print_char c
  done

let output_title fmt location =
  let title = match fmt with
    | Path -> location
    | Filename -> Filename.basename location
  in
  Printf.printf "\t\t\t<title>";
  output_escaped title;
  Printf.printf "</title>\n"

let output_track title_fmt location duration id =
  Printf.printf "\t\t<track>\n";
  Printf.printf "\t\t\t<location>file://";
  output_escaped location;
  Printf.printf "</location>\n";
  output_title title_fmt location;
  Printf.printf "\t\t\t<duration>%Lu</duration>\n" duration;
  Printf.printf "\t\t\t<extension application=\"http://www.videolan.org/vlc/playlist/0\">\n";
  Printf.printf "\t\t\t\t<vlc:id>%d</vlc:id>\n" id;
  Printf.printf "\t\t\t</extension>\n";
  Printf.printf "\t\t</track>\n";
  flush stdout

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
       output_track cf.title_fmt path duration id;
       Av.close handle;
       succ id
     | Some duration when duration <= 0L ->
       warn "%s: insane duration %Lu, using 10" path duration;
       output_track cf.title_fmt path duration id;
       Av.close handle;
       succ id
     | _ ->
       warn "%s: no duration, skipped" path;
       Av.close handle;
       id)

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

(* parse and traverse each path from stdin *)
let paths_of_stdin cf id : int =
  let rec loop id =
    match input_line stdin with
    | path -> traverse cf (path_without_slash path) id |> loop
    | exception End_of_file -> id
  in
  loop id

let xspfmaker title_fmt paths =
  let cf = { title_fmt } in
  output_header ();
  let paths = if paths = [] then ["-"] else paths in
  List.fold_left
    (fun id path ->
       let path = path_without_slash path in
       if path = "-" then
         paths_of_stdin cf id
       else
         traverse cf path id)
    0 paths |>
  output_footer
