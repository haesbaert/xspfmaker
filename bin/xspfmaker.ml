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

open Xspfmaker

let preprocess title_fmt input output paths =
  let input = match input with
    | None -> stdin
    | Some file -> open_in file
  in
  let output = match output with
    | None -> stdout
    | Some file -> open_out file
  in
  xspfmaker title_fmt input output paths

let () =
  let open Cmdliner in
  let cmd =
    let examples =
      let esc = Manpage.escape in
      [ `S Manpage.s_examples;
        `P "Create a xspf list from two directories a single file:"; `Noblank;
        `P  ("$ xspfmaker ~/videos ~/nudes.mp4 /mnt/oldvids > playlist.xspf" |> esc);
        `P "or the equivalent:"; `Noblank;
        `P ("$ xspfmaker -o playlist.xspf ~/videos ~/nudes.mp4 /mnt/oldvids" |> esc);
        `P "or the equivalent getting the list from stdin:"; `Noblank;
        `P ({|$ printf "~/videos\n~/nudes.mp4\n/mnt/oldvids" | xspfmaker -o playlist.xspf|} |> esc);
        `P "or the equivalent but parsing the list from a file:"; `Noblank;
        `P ({|$ printf "~/videos\n~/nudes.mp4\n/mnt/oldvids" > list &&|} |> esc); `Noblank;
        `P ("xspfmaker -o playlist.xspf -i list" |> esc);
      ]
    in
    let doc = "make xspf playlists" in
    let man = [
      `S Manpage.s_description;
      `P "$(tname) creates xspf playlists of media files to be used in players like $(b,vlc)(1). \
          Each path in $(i,PATHS) is traversed and scanned for suitable files. A list of \
          $(i,PATHS) as one per line can also be read from stdin by not passing it any \
          $(i,PATHS) or passing it a -. See the $(b,EXAMPLES) section.";
      `S Manpage.s_exit_status;
      `P "$(tname) exits 0 on success, and > 0 if an error occurs.";
      `Blocks examples;
      `S Manpage.s_bugs;
      `P "https://github.com/haesbaert/xspfmaker/issues";
      `S Manpage.s_see_also;
      `P "$(b,find)(1), $(b,vlc)(1)";
      `S Manpage.s_authors;
      `P "Christiano Haesbaert <haesbaert@haesbaert.org>";
    ]
    in
    let title_fmt =
      let doc =
        "Specifies the format of the title of each track, $(docv) must be either $(b,path) \
         or $(b,filename). If $(b,path) the title is the complete file path. If $(b,filename) \
         just the actual filename is used as the title. If empty or $(b,-), read a list of one \
         path per line from stdin."
      and docv = "FMT" in
      let t = Arg.enum [("path", Path); ("filename", Filename)] in
      Arg.(value & opt t Filename & info ["t"; "title-format"] ~doc ~docv)
    in
    let input =
      let doc = "Input file with a list of one path per line to be to be used in addition \
                 to $(i,PATHS)."
      and docv = "INPUTFILE" in
      Arg.(value & opt (some string) None & info ["i"; "input-file"] ~doc ~docv)
    in
    let output =
      let doc = "Output file to be used instead of stdout."
      and docv = "OUTPUTFILE" in
      Arg.(value & opt (some string) None & info ["o"; "output-file"] ~doc ~docv)
    in
    let paths =
      let doc =
        "List of $(docv) to be traversed. All files in the subtree will be considered for the playlist \
         by querying its duration via ffmpeg. Discarded files are printed out on stderr."
      and docv = "PATHS" in
      Arg.(value & pos_all string [] & info [] ~doc ~docv)
    in
    Cmd.v
      (Cmd.info "xspfmaker" ~doc ~man ~exits:[])
      Term.(const preprocess $ title_fmt $ input $ output $ paths)
  in
  exit @@ Cmd.eval cmd
