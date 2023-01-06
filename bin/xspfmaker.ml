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

let () =
  let open Cmdliner in
  let cmd =
    let title_fmt =
      let doc =
        "Specifies the format of the title of each track, $(docv) must be either $(b,path) \
         or $(b,filename). If $(b,path) the title is the complete file path. If $(b,filename) \
         just the actual filename is used as the title."
      and docv = "FMT" in
      let t = Arg.enum [("path", Path); ("filename", Filename)] in
      Arg.(value & opt t Filename & info ["t"; "title-format"] ~doc ~docv)
    in
    let paths =
      let doc =
        "List of $(docv) to be traversed. All files in the subtree will be considered for the playlist \
         by querying its duration via ffmpeg. Discarded files are printed out on stderr."
      and docv = "PATHS" in
      Arg.(value & pos_all string [] & info [] ~doc ~docv)
    in
    Cmd.v
      (Cmd.info "xspfmaker" ~doc:"Make xspf playlists")
      Term.(const xspfmaker $ title_fmt $ paths)
  in
  exit @@ Cmd.eval cmd
