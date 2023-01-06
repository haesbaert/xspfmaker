let expected_filename =
  Printf.sprintf
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<playlist xmlns=\"http://xspf.org/ns/0/\" xmlns:vlc=\"http://www.videolan.org/vlc/playlist/ns/0/\" version=\"1\">
	<title>Playlist</title>
	<trackList>
		<track>
			<location>file://%s/data/company.gif</location>
			<title>company.gif</title>
			<duration>17</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>0</vlc:id>
			</extension>
		</track>
		<track>
			<location>file://%s/data/escape_&apos;&quot;&lt;&gt;_escape</location>
			<title>escape_&apos;&quot;&lt;&gt;_escape</title>
			<duration>11</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>1</vlc:id>
			</extension>
		</track>
		<track>
			<location>file://%s/data/rl.gif</location>
			<title>rl.gif</title>
			<duration>47</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>2</vlc:id>
			</extension>
		</track>
		<track>
			<location>file://%s/data/utop_next_word.gif</location>
			<title>utop_next_word.gif</title>
			<duration>11</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>3</vlc:id>
			</extension>
		</track>
	</trackList>
	<extension application=\"http://www.videolan.org/vlc/playlist/0\">
		<vlc:item tid=\"0\"/>
		<vlc:item tid=\"1\"/>
		<vlc:item tid=\"2\"/>
		<vlc:item tid=\"3\"/>
	</extension>
</playlist>
" (Sys.getcwd ()) (Sys.getcwd ()) (Sys.getcwd ()) (Sys.getcwd ())

let expected_path =
  Printf.sprintf
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<playlist xmlns=\"http://xspf.org/ns/0/\" xmlns:vlc=\"http://www.videolan.org/vlc/playlist/ns/0/\" version=\"1\">
	<title>Playlist</title>
	<trackList>
		<track>
			<location>file://%s/data/company.gif</location>
			<title>%s/data/company.gif</title>
			<duration>17</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>0</vlc:id>
			</extension>
		</track>
		<track>
			<location>file://%s/data/escape_&apos;&quot;&lt;&gt;_escape</location>
			<title>%s/data/escape_&apos;&quot;&lt;&gt;_escape</title>
			<duration>11</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>1</vlc:id>
			</extension>
		</track>
		<track>
			<location>file://%s/data/rl.gif</location>
			<title>%s/data/rl.gif</title>
			<duration>47</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>2</vlc:id>
			</extension>
		</track>
		<track>
			<location>file://%s/data/utop_next_word.gif</location>
			<title>%s/data/utop_next_word.gif</title>
			<duration>11</duration>
			<extension application=\"http://www.videolan.org/vlc/playlist/0\">
				<vlc:id>3</vlc:id>
			</extension>
		</track>
	</trackList>
	<extension application=\"http://www.videolan.org/vlc/playlist/0\">
		<vlc:item tid=\"0\"/>
		<vlc:item tid=\"1\"/>
		<vlc:item tid=\"2\"/>
		<vlc:item tid=\"3\"/>
	</extension>
</playlist>
" (Sys.getcwd ()) (Sys.getcwd ()) (Sys.getcwd ()) (Sys.getcwd ())
  (Sys.getcwd ()) (Sys.getcwd ()) (Sys.getcwd ()) (Sys.getcwd ())

let test_output mode expected =
  let rp, wp = Unix.pipe () in
  let erp, ewp = Unix.pipe () in
  let ic = Unix.in_channel_of_descr rp in
  let eic = Unix.in_channel_of_descr erp in
  match Unix.fork () with
  | 0 -> (* child *)
    Unix.close Unix.stdout;
    Unix.close Unix.stderr;
    Unix.dup wp |> ignore;
    Unix.dup ewp |> ignore;
    List.iter Unix.close [ wp; ewp; rp; erp ];
    let path = Printf.sprintf "%s/data" (Sys.getcwd ()) in
    Xspfmaker.xspfmaker mode [path];
    Unix._exit 0
  | _pid -> (* parent *)
    Unix.close wp;
    Unix.close ewp;
    let output = In_channel.input_all ic in
    In_channel.close ic;
    let eoutput = In_channel.input_all eic in
    In_channel.close eic;
    let eexpected =
      Printf.sprintf
        "%s/data/empty: error Invalid data found when processing input\n"
        (Sys.getcwd ())
    in
    (* let outf = open_out "/tmp/output" in *)
    (* let expectedf = open_out "/tmp/expected" in *)
    (* output_string outf output; *)
    (* output_string expectedf expected; *)
    (* Out_channel.close outf; *)
    (* Out_channel.close expectedf; *)
    (* Printf.eprintf "output=%d expected=%d\n%!" *)
    (*   (String.length output) (String.length expected); *)
    assert (output = expected);
    assert (eoutput = eexpected)

let test_filelist mode expected =
  let irp, iwp = Unix.pipe () in
  let rp, wp = Unix.pipe () in
  let erp, ewp = Unix.pipe () in
  let ioc = Unix.out_channel_of_descr iwp in
  let ic = Unix.in_channel_of_descr rp in
  let eic = Unix.in_channel_of_descr erp in
  match Unix.fork () with
  | 0 -> (* child *)
    Unix.close Unix.stdin;
    Unix.close Unix.stdout;
    Unix.close Unix.stderr;
    Unix.dup irp |> ignore;
    Unix.dup wp |> ignore;
    Unix.dup ewp |> ignore;
    List.iter Unix.close [ irp; wp; ewp; iwp; rp; erp ];
    Xspfmaker.xspfmaker mode ["-"];
    Unix._exit 0
  | _pid -> (* parent *)
    Unix.close irp;
    Unix.close wp;
    Unix.close ewp;
    let sendline file =
      output_string ioc @@
      Printf.sprintf "%s/data/%s\n" (Sys.getcwd ()) file
    in
    sendline "company.gif";
    sendline "escape_'\"<>_escape";
    sendline "rl.gif";
    sendline "utop_next_word.gif";
    sendline "empty";
    close_out ioc;
    let output = In_channel.input_all ic in
    close_in ic;
    let eoutput = In_channel.input_all eic in
    close_in eic;
    let eexpected =
      Printf.sprintf
        "%s/data/empty: error Invalid data found when processing input\n"
        (Sys.getcwd ())
    in
    (* let outf = open_out "/tmp/output" in *)
    (* let expectedf = open_out "/tmp/expected" in *)
    (* output_string outf output; *)
    (* output_string expectedf expected; *)
    (* Out_channel.close outf; *)
    (* Out_channel.close expectedf; *)
    (* Printf.eprintf "output=%d expected=%d\n%!" *)
    (*   (String.length output) (String.length expected); *)
    assert (output = expected);
    assert (eoutput = eexpected)

let with_stdout_null f =
  let old_stdout = Unix.dup Unix.stdout in
  Unix.close Unix.stdout;
  let fd = Unix.openfile "/dev/null" [ O_WRONLY ] 0 in
  assert ((Obj.magic fd : int ) = 1);
  Fun.protect
    ~finally:(fun () ->
        Unix.close Unix.stdout;
        Unix.dup old_stdout |> ignore;
        Unix.close old_stdout)
    f

let test_badpath () =
  with_stdout_null @@ fun () ->
  try
    Xspfmaker.xspfmaker Filename ["foo"];
    failwith "expected Invalid_argument"
  with Invalid_argument _ -> ()

let () =
  test_output Xspfmaker.Filename expected_filename;
  test_output Xspfmaker.Path expected_path;
  test_filelist Xspfmaker.Filename expected_filename;
  test_filelist Xspfmaker.Path expected_path;
  test_badpath ()
