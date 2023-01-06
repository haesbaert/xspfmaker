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
  let ic = Unix.in_channel_of_descr rp in
  match Unix.fork () with
  | 0 -> (* child *)
    Unix.close Unix.stdout;
    Unix.dup wp |> ignore;
    let path = Printf.sprintf "%s/data" (Sys.getcwd ()) in
    Xspfmaker.xspfmaker mode [path];
    Unix._exit 0
  | _pid -> (* parent *)
    Unix.close wp;
    let output = In_channel.input_all ic in
    In_channel.close ic;
    (* let outf = open_out "/tmp/output" in *)
    (* let expectedf = open_out "/tmp/expected" in *)
    (* output_string outf output; *)
    (* output_string expectedf expected; *)
    (* Out_channel.close outf; *)
    (* Out_channel.close expectedf; *)
    (* Printf.eprintf "output=%d expected=%d\n%!" *)
    (*   (String.length output) (String.length expected); *)
    assert (output = expected)

let () =
  test_output Xspfmaker.Filename expected_filename;
  test_output Xspfmaker.Path expected_path
