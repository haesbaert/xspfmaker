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
  let ic = Unix.in_channel_of_descr rp
  and oc = Unix.out_channel_of_descr wp in
  let path = Printf.sprintf "%s/data" (Sys.getcwd ()) in
  Xspfmaker.xspfmaker mode ic oc [path];
  Out_channel.close oc;
  let output = In_channel.input_all ic in
  In_channel.close ic;
  if output <> expected then begin
    let got = open_out "/tmp/got" in
    let ex = open_out "/tmp/expected" in
    output_string got output;
    output_string ex expected;
    close_out got;
    close_out ex;
    Unix.system "diff -u /tmp/got /tmp/expected" |> ignore;
    output_string stderr "Check /tmp/got vs /tmp/expected\n"
  end;
  assert (output = expected)

let test_filelist mode expected =
  let rp, wp = Unix.pipe () in
  let ic = Unix.in_channel_of_descr rp
  and oc = Unix.out_channel_of_descr wp in
  let rp2, wp2 = Unix.pipe () in
  let ic2 = Unix.in_channel_of_descr rp2
  and oc2 = Unix.out_channel_of_descr wp2 in
  let sendline file =
    output_string oc2 @@
    Printf.sprintf "%s/data/%s\n" (Sys.getcwd ()) file
  in
  sendline "company.gif";
  sendline "escape_'\"<>_escape";
  sendline "rl.gif";
  sendline "utop_next_word.gif";
  sendline "empty";
  close_out oc2;
  Xspfmaker.xspfmaker mode ic2 oc [];
  Out_channel.close oc;
  close_in ic2;
  let output = In_channel.input_all ic in
  close_in ic;
  assert (output = expected)

let with_null_redirect ofd f =
  let old = Unix.dup ofd in
  Unix.close ofd;
  let fd = Unix.openfile "/dev/null" [ O_WRONLY ] 0 in
  assert ((Obj.magic fd : int) = (Obj.magic ofd : int));
  Fun.protect
    ~finally:(fun () ->
        Unix.close ofd;
        Unix.dup old |> ignore;
        Unix.close old)
    f

let test_badpath () =
  with_null_redirect Unix.stdout @@ fun () ->
  try
    Xspfmaker.xspfmaker Filename stdin stdout ["foo"];
    failwith "expected Invalid_argument"
  with Invalid_argument _ -> ()

let () =
  with_null_redirect Unix.stderr @@ fun () ->
  test_output Xspfmaker.Filename expected_filename;
  test_output Xspfmaker.Path expected_path;
  test_filelist Xspfmaker.Filename expected_filename;
  test_filelist Xspfmaker.Path expected_path;
  test_badpath ()
