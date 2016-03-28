open Lwt.Infix
open Types
module Irc = Irc_client_lwt

let display_help m connection =
  let connection = connection in
  Lwt_list.iter_s (fun message ->
		   Irc.send_privmsg ~connection ~target:m.channel ~message) help_msg

let update ((v : rs), (f : rf)) connection action =
  let model = Lwt_react.S.value v in
  match action with
  | `Log (author, content) ->
     let timestamp = Unix.gettimeofday () |> CalendarLib.Calendar.from_unixfloat in
     let new_line = { author; content; timestamp } in
     let participants = SS.add author model.participants in
     let new_log = { model with state = Logging;
				logs = (List.append model.logs [new_line]);
				participants} in
     f new_log
  | `Help ->
     f { model with state = Helping }
  | `Quit ->
     f { model with state = Quiting }
  | `Saving msg ->
     let content = canopy_writer model in
     let state = Saving (msg, content) in
     f { model with state; logs = []; participants = SS.empty;}

let callback (r : rp) connection result =
  let open Irc_message in
  let open Parse in
  match result with
  | `Ok msg ->
     begin
       match msg.command with
       | PRIVMSG (_, content) ->
	  let author = get_nick (to_string msg) in
	  let action = parse_content r connection author content in
	  update r connection action |> Lwt.return
       | _ -> Lwt.return_unit
     end
    | `Error e ->
       Lwt.return_unit

let lwt_main server port channel nick message git_root logs_folder =
  let open Lwt_unix in
  let realname = "Imaginary Friend" in
  let username = nick in
  gethostbyname server
  >>= fun host -> Irc.connect ~addr:(host.h_addr_list.(0))
			      ~port ~username ~mode:0 ~realname ~nick ()
  >>= fun connection -> Irc.send_join ~connection ~channel
  >>= fun () -> Irc.send_privmsg ~connection ~target:channel ~message
  >>= fun () ->
		let signal_map_fn m =
		  match m.state with
		  | Helping -> display_help m connection
		  | Logging -> Lwt.return_unit
		  | Saving (msg, content) -> save_to_store m msg content
		  | Quiting -> Irc_client_lwt.send_quit ~connection in

		let model = { state = Logging; participants = SS.empty;
			      logs = []; nick; channel; git_root; logs_folder} in
		let (v, f) = Lwt_react.S.create model in
		let callback = callback (v, f) in
		Lwt_react.S.map_s signal_map_fn v >>= fun _ ->
		Irc.listen ~connection ~callback
  >>= fun () -> Irc.send_quit ~connection

let lwt_start server port channel nick message git_root logs_folder =
  Lwt_main.run (lwt_main server port channel nick message git_root logs_folder)

let () = match Cmdliner.Term.eval (Args.arg_description lwt_start) with `Error _ -> exit 1 | _ -> exit 0
