open Lwt.Infix
open Irmin_unix
module SS = Set.Make(String)

type line =
  {
    author: string;
    content: string;
    timestamp : CalendarLib.Calendar.t;
  }

type state =
  | Logging
  | Helping
  | Saving of (string * string * string)
  | Quiting

type model =
  {
    state : state;
    participants : SS.t;
    logs : line list;
    nick : string;
    channel : string;
    git_root : string;
    logs_folder : string;
  }

type rs = model React.signal
type rf = ?step:React.step -> model -> unit
type rp = rs * rf

module S =
  Irmin_git.FS (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let config_store root = Irmin_git.config ~root ~bare:true ()

let header date m =
  let participants = "participants: " ^ (String.concat "," (SS.elements m.participants)) in
  let author = "author: " ^ m.nick in
  let title = Printf.sprintf "title: Irc discussions from %s on %s" m.channel date in
  let tags = "tags: irc,log" in
  Printf.sprintf {|---
%s
%s
%s
%s
---
|} participants author title tags

let canopy_writer date m =
  let rec content_highlight content = function
    | [] -> content
    | participant::participants ->
       let highlighted = Printf.sprintf "`%s`" participant in
       let re = Printf.sprintf "\\(%s\\)" participant |> Re_str.regexp in
       content_highlight (Re_str.global_replace re highlighted content) participants in
  let format_line line =
    let date = CalendarLib.Printer.Calendar.sprint "%d-%m-%Y %H:%M" line.timestamp in
    Printf.sprintf "`%s`   %s   %s\n\n" date line.author line.content in
  let content = List.fold_left (fun a b -> a ^ (format_line b) ) "" m.logs in
  let content_hl = content_highlight content (SS.elements m.participants) in
  (header date m) ^ content_hl

let save_to_store m msg content date =
  let config = config_store m.git_root in
  S.Repo.create config
  >>= S.master task
  >>= fun t ->
  S.update (t msg) [m.logs_folder;date] content

let help_msg =
  ["The following commands are accepted:";
   "commit: commit_msg: will commit the stored conversation to the file $path/timestam-tag with message commit_msg";
   "bye: I will promptly exit the channel";
   "help: Display this message again"
  ]
