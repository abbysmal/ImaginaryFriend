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
  | Saving of (string * string)
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

let save_to_store m msg content =
  let config = config_store m.git_root in
  S.Repo.create config
  >>= S.master task
  >>= fun t ->
  let timestamp = Unix.gettimeofday () |> CalendarLib.Calendar.from_unixfloat in
  let filename = CalendarLib.Printer.Calendar.sprint "%d-%m-%Y"timestamp in
  S.update (t msg) [m.logs_folder;filename] content

let help_msg =
  ["The following commands are accepted:";
   "commit filetag commit_msg: will commit the stored conversation to the file $path/timestam-tag with message commit_msg";
   "bye: I will promptly exit the channel";
   "help: Display this message again"
  ]
