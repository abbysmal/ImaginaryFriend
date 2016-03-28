open Cmdliner

let arg_description start_fn =
  let server =
    let doc = Arg.info ~doc:"Server addr" ["s"; "server"] in
    Arg.(value & opt string "chat.freenode.net" doc) in
  let port =
    let doc = Arg.info ~doc:"Server port" ["p"; "port"] in
    Arg.(value & opt int 6667 doc) in
  let channel =
    let doc = Arg.info ~doc:"Channel" ["c"; "channel"] in
    Arg.(value & opt string "##miraitest" doc) in
  let nick =
    let doc = Arg.info ~doc:"Nick" ["n"; "nick"] in
    Arg.(value & opt string "canobot" doc) in
  let message =
    let doc = Arg.info ~doc:"Welcome message" ["m"; "message"] in
    Arg.(value & opt string "starting logging" doc) in
  let git_root =
    let doc = Arg.info ~doc:"Local git clone" ["g"; "gitroot"] in
    Arg.(value & opt string "/tmp/imaginaryfriend" doc) in
  let logs_folder =
    let doc = Arg.info ~doc:"Folder where logs will be commited" ["f"; "logs_folder"] in
    Arg.(value & opt string "irclogs" doc) in
  Term.(Term.const start_fn $ server $ port $ channel $ nick $ message $ git_root $ logs_folder),
  Term.info "imaginaryfriend" ~version:"0.1"
