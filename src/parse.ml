open Types

let get_nick msg =
  let reg = Re_str.regexp ":\\(.*\\)!.*" in
  let _ = Re_str.string_match reg msg 0 in
  Re_str.matched_group 1 msg

let get_commit_msg content =
  let reg = Re_str.regexp ".*commit \\(.*\\)" in
  let _ = Re_str.string_match reg content 0 in
  Re_str.matched_group 1 content

let match_word word msg =
  let reg = Re_str.regexp (Printf.sprintf ".*%s.*" word) in
  Re_str.string_match reg msg 0

let parse_content (v, f) connection author content =
  let model = Lwt_react.S.value v in
  if match_word model.nick content then
    if match_word "help" content then
      `Help
    else if match_word "bye" content then
      `Quit
    else if match_word "commit" content then
      `Saving (get_commit_msg content)
    else
      `Log (author, content)
  else
    `Log (author, content)
