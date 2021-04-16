open Opium.Std
open Lwt.Infix

let page = {|
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1.0">
  <title>Index</title>
</head>
<body>
<form method="post" enctype="multipart/form-data">
  <input type="file" name="myfile" id="myfile">
  <input type="submit" value="upload" name="submit">
</form>
</body>
</html>
|}

let index =
  get "/" (fun _req -> respond' (`Html page))

let open_out_res file =
  open_out_gen [Open_creat; Open_binary; Open_wronly] 0o600 file

let (let*) = Option.bind

let get_content_type headers =
  let* ct =
    Cohttp.Header.to_list headers
    |> List.map (fun (k,v) -> String.lowercase_ascii k, v)
    |> List.assoc_opt "content-type"
  in
  Multipart_form.Content_type.of_string (ct ^ "\r\n") |> Result.to_option

(* Uses Multipart_form_lwt.stream *)
let form_post =
  post "/" (fun req ->
    print_endline "received post";
    let stream_body = Cohttp_lwt.Body.to_stream req.body in
    let open_files = ref [] in
    match get_content_type (Cohttp.Request.headers req.request) with
    | None -> respond' (`String ("Error: missing content type"))
    | Some content_type ->
      let identify =
        let r = ref 0 in
        fun _ -> incr r; !r
      in
      let `Parse th, stream =
        Multipart_form_lwt.stream ~identify stream_body content_type
      in
      let save_part ~id contents =
        Lwt_io.open_file ~flags:[O_CREAT; O_WRONLY] ~perm:0o600 ~mode:Output
            ("file-" ^ string_of_int id) >>= fun cout ->
        open_files := cout :: !open_files;
        let count = ref 0 in
        Lwt_stream.iter_s (fun s ->
          incr count;
          if !count >= 1000 then (
            count := 0;
            Printf.printf "wrote 1000 parts\n%!";
          );
          Lwt_io.write cout s
        ) contents
      in

      let rec save_files () = Lwt_stream.get stream >>= function
        | None -> Lwt.return_unit
        | Some (id, _hdr, contents) ->
          save_part ~id contents >>= fun () ->
          save_files () in
      Lwt.both th (save_files ()) >>= fun (_res, ()) ->
      Lwt_list.iter_s Lwt_io.close !open_files >>= fun () ->
      print_endline "Done";
      redirect' (Uri.of_string "/")
  )

(* uses Multipart_form_lwt.parse *)
let form_post2 =
  post "/" (fun req ->
    print_endline "received post";
    let stream_body = Cohttp_lwt.Body.to_stream req.body in
    match get_content_type (Cohttp.Request.headers req.request) with
    | None -> respond' (`String ("Error: missing content type"))
    | Some content_type ->
      let identify =
        let r = ref 0 in
        fun _ -> incr r; !r
      in
      let on_part _ =
        let id = identify () in
        Lwt_io.open_file ~flags:[O_CREAT; O_WRONLY] ~perm:0o600 ~mode:Output
            ("file-" ^ string_of_int id) >>= fun cout ->
        Lwt.return (
          (fun s -> match s with None -> Lwt_io.close cout | Some s -> Lwt_io.write cout s),
          id
        )
      in
      let th = Multipart_form_lwt.parse ~on_part stream_body content_type in
      th >>= fun _ ->
      print_endline "Done";
      redirect' (Uri.of_string "/")
  )

let app =
  App.empty
  |> index |> form_post

let _ =
  Memtrace.trace_if_requested ();
  App.run_command app
