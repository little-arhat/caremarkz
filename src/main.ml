open Am_All

open Amall_types

module IO = IO_Lwt

module I = Iteratees.Make(IO)

module S = Amall_http_service.Service(IO)(I)

let addr =
  let he = Unix.gethostbyname "remar.kz" in
  let hal = he.Unix.h_addr_list in
  hal.(0)

let (_listener, root) = S.run_listener (`Inet_addr (addr, 9090))

let my_endpoint = (root, `Service ([""; "http://remar.kz:9090"], "test"))

open Amall_http

let tmpl = Printf.sprintf "<html><body><h1>%s</h1></body></html>"

let worker segpath _rq =
  let txt = Printf.sprintf "[%s]\n" &
    String.concat " ; " &
    List.map (Printf.sprintf "%S") &
    segpath in
  I.return
    {rs_status_code = 200;
     rs_reason_phrase = "OK";
     rs_headers = { rs_all = [] };
     rs_body = Body_string (tmpl txt)
    }


open I.Ops

let rec loop_forever () =
  Lwt_unix.sleep 1. >>% fun () ->
  loop_forever ()

let () = S.mount my_endpoint worker

let (_ : [> `Error of exn | `Ok of 'a]) = IO.runIO (loop_forever ())
