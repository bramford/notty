open Notty

let output_image_endline = Notty_unix.output_image_endline
;;

    (*{2 Hello})*)
I.string A.empty "Rad!" |> output_image_endline
;;

I.string A.(fg lightred) "Rad!" |> output_image_endline
;;

let a1 = A.(fg lightwhite ++ bg red)
and a2 = A.(fg red)
;;

I.(string a1 "Rad" <|> string a2 " stuff!")
|> output_image_endline
;;

I.(string a1 "Rad" <|> (void 0 1 <-> string a2 "stuff!"))
|> output_image_endline
;;

let square = "\xe2\x97\xbe"

let rec sierp n =
  if n > 1 then
    let ss = sierp (pred n) in I.(ss <-> (ss <|> ss))
  else I.(string A.(fg magenta) square |> hpad 1 0)
;;

sierp 8 |> output_image_endline;;

let s = sierp 6 in I.(s </> hpad 1 0 s) |> output_image_endline
;;

let rad n color =
  let a1 = A.fg color in
  let a2 = A.(st blink ++ a1) in
  I.((string a1 "Rad" |> hpad n 0) <->
     (string a2 "stuff!" |> hpad (n + 6) 0))
in
A.[ red; green; yellow; blue; magenta; cyan ]
  |> List.mapi I.(fun i c -> rad i c |> pad ~t:i ~l:(2 * i))
  |> I.zcat
  |> output_image_endline
;;

I.strf ~attr:A.(fg green) "(%d)" 42 |> output_image_endline
;;

let pp = Format.pp_print_int |> I.pp_attr A.(fg green) in
I.strf "(%a)" pp 43 |> output_image_endline
;;

Notty_unix.output_image_size @@ fun (w, _) ->
  let i1 = I.string A.(fg green) "very"
  and i2 = I.string A.(fg yellow) "melon" in
  I.(i1 <|> void (w - width i1 - width i2) 1 <|> i2)
;;

Notty_unix.output_image_size @@ fun (w, _) ->
  let steps = int_of_float ((log (float w)) /. log 2.) in
  sierp steps |> I.vpad 0 1
;;

open Notty_unix

let img (double, n) =
  let s = sierp n in
  if double then I.(s </> hpad 1 0 s) else s
let rec update t state =
  Term.image t (img state); loop t state
and loop t (double, n as state) =
  match Term.event t with
  | `Key (`Enter,_)        -> ()
  | `Key (`Arrow `Left,_)  -> update t (double, max 1 (n - 1))
  | `Key (`Arrow `Right,_) -> update t (double, min 8 (n + 1))
  | `Key (`Uchar 0x20,_)   -> update t (not double, n)
  | `Resize _              -> update t state
  | _                      -> loop t state
let t = Term.create ();;
update t (false, 1);
Term.release t
