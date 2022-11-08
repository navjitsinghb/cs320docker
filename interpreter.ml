(*
Students: Navjit Bath, Franky  
Assignemnt: interpreter project part 1
Date: 11/04/2022


*)


(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in loop cs ls;;

      (*

      STUFF I ADDED: PARSERS AND SUCH 
      Parts I Have Completed: 
      ---------------------------------------------------------------------------------------------------------------------------------------------------
      STUFF THAT CAME WITH ASSIGNMENT

      *)

(*came with assignment*)
let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *)
let return = pure
type com =
    Push of int
  |
    Pop of int;;


(* let integer parser - can be found on canvas week 10 or 11  *)
let integer = 
  many1 digit >>= fun ls -> 
  pure (int_of_string (implode ls));;
(* boooool *)
let bool = 
  (literal "True" >>= fun _->
   return true)
  <|>
  (literal "False" >>= fun _->
   return false);;


let pushCommand = 
  satisfy (fun x ->x='P') >>= fun _ -> 
  satisfy (fun x ->x='u') >>= fun _ -> 
  satisfy (fun x ->x='s') >>= fun _ -> 
  satisfy (fun x ->x='h') >>= fun _ -> 
  satisfy (fun x ->x=' ') >>= fun _ -> 
  integer >>= fun i -> 
  ws >>= fun _->
  pure (Push i);;

let popCommand =
  satisfy (fun x->x='P') >>= fun _ ->
  satisfy (fun x->x='o') >>= fun _ ->
  satisfy (fun x->x='p') >>= fun _ ->
  whitespace >>= fun _ ->
  natural >>= fun i->
  pure (Pop i);;


let popCommand' = 
  literal "Pop" >>= fun _->
  ws >>= fun _->
  integer >>= fun i -> 
  pure (Pop i);;


let traceCommand = 
  literal "Trace" >>= fun _->
  ws >> fun _->

  let parse_abc = 
    satisfy (fun x->x='a') >>= fun c1 ->
    satisfy (fun x->x='b') >>= fun c2 ->
    satisfy (fun x->x='c') >>= fun c3 ->
    pure ([c1;c2;c3]);;

let commandParser = 
  pushCommand <|> popCommand';;


(* evaluating the commands given: push, pop, trace, add *)

let rec eval ls s = 
  (* match ls with 
     Push i :: rest -> eval rest (i::s)
     |
     Pop i :: rest -> 
     match s with 
      h::tail -> eval rest tail
     |
      Add i :: rest ->
      match s with 
        h1::h2::tail -> eval rest (h1+h2)::tail *)




  (* TODO *)
  let interp (src : string) : string list = failwith "unimplemented"

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src



