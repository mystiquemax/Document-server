open Event
open Thread
open Result

type document = string
type username = string
type password = string
type entry = { id : int; doc : document; owner : username; viewers : username list }
type account = { u : username; p : password }
type 'a response = ('a, string) result

type request =
  | Publish of account * document * int response channel
  | ChangeOwner of account * int * username * unit response channel
  | View of account * int * string response channel
  | AddAccount of account * unit response channel
  | AddViewer of account * int * username * unit response channel

type t = request channel

exception Invalid_operation of string

let document_server () =
  let c = new_channel () in
  let rec loop accs docs next_id =
    let check_account a c =
      if List.find_opt (( = ) a) accs = None then (
        sync (send c (Error "invalid login"));
        loop accs docs next_id)
    in
    (match sync (receive c) with
    | Publish (acc, doc, answer_c) ->
        check_account acc answer_c;
        sync (send answer_c (Ok next_id));
        loop accs ({ id = next_id; doc; owner = acc.u; viewers = [] } :: docs) (next_id + 1)
    | ChangeOwner (acc, id, new_owner, answer_c) ->
        check_account acc answer_c;
        if List.find_opt (fun a -> a.u = new_owner) accs = None then
          sync (send answer_c (Error "new owner does not exist"))
        else if List.find_opt (fun d -> d.id = id && d.owner = acc.u) docs = None then
          sync (send answer_c (Error "non-owner can't change owner"))
        else (
          sync (send answer_c (Ok ()));
          let new_docs = List.map (fun e -> if e.id = id then { e with owner = new_owner } else e) docs in
          loop accs new_docs next_id)
    | View (acc, id, answer_c) -> (
        check_account acc answer_c;
        match List.find_opt (fun e -> e.id = id) docs with
        | None -> sync (send answer_c (Error "document does not exist"))
        | Some e ->
            if e.owner = acc.u || List.find_opt (( = ) acc.u) e.viewers <> None then sync (send answer_c (Ok e.doc))
            else sync (send answer_c (Error "cannot view document")))
    | AddViewer (acc, id, viewer, answer_c) -> (
        check_account acc answer_c;
        match (List.find_opt (fun e -> e.id = id) docs, List.find_opt (fun e -> e.u = viewer) accs) with
        | None, _ -> sync (send answer_c (Error "document does not exist"))
        | _, None -> sync (send answer_c (Error "viewer does not exist"))
        | Some e, _ when e.owner <> acc.u -> sync (send answer_c (Error "non-owner can't add viewers"))
        | _ ->
            sync (send answer_c (Ok ()));
            let new_docs = List.map (fun e -> if e.id = id then { e with viewers = viewer :: e.viewers } else e) docs in
            loop accs new_docs next_id)
    | AddAccount (acc, answer_c) ->
        if List.find_opt (fun a -> a.u = acc.u) accs <> None then sync (send answer_c (Error "account already exists"))
        else (
          sync (send answer_c (Ok ()));
          loop (acc :: accs) docs next_id));
    loop accs docs next_id;
    failwith "no return"
  in
  let _ = create (loop [] []) 0 in
  c

let op msg s =
  let c = new_channel () in
  sync (send s (msg c));
  match sync (receive c) with Ok v -> v | Error msg -> raise (Invalid_operation msg)

let publish u p doc = op (fun c -> Publish ({ u; p }, doc, c))
let change_owner u p id owner = op (fun c -> ChangeOwner ({ u; p }, id, owner, c))
let view u p id = op (fun c -> View ({ u; p }, id, c))
let add_account u p = op (fun c -> AddAccount ({ u; p }, c))
let add_viewer u p id viewer = op (fun c -> AddViewer ({ u; p }, id, viewer, c))
