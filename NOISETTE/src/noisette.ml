
(** 
    Cette partie du programme NE DOIT EN AUCUN CAS être modifiée sous peine
    de voir votre code ne pas passer les tests 
*)
type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

(**
   VOUS N'AVEZ PAS BESOIN DE LIRE OU DE COMPRENDRE CETTE PARTIE DU CODE (LES SIX FONCTIONS SUIVANTES). 
   ELLE EST VOLONTAIREMENT NON COMMENTÉE ET RÉDIGÉE DE MANIÈRE PEU CLAIRE 
 *)
let rec analyse_comment fd =
  let c = Scanf.bscanf fd "%c" (fun c -> c) in
  if c = '\n'
  then ()
  else analyse_comment fd
             
let rec analyse_program_aux =
  fun fd lvl  ->
  try 
    let c = Scanf.bscanf fd "%c" (fun a  -> a) in
    if c = '#'
    then
      let _ = analyse_comment fd in
      analyse_program_aux fd lvl
    else if c = ';' || c = '\n' || c = ' '
    then analyse_program_aux fd lvl
    else if c = ']'
    then
      if lvl > 0
      then []
      else raise (Invalid_argument "Error on char ]")
    else if c = 'W'
      then
        let i= Write(Scanf.bscanf fd "(%c)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'C'
      then
        let i= Caesar(Scanf.bscanf fd "(%d)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'D'
    then
      let a = Scanf.bscanf fd "(%c)" (fun a -> a) in 
      let i= Delete(a) in
      let li = analyse_program_aux fd lvl in
      i::li
    else if c = 'R'
    then let li = analyse_program_aux fd lvl in
         Right::li
    else if c = 'I'
    then Invert::analyse_program_aux fd lvl
    else if c = 'L'
    then Left::analyse_program_aux fd lvl
    else if c = 'F'
    then
      let n = Scanf.bscanf fd "(%d,[" (fun n -> n) in
      let l = analyse_program_aux fd (lvl + 1) in
      let c = Scanf.bscanf fd "%c" (fun a -> a) in
      if c <> ')' then raise (Invalid_argument ("Error found '"^String.make 1 c^"' instead of ')'"))
      else
        let li = analyse_program_aux fd lvl in
        Repeat(n,l)::li
    else
      let _ = Format.printf  "ERROR '%c' (%d)@." c (Char.code c) in
      assert false
  with End_of_file -> []

let rec read_file_aux =
  fun acc fd ->
  try
    let c = Scanf.bscanf fd "%c" (fun x -> x) in
    read_file_aux (c::acc) fd
  with End_of_file -> acc

let read_file file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      List.rev (read_file_aux [] fd)
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127

                    
let rec fprintf_instruction fmt i =
  match i with
  | Write c -> Format.fprintf fmt "W(%c)" c
  | Right -> Format.fprintf fmt "R"
  | Left -> Format.fprintf fmt "L"
  | Repeat(n,li) ->
     Format.fprintf fmt "F(%d,[%a])" n (fun fmt li -> List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) li) li
  | Caesar n -> Format.fprintf fmt "C(%d)" n
  | Delete(a) -> Format.fprintf fmt "D(%c)" a
  | Invert -> Format.fprintf fmt "I"
            
let fprintf_program fmt l =
  List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) l

(*** 
     Retour au côté clair
*)


(* 
   [print_program] : program -> unit 
   affiche un [program] sur la sortie standard de l'executable
 *)  
let print_program p = Format.printf "%a" fprintf_program p

(*
  [analyse_program] : unit -> program
  Lit un programme 1Aien ou 2Aien, l'analyse et le retourne sous forme d'une valeur de type [program]
 *)                    
let analyse_program file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      analyse_program_aux fd 0
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127









    
(** Votre code doit commencer à partir de ce point.

    NE MODIFIER RIEN AU DESSUS DE CE POINT 
*)

type ruban = {left: char list; right: char list};; (*This is a zipper*)

(*
ruban -> ruban

@requires nothing
@ensures move the cursor one character to the left, generating an nul character if the leftmost side of the zipper is reached.
@raises nothing
*)
let go_left ruban = 
  match ruban with
  | {left= [] ; right=rlist} -> {left=[]; right= '\000'::rlist};(*'\000' correspond au caractère nul*)
  | {left=hd::tl; right=rlist} -> {left=tl; right=hd::rlist};;

(*
ruban -> ruban

@requires nothing
@ensures move the cursor one character to the right, generating an nul character if the rightmost side of the zipper is reached. 
@raises nothing
*)
let go_right ruban = 
  match ruban with
  | {left= llist ; right=[]} -> {left='\000'::llist; right=[]};(*'\000' correspond au caractère nul*)
  | {left=llist; right=hd::tl} -> {left=hd::llist; right=tl};;

(*
char -> ruban -> ruban

@requires nothing
@ensures write the character c at the cursor, replace the current one if there is one.
@raises nothing 
*)
let write c ruban = 
  match ruban with
  | {left=llist;right=[]} -> {left=llist;right=[c]};
  | {left=llist;right=hd::tl} -> {left=llist;right=c::tl};;

(*
int -> char -> char

@requires caesar_key should be positive
@ensures shift the character c caesar_key times in the alphabet, if it c is indeed a letter
@raises nothing
*)
let caesar_shift caesar_key c = 
  let code = Char.code c in
  if code > 64 && code < 91 then(*Char.code 'A' = 65 & Char.code 'Z' = 90*)
    Char.chr ( 65 + ( ( code - 65 + caesar_key ) mod 26 ) )
  else if code > 96 && code < 123 then
    Char.chr ( 97 + ( ( code - 97 + caesar_key ) mod 26 ) )(*Char.code 'a' = 97 & Char.code 'z' = 122*)
  else
    c;;

(*
int -> char list -> char list

@requires ceasar_key should be positive
@ensures shift all the letters of the list l ceasar_key times in the alphabet
@raises nothing
*)
let caesar_list caesar_key l = 
  List.map (caesar_shift caesar_key) l;;

(*
int -> ruban -> ruban

@requires caesar_key should be positive
@ensures shift all the letters of ruban ceasar_key times in the alphabet
@raises nothing
*)
let caesar caesar_key ruban =
  {left= caesar_list caesar_key ruban.left; right= caesar_list caesar_key ruban.right};;  

(*
char -> ruban -> ruban

@requires nothing
@ensures delete all the instances of the character c in ruban
@raises nothing
*)
let delete c ruban =
  let aux elt = (elt <> c) in
  {left=List.filter aux ruban.left; right=List.filter aux ruban.right};;(*List.filter f l return the list l with only the elements e where f e is true*)

(*
ruban -> ruban

@requires nothing
@ensures invert ruban, and keep the cursor at the same position
@raises nothing
*)
let invert ruban =
  go_left {left = ruban.right ; right = ruban.left};;(*go_left is used here to replace the cursor at the same point.*)

(*
program -> ruban -> ruban

@requires nothing
@ensures apply all the instructions of program p to ruban
@raises nothing

This function was made to not change the inputs of the function execute_program
*)
let rec execute_on_ruban p ruban  = 
  match p with
  | [] -> ruban;
  | Left::tl -> let new_ruban = go_left ruban in execute_on_ruban tl new_ruban ;
  | Right::tl -> let new_ruban = go_right ruban in execute_on_ruban tl new_ruban ;
  | (Write c)::tl -> let new_ruban = write c ruban  in execute_on_ruban tl new_ruban ;
  | (Repeat (n,instructions))::tl -> 
      if n=0 then 
        execute_on_ruban tl ruban
      else
        let new_ruban = execute_on_ruban instructions ruban  in execute_on_ruban ((Repeat (n-1,instructions))::tl) new_ruban ;
  | (Delete a)::tl -> let new_ruban = delete a ruban in execute_on_ruban tl new_ruban ;
  | (Caesar n)::tl -> let new_ruban = caesar n ruban in execute_on_ruban tl new_ruban ;
  | Invert::tl -> let new_ruban = invert ruban in execute_on_ruban tl new_ruban ;;

(*
program -> ruban

@requires nothing
@ensures execute the program p, and write the resulting message on a ruban
@raises nothing
*)
let execute_program p = 
  execute_on_ruban p {left=[];right=[]};;

(*
('a -> char -> 'a) 'a -> ruban -> 'a

@requires the function f should accept every kind of character as a second argument
@ensures apply the function f on the ruban r from the leftmost to the rightmost side as List.fold_left on lists, using v0 as the first argument of the function
@raises nothing
*)
let fold_ruban f v0 r = 
  List.fold_left f (List.fold_right (fun x y -> f y x) r.left v0) r.right;;

(*
char list -> program

@requires nothing
@ensures create a first program with simply a write of every character of the msg, in the same order
@raises nothing

We first replace every character of the msg with their Write to simplify their manipulation along side the repeats.
*)
let list_of_write msg =
  List.fold_right (fun elmt acc -> (Write elmt)::acc) msg [];;

(*
program -> program

@requires nothing
@ensures add the instruction Right after every instruction Write of the program p
@raises nothing

Pattern recognition is more efficient with just the characters, without the right instructions, that is why those are inserted only at the end.
*)
let rec add_right p =
  let aux elmt acc =
    match elmt with
    | Repeat (n,instructions) ->
      (Repeat (n,add_right instructions))::acc;
    | Write c -> elmt::Right::acc;
    | _ -> elmt::acc;(*this function work on every kind of programs, including ones with Invert,Caesar,etc... We should not add a right next to them*)
  in
  List.fold_right aux p [];;

(*
'a list -> 'a list -> int -> 'a list * 'a list

@requires nb_elements should be positive
@ensures extracts the first nb_elements elements from program and stitches it to the end of the reverse of current_pattern. If program doesn't have that much elements, it will entirely be stitched to current_pattern.
@raises nothing
*)
let rec extract_pattern_aux program current_pattern nb_elements =
  if nb_elements = 0 then 
    (List.rev current_pattern,program)(*We add the elements in the wrong order in the pattern for the actual fonction to work properly. Here is why we use List.rev*)
  else
    match program with
    | [] -> (List.rev current_pattern,program);(*cf 3 lines higher*)
    | hd::tl -> extract_pattern_aux tl (hd::current_pattern) (nb_elements-1);;

(*
'a list -> int -> 'a list * 'a list

@requires nb_elements should be strictly positive
@ensures extract the nb_elements first elements of program and returns a couple with first the extracted elements, and second the remaining list
@raises nothing
*)
let extract_pattern program nb_elements =
  extract_pattern_aux program [] nb_elements;;

(*
program -> program -> program -> int -> program -> program

@requires pattern_size > 0, and the first fst_pattern should be larger or the same size than the first snd_pattern. The first acc should be empty.
@ensures merge the identicl groups of pattern_size instructions from the program (fst_pattern@snd_pattern@program), into Repeats
@raises nothing
*)
let rec match_pattern program fst_pattern snd_pattern pattern_size acc =
  match (fst_pattern,snd_pattern) with
  (*Note: The same way than in extract_pattern, we have to reverse acc when returning it, because we use :: to add new elements to it.*)
  | (instructions,[]) -> (List.rev acc) @ instructions;
  | ([Repeat (n1,i1)], [Repeat (n2,i2)]) ->
    let (new_pattern,remaining_program) = extract_pattern program pattern_size in 
    if i1 = i2 then
      match_pattern remaining_program [Repeat (n1+n2,i1)] new_pattern pattern_size acc(*the two Repeats are merged*)
    else
      match_pattern remaining_program snd_pattern new_pattern pattern_size ((Repeat (n1,i1))::acc);(*there is no match, we move to the next elements*)
  | ([Repeat (n,i1)],i2) ->
    (*
    The pattern (i1,[Repeat(n,i2)]) is not considered because:
      - if i1 = i2 ; if there is Repeat(n,i), it means that the pattern i2 has already been recognised. However, i1 is analysed first. Therefore, i1 should be in a Repeat. So i1 <> 12.
      - if i1 <> i2 ; this match would be managed the same way than (instructions1,instructions2), with i1 = instructions1 and Repeat(n,i2) = instructions2.
      => We don't need to consider this case.
    *)
    let (new_pattern,remaining_program) = extract_pattern program pattern_size in 
    if i1 = i2 then
      match_pattern remaining_program [Repeat (n+1,i1)] new_pattern pattern_size acc(*there, i2 is added into the Repeat*)
    else
      match_pattern remaining_program snd_pattern new_pattern pattern_size ((Repeat (n,i1))::acc);(*there is no match there, we move to the next group*)
  | (instructions1,instructions2) ->
    if instructions1 = instructions2 then
      let (new_pattern,remaining_program) = extract_pattern program pattern_size in
      match_pattern remaining_program [Repeat (2,instructions1)] new_pattern pattern_size acc(*i1 and i2 are merged*)
    else
      match program with
      | [] -> (List.rev acc) @ (fst_pattern @ snd_pattern);
      | program_hd::program_tl ->
        match fst_pattern with
        | [] -> [];(*Only happens when the first fst_pattern is empty*)
        | fst_pattern_hd::fst_pattern_tl ->
          match snd_pattern with
          | [] -> (List.rev acc) @ fst_pattern;(*This case can't happen, because we already take care of this situation in the case (fst_pattern,snd_pattern) = (instructions,[])*)
          | snd_pattern_hd::snd_pattern_tl ->
            match_pattern program_tl (fst_pattern_tl@[snd_pattern_hd]) (snd_pattern_tl@[program_hd]) pattern_size (fst_pattern_hd::acc);;(*We shift all the patterns by 1*)

(*
program -> int -> program

@requires pattern_size > 0
@ensures merge the identical groups of instrcutions, with a size of at least pattern_size, from program into Repeats
@raises nothing
*)
let rec simplify program pattern_size =
  if pattern_size > (List.length program)/2 then program (*if the first pattern is larger than half of the program, it can't match with anything after it*)
  else
    let (fst_pattern,fst_program) = extract_pattern program pattern_size in
    let (snd_pattern,snd_program) = extract_pattern fst_program pattern_size in
    let new_program = match_pattern snd_program fst_pattern snd_pattern pattern_size [] in
    if program = new_program then 
      simplify new_program (pattern_size+1)
    else
      simplify new_program pattern_size;;

(*
char list -> program

@requires nothing
@ensures create a program with a low amount of instructions to write the message represented by msg
@raises nothing
*)
let generate_program msg = 
  let program = list_of_write msg in(*We replace every character with some writes*)
  let simplified_program = simplify program 1 in(*We combine theim into repeats*)
  add_right simplified_program;;(*we finaly add the rights*)



                      
(** Votre code doit s'arreter à partir de ce point.

    NE MODIFIER RIEN EN DESSOUS DE CE POINT 
 *)











                         
                         
let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s <1|2|3> <file>@." Sys.argv.(0) in
  exit i
  
let main phase file =
  if phase = "1" || phase = "2"
  then
    let li = analyse_program file in
    let rub = execute_program li in
    let _ = fold_ruban (fun _ c -> Format.printf "%c" c) () rub in
    Format.printf "@."
  else if phase = "3"
  then
    let msg = read_file file in
    let p = generate_program msg in
    print_program p
  else die 1


let _ =
  if Array.length Sys.argv = 3 
  then
    main Sys.argv.(1) Sys.argv.(2)
  else die 2
