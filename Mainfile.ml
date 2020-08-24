type variable=string;;
type symbol = string*int;;
type term = V of variable | Node of (symbol * (term list));;
type substitution = term*term;;
type clause= term*term list

exception Invalid_substitution
exception Not_Unifiable

(* Return the first and second value of dublet*)
let doublet_first a =
  match a with
  (b,c)->b
let doublet_second a =
    match a with
    (b,c)->c

let print_bool b=
  if(b) then
  print_string (" True ")
  else
  print_string(" false ");;

(*Return the size of list*)
let rec list_size l =
  match l with 
  [] ->0 |
  x::xs -> 1 + (list_size xs);;

let rec list_append l1 l2 =
  match l2 with
  []->l1
  | x::xs-> list_append (x::l1) xs


  (* heper function to print the term *)
  
let rec print_term (t:term) : (unit) =
  match t with 
  V(x) -> print_string "V(" ; print_string x; print_string ")";
  | Node (a, []) -> print_string "Node(" ; print_string (doublet_first a) ; print_string ",[])" 
  | Node (a,c) -> print_string "Node(" ; print_string (doublet_first a) ; print_string ",[" ;print_term_list c ;  print_string "])" 
  and 
  print_term_list (c : term list) : unit =
  match c with
  [] -> ()|
  x::xs -> print_term x ; print_string " ; " ; print_term_list xs;;
  

 (* let rec print_term (t:term) : (unit) =
    match t with 
    V(x) ->  print_string x; 
    | Node (a, []) ->  print_string (doublet_first a) ; 
    | Node (a,c) -> print_string "Node(" ; print_string (doublet_first a) ; print_string ",[" ;print_term_list c ;  print_string "])" 
    and 
    print_term_list (c : term list) : unit =
    match c with
    [] -> ()|
    x::xs -> print_term x ; print_string " ; " ; print_term_list xs;;
 *)

  (* Helper function to print a substitution*)
let print_subst (s:substitution) : unit =
  print_term (doublet_first s) ; print_string " -> " ; print_term (doublet_second s) ;;

  (*helper function to print the list of substitution*)
let rec print_subst_list (s:substitution list) : unit =
  match s with 
  []->()
  |x::[]-> print_subst x ;
  |x::xs -> print_subst x ;print_newline(); print_subst_list xs;;

  (* implemented my own List map*)
let rec map f l =
  match l with 
  []->[]
  | x::xs -> ( (f x) :: (map f xs) );;

  (* Helper function to print int list*)
let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

(* helper function to print string list*)
let rec print_list2 = function 
  [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list2 l

  (* helper function to print string list list*)
let rec print_list3 = function 
  [] -> ()
  | e::l -> print_list2 e ; print_string "\n" ; print_list3 l

let checked_symbol = [];;

(* checks if arity is non-negative or not*)
let rec check_arity (s: symbol ) : int = 
  if ((doublet_second s) < 0) then 0
  else 1;;

(* checks if the symbol is repeated  or not in the term list*)
let rec check_symbol_rep (l : symbol list) (s:symbol) : int=
  match l with
  [] -> 1
  | x::xs -> if ( doublet_first (x) = (doublet_first s)) then 0 else check_symbol_rep xs s;;

(* helper function to add a term in list*)
let rec add_in_list l s =
  s::l;;

(* mainly check_sig helper function but with a extra argument list which has the checked terms  *)
let rec check_rep (list1 :symbol list) (list2 :symbol list) : int =
  match list1 with
  []-> 1 |
  x::xs -> if ((check_symbol_rep list2 x)=0) then 0 
  else if ((check_arity x)=0) then 0
  else check_rep xs  (add_in_list list2 x) ;;

(* checks if the signature is valid or nnot by calling the check_rep helper function*)
let check_sig (list1 :symbol list) : int =
  check_rep list1 [] ;;

  (* checks if the term is valid or not that is the arity match with the number of terms in the list or not*)
let rec wfterm (t:term) : int =
  match t with
  V(x) -> 1
  | Node (a,b) -> if( (doublet_second a) <> (list_size b) ) then 0 else wfterm_list b
  and 
  wfterm_list (l:term list) : int =
    match l with
    []->1 |
    x::xs -> if((wfterm x ) =0) then 0 else wfterm_list xs;;

  (* return the height of a function by iterating over the list of nodes
  Here I have given V(x) and Node(zero,[]) ht 0 as the are leaf nodes
  *)
let rec ht (a : term) : int =
  match a with
  V(x)-> 0 |
  Node (a,[]) -> 0 |
  Node (a,b) -> 1 + (max_ht b 0) 
  and max_ht  (l:term list) (i:int) : int =
  match l with
  [] -> i |
  x::xs -> if( (ht x) > i ) then (max_ht xs (ht x)) else (max_ht xs i)
  ;;

  (* return the size of function that is the number of terms present in the term*)
let rec size (a : term) : int =
  match a with
  V(x)-> 1 |
  Node (a,[]) -> 1 |
  Node (a,b) -> 1 + (sum_size b 0) 
  and sum_size (l:term list) (i:int) : int =
  match l with
  [] -> i |
  x::xs -> sum_size xs (i+ (size x));;

(* Add the element a in the list l if it is not present else nothing*)
let rec union1 l a =
  match l with
  [] -> a::[] |
  x::xs -> if (x=a ) then l else (x:: (union1 xs a));;

(* returns the union of l1 and l2 *)
let rec union2 l1 l2 =
  match l1 with 
  [] -> l2 |
  x::xs -> union2 xs (union1 l2 x);;

(* Returns the variable list in the term *)
let rec vars_helper (a : term) (b: variable list): (variable list) =
  match a with
  V(x)-> union1 b x |
  Node (a,[]) -> b|
  Node (a, c) -> plus_vars c b
  and 
  plus_vars (c: term list) (b: variable list) : (variable list)= 
  match c with
  []-> b|
  x::xs -> plus_vars xs (union2 b (vars_helper x b))
  ;;

  
let rec vars (a:term) : (variable list) =
  vars_helper a [];;

  (* Return the term by applyin the substitution  in the term given
  I have assumed that the fidst term is variable and not a Node if the substitution contains the 
  first term node then my function return the term as it is without changing anytthing*)
let rec subst_helper (t:term) (s:substitution) : term = 
  match t with
  V(x) -> if ( doublet_first s = V(x)) then (doublet_second s) else V(x) |
  Node (a,[]) -> Node (a,[])|
  Node (a,c) -> Node (a, (subst_list c s)) 
  and
   subst_list (c:term list) (s:substitution) : (term list) =
   match c with 
   [] -> [] |
   x::xs -> (subst_helper x s) :: (subst_list xs s)
;;

  (* Subst function as asked in the assignment I have assumed to take the second argument as a list that is it 
  do the  substitution one by one*)
let rec subst (t:term) (s1 : substitution list) : term =
  match s1 with 
  [] -> t
  | x::xs -> subst (subst_helper t x) xs;;

let rec subst1 s t=
  subst t s;;

  (* It checks if the elemeent a is present in the list l or not *)
let rec not_in_list a b =
  match b with
  []->1|
  x::xs -> if(a=x) then 0 else not_in_list a xs ;;

let rec not_in_list2 a b =
  match b with
  []->true|
  x::xs -> if(a=x) then false else not_in_list2 a xs ;;


  (* It is helper function for mgu it just returns the first substitution which is valid *)
let rec first_diff (s: substitution list)  : substitution =
  match s with
  [] -> raise Invalid_substitution |
  (a,b)::xs -> if a=b then first_diff xs else (a,b) 
  ;;

  (* It return the one substitutuion of all the substitutuion required for the unifing two terms and raise the Not_Unifiable exception if
  the variable to be substituted lies in the list of the tems of node of second term*)
let rec mgu_helper (t1:term) (t2:term) : substitution =
  if(t1 =t2) then (t1,t2) else
  match (t1,t2) with 
  (V(x) , V(y) ) -> ( (V(y),V(x)) ) |
  (V(x) , Node (a,[]) )-> ( (V(x),Node(a,[])) ) |
  (Node (a,[]), V(x)) -> ( (V(x),Node(a,[])) ) |
  (V(x) , Node (a,c)) -> if ((not_in_list x (vars t2) )=1 ) then ( (V(x),Node(a,c)) ) else raise Not_Unifiable |
  (Node (a,c) , V(x)) -> if ((not_in_list x (vars t1) )=1 ) then ( (V(x),Node(a,c)) ) else raise Not_Unifiable |
  (Node (a,c),Node (b,d)) -> if(a <> b) then raise Not_Unifiable else  first_diff ( List.map2 mgu_helper c d )   
  ;;

  (* Returns the list of substitution required to unify the two terms if possible*)
let rec mgu (t1:term) (t2:term) : (substitution list) = 
  if(t1=t2) then [] else ( mgu_helper t1 t2) ::  mgu ( subst_helper t1 (mgu_helper t1 t2) ) ( subst_helper t2 ( mgu_helper t1 t2)) ;;


  (* Return if a two term can be unifies or not*)
let rec mgu_check (t1:term) (t2:term) : bool =
  if(t1=t2) then true 
  else
  let s= try mgu t1 t2 with Not_Unifiable -> [] in
  match s with
  []-> false
  | _ ->true;;

  (* The three function below are newterm newclause and newtable convert the table to a new table with variable string 
  converted to a new string like V(X) will be converted to V(X:) because if the query has same variable given in the 
  input table then my algorithm was h=getting confused that it is variable asked in a query or variable in the input 
  table so I appended the colon term as it can never come in a variable to ensure that the variable in a query are different 
  then input variable.*)
let rec newTerm term = match term with 
V(str) -> V(str^":") 
| Node((strg,intg),[]) -> term 
| Node((strg,intg),l) -> Node((strg,intg),(List.map newTerm l));;

let newClause c1= match c1 with 
(a,l) -> ((newTerm a), (List.map newTerm l));;

let rec newTable table = match table with 
[] -> []
| x::xs -> (newClause x)::(newTable xs);;

(* Return the first and second term of the triplet.*)
let triplet_first c =
  match c with
  (a1,a2,a3)->a1;;

let triplet_second c =
  match c with
  (a1,a2,a3)->a2;;

  (* Ask the user if the answer printed is correct or not.*)
let isCorrect a = 
  let rea = read_line () in
    match rea with ";" -> true
    | "." -> false
    | _-> Printf.printf "Invalid Input\n"; false
    ;;
  
    (* Helper function to unify the substitution llist.*)
let rec unify_helper (s:substitution list) (t:term) : (bool*term) =
  match s with
  []->(false,t)
  | x::xs->match x with
  (a,b)-> if(a=t) then (true,b) else (unify_helper xs t);; 

(* Main function to unify the substitution list*)
let rec unify (s:substitution list) (s1)=
  match s with
  []->[]
  | x::xs-> 
  begin
    match x with
    (a,b) -> let yo=unify_helper s1 b in
    match yo with 
    (true,d) ->(unify ((a,d)::xs) s1)
    |_ ->  (x:: (unify xs s1))
  end
  ;;

  (* Helper function to prin the answer*)
let rec print_ans_helper (s:substitution list) (t:term)  =
  match s with
  []->begin print_term t; raise Invalid_substitution end
  | (a,b)::xs -> if(a=t) then print_subst (a,b) else (print_ans_helper xs t)

  (*Main function to print the  answer. *)
let rec print_ans (s:substitution list) (t:term list)  =
  match t with
  [] ->()
  |x::[]-> (print_ans_helper s x)
  | x::xs -> (print_ans_helper s x) ;print_string " ,";print_newline(); (print_ans s xs);;

  (* Return the variable in the term list*)
let rec vars2_helper (l1:variable list) (l2:term list) =
  match l2 with
  []->l1
  |x::xs-> vars2_helper (union2 l1 (vars x)) xs

 (* Convert the sting into a variable*)  
let rec vars2_helper2 (t:variable list) =
  match t with 
  []->[]
  | x::xs -> V(x):: (vars2_helper2 xs);;

  (* Return  a list a variable in the clause*)
let vars2 (c:clause)   =
  let yo1= (vars (doublet_first c) )in
  vars2_helper2 (vars2_helper yo1 (doublet_second c) )
  ;;

(* My main funciton to solve the assignment solve3 and solve4 in the solve3 the terms are table -> current table , 
table2-> full table , c1->current clause , c2-> query clause, s2:current substitution list , b1 -> bool value 
if it is the main term of the query or a clause which was call between to function,s5-> substitutuion list list 
containing all the asnswers which are rejected so to avoid these annswer and find another answer.*)
let rec solve3 (table:clause list) (table2:clause list) (c1: clause) (c2: clause)  (s2: substitution list) (b1:bool) (s3:substitution list list) =
 (* print_string ("solve3");print_int (list_size table);(print_bool b1);print_int (list_size s3);print_newline();*)
  match table with
  x::xs->
  begin 
    if( mgu_check (doublet_first c1) (doublet_first x) )  then 
    let yo3=list_append s2 (mgu (doublet_first c1) (doublet_first x) ) in
    let yo = List.map (subst1 yo3) (doublet_second x) in 
    let yo2 = (solve4 table2 table2 yo c2 yo3 b1 s3 []) in 
    match yo2 with
    (true,b,s4)->(true,b,(union2 s3 s4))
    |(false,_,s4)->(solve3 xs table2 c1 c2 s2 b1 (union2 s3 s4))
    else
    solve3 xs table2 c1 c2 s2 b1 s3
  end
  |[]->(false,[],s3)
  and
  (* A helper function to the solve3 the variable are  table -> current table , table2-> full table ,termlist -> list of the 
  terms in the caluse to  be solved , c2-> query clause, s2:current substitution list , b1 -> bool value if it is the main term of the query 
  or a clause which was call between to function , s3-> subtitution list list containing all the rejected answer of the parent clause,
  s5-> substitution list list contiaining all the rejected answers of the first element in the termlist.*)
  solve4 (table:clause list) (table2:clause list)  (termlist:term list)  (c2: clause) (s2: substitution list) (b1:bool) (s3:substitution list list) (s5: substitution list list)=
  (*print_string ("solve4");print_int (list_size table);print_int(list_size termlist);(print_bool b1) ;print_int (list_size s3);print_newline();*)
  match termlist with
  []->
  begin 
    if(b1) then
    begin
      if(list_size (vars2 c2)=0) then
      (true,s2,s3) 
      else
      begin
        if(not_in_list2 (unify s2 s2) s3) then
        begin
        print_ans (unify s2 s2) (vars2 c2);
        if(isCorrect s2) then
        (false,s2,s3)
        else
        (true,s2,(unify s2 s2)::s3)
        end
        else
        (false,[],s3)
      end
    end
    else
    if(not_in_list2 s2 s3) then
    (true,s2,s3)
    else
    (false,s2,s3)
  end
  |l::ls->
  match table with
  x::xs->
  begin
    let yo=(solve3 [x] table2 (l,[]) c2 [] false s5) in
    match yo with
    (false,_,s4)->solve4 xs table2 termlist c2 s2 b1 s3 s5
    |(true,a,b)-> 
    let yo1 =List.map (subst1 (union2 a s2)) ls in
    let yo2=solve4 table2 table2 yo1 c2 (union2 a s2) b1 s3 [] in
    match yo2 with
    (false,_,s4)->solve4 table table2 termlist c2 s2 b1 s3 (a::s5)
    |(true,p,q)-> (true,p,(union2 q s3))
  end
  |[]->(false,[],s3);;
