open Mainfile
let table = 
    let filename = Sys.argv.(1) in 
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    let rec createTable acc = 
    let result = Real_parser.main Real_lexer.token lexbuf in
        match result with (Node(("file_end",0),[]),[]) -> acc
        | _ -> (createTable (result::acc)) 
      in 
    (createTable []) 
;;

let _ =
  Printf.printf "?-"; flush stdout;
let lexbuf = Lexing.from_channel stdin in
while true do
  let result = Real_parser.main Real_lexer.token lexbuf in
  match result with 
  (Node(("exit",0),[]),[]) -> Printf.printf "EXITING\n";flush stdout; exit 0
  | (a,[])->
     begin
  let yo =(solve3  ( newTable table) ( newTable table) result result [] true []) in
      if(triplet_first yo) then 
      begin
      Printf.printf "True\n"; 
      Printf.printf "\n?-";flush stdout;
      end
      else
      begin
      Printf.printf "False\n" ;Printf.printf "\n?-"; flush stdout;
      end
    end
    | _ ->  Printf.printf "INVALID INPUT GOAL\n";Printf.printf "\n?-"; flush stdout;
  done
      