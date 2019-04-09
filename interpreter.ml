type elem = S of string | I of string | N of string | B of string | P of string | NOTHING of string | QUIT of unit | ERROR of string

let interpreter (inFile : string) (outFile : string) : unit =
    
  let ic = open_in inFile in

  let oc = open_out outFile in 

  let rec loop_read acc =
      try 
          
          let l = input_line ic in loop_read (l::acc)
      with
      | End_of_file -> List.rev acc in


let st = Stack.create() in

  let command_lst = loop_read [] in 
  
  
  
  
  let is_alpha alpha = 
    match alpha with 
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true 
    | _ -> false
  
    in 
    
    (*this just takes the 'string list' and returns the head as a string*)
    let str s =
       match s with
       |[]-> "" 
       |hd::tl-> hd 
    in 
   (*my original implementation for dealing with pushs would only push
    everything besides the first word of pushs. e.g. pushs "hello world i love you"
    would push world i love you.
    This final impelemntation checks if the first char is a quotation mark and if so 
    pushes the first word without the quotation mark, then the rest of the words *)
    let rec build pushs =
     match pushs with 
     |[]-> ""
     |hd::tl-> if (hd.[0] = '"') then (String.sub hd 1 (String.length hd - 1)) ^ " " ^ (build tl) 
                                 else str (String.split_on_char '"' hd) ^ " "  ^ (build tl)   	     
 		
 
 in
 
 
    (*passed into this function is each command as a string 
     e.g. "pushi 5" and then splits it up at the space and calls com,
     passing in the new string list*)
(*
    let add (): int = 
    	match (Stack.pop st) with
    	|I i-> (match (Stack.pop st) with
    			|I j-> ((int_of_string i) + (int_of_string j))
    			|_-> -100000000)
    	|_-> -100000000
   	
   	 
   in  *)
    
     let rec start (string_list: string list) : unit =   
    match string_list with                                  
    |[]-> ()                                                
    |hd::tl -> match (String.split_on_char ' ' hd) with
               |[]-> Stack.push (NOTHING "nothing") st; start tl
    		   |[a]->  (match a with
    		   		   |"push"-> Stack.push (ERROR ":error:") st; start tl
            		   |"quit"-> start tl
(*------------------------------------------------------------------------------------------------------------------------------------------------*)            		   
            		   |"pop"-> if(Stack.is_empty st) then begin Stack.push (ERROR ":error:") st; start tl end 
            		  								   else begin Stack.pop st; start tl end
(*------------------------------------------------------------------------------------------------------------------*)            		  								   
            		   |"add"-> if (Stack.length st < 2) then begin Stack.push (ERROR ":error:") st; start tl end 
            		    								  else (match (Stack.top st) with
    							 								|I i-> Stack.pop st; (match (Stack.top st) with
    							 										|I j->  Stack.pop st; Stack.push (I (string_of_int((int_of_string i) + (int_of_string j)))) st; 
    							 										        start tl
    							 										|_-> Stack.push (ERROR ":error:") st; start tl)    							 		
    							 								|_-> Stack.push (ERROR ":error:") st; start tl) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------*)    				
        |"sub"-> if (Stack.length st < 2) then begin Stack.push (ERROR ":error:") st; start tl end 
            		    								  else (match (Stack.top st) with
    							 								|I i-> Stack.pop st; (match (Stack.top st) with
    							 										|I j->  Stack.pop st; Stack.push (I (string_of_int((int_of_string j) - (int_of_string i)))) st; 
    							 										        start tl
    							 										|_-> Stack.push (ERROR ":error:") st; start tl)    							 		
    							 								|_-> Stack.push (ERROR ":error:") st; start tl)
(*---------------------------------------------------------------------------------------------------------------------------------------------------------------*)    			|"mul"-> if (Stack.length st < 2) then begin Stack.push (ERROR ":error:") st; start tl end 
            		    								  else (match (Stack.top st) with
    							 								|I i-> Stack.pop st; (match (Stack.top st) with
    							 										|I j->  Stack.pop st; Stack.push (I (string_of_int((int_of_string i) * (int_of_string j)))) st; 
    							 										        start tl
    							 										|_-> Stack.push (ERROR ":error:") st; start tl)    							 		
    							 								|_-> Stack.push (ERROR ":error:") st; start tl)	 								
(*------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    			|"div"-> if (Stack.length st < 2) then begin Stack.push (ERROR ":error:") st; start tl end 
            		    				  else (match (Stack.top st) with        		    								  		
    							 				|I i-> let c = Stack.pop st in (match (Stack.top st) with
    							 					                           |I j-> if (j = "0" || i = "0") then begin Stack.push c st; Stack.push (ERROR ":error:") st; start tl end
    							 					                                               else begin Stack.pop st; Stack.push (I(string_of_int((int_of_string j) 
    							 													                              / (int_of_string i)))) st; start tl end
    							 					                            |_-> Stack.push (ERROR ":error:") st; start tl)    							 		
    							 				|_-> Stack.push (ERROR ":error:") st; start tl)								
    												 								
    							 								
    							 								)
(*------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    							 								
    							 
    		   |[a;b]->  (match a with 
    		   			  |"push"-> if (b = ":error:" || b = ":unit:") then begin Stack.push (P b) st; start tl end 
    		   			  											   else begin Stack.push (ERROR ":error:") st; start tl end
(*---------------------------------------------------------------------------------------------------------------------------------------------------------*)    		   			  											   
    		   			  |"pushi"->Stack.push (I b) st; start tl  
    		   			  |"pushs"->Stack.push (S (String.sub b 1 (String.length b - 2))) st; start tl 
    		   			  |"pushn"-> if (is_alpha b.[0] = false && b.[0] != '_') then begin Stack.push (ERROR ":error:") st; start tl end
    		   			  														 else begin Stack.push (N b) st; start tl end
(*---------------------------------------------------------------------------------------------------------------------------------------------------------*)													 
    		   			  |"pushb"-> if (b = ":true:" || b = ":false:") then begin Stack.push (B b) st; start tl end 
    		   			  												else begin Stack.push (ERROR ":error:") st; start tl end
(*------------------------------------------------------------------------------------------------------------------------------------------------------*)    		   			  												
    		   			  |_ -> Stack.push (NOTHING "nothing") st; start tl)
     
    		   |hd::tl-> (match hd with
    		   			 |"push" -> Stack.push (ERROR ":error:") st; start tl 
    		  			 |"pushs"-> Stack.push (S (build tl)) st; start tl )
  
    
    in
    
  (*  let c = start ls_str [] in*)
  		  start command_lst;
  		  Stack.iter (fun (ERROR p|NOTHING p|S p|I p|N p|B p|P p) -> Printf.fprintf oc "%s\n" p) st 
    	(* file_write (List.rev c)  *)
    ;;
(* use this line to test*)
interpreter "sample_input3.txt" "output.txt" 
