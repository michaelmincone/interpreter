type elem = S of string | I of string | N of string | B of string | P of string | NOTHING of string | QUIT of unit | ERROR of string

let interpreter ( (inFile : string), (outFile : string )) : unit =
    
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
    
  let is_num str =
  try ignore (int_of_string str); true
  with _ -> false
    
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
 
 let rec buildWQ pushs = 
 	match pushs with
 	|[]-> ""
    |hd::tl->if (hd = "") then " " ^ buildWQ tl else hd ^ buildWQ tl 
 in
 
 let boolean str = 
 	match str with
 	|":false:"-> false
 	|":true:"-> true
 	|_->false
 in
 
 
 
     let oneComm com = 
     	match com with 
     	|"push"-> Stack.push (ERROR ":error:") st; ()
        
(*------------------------------------------------------------------------------------------------------------------------------------------------*)            		   
        |"pop"-> if (Stack.is_empty st) then begin Stack.push (ERROR ":error:") st end else begin Stack.pop st; () end
(*------------------------------------------------------------------------------------------------------------------*)            		  								   
        |"add"-> if (Stack.length st < 2) then Stack.push (ERROR ":error:") st 
                 else (match (Stack.top st) with
    				  |I i-> let a = Stack.pop st in (match (Stack.top st) with
    							 					 |I j->  Stack.pop st; Stack.push (I (string_of_int((int_of_string i) + (int_of_string j)))) st;
    							 				     |_-> Stack.push a st; Stack.push (ERROR ":error:") st; )    							 		
    				  |_-> Stack.push (ERROR ":error:") st;) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------*)    				
        |"sub"-> if (Stack.length st < 2) then Stack.push (ERROR ":error:") st 
            	 else (match (Stack.top st) with
    				  |I i-> let s = Stack.pop st in (match (Stack.top st) with
    							 		             |I j->  Stack.pop st; Stack.push (I (string_of_int((int_of_string j) - (int_of_string i)))) st;
    							 		   			 |_-> Stack.push s st; Stack.push (ERROR ":error:") st;)    							 		
    				  |_-> Stack.push (ERROR ":error:") st;)
(*---------------------------------------------------------------------------------------------------------------------------------------------------------------*)    			|"mul"-> if (Stack.length st < 2) then Stack.push (ERROR ":error:") st  
            	 else (match (Stack.top st) with
    				  |I i-> let m = Stack.pop st in (match (Stack.top st) with
    							 		   			 |I j->  Stack.pop st; Stack.push (I (string_of_int((int_of_string i) * (int_of_string j)))) st;
    							 		   			 |_-> Stack.push m st; Stack.push (ERROR ":error:") st;)    							 		
    				  |_-> Stack.push (ERROR ":error:") st;)	 								
(*------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    			|"div"-> if (Stack.length st < 2) then Stack.push (ERROR ":error:") st  
            	 else (match (Stack.top st) with        		    								  		
    				  |I i-> let d = Stack.pop st in (match (Stack.top st) with
    							 					 |I j-> if (i = "0") then begin Stack.push d st; Stack.push (ERROR ":error:") st end
    							 					        else begin Stack.pop st; Stack.push (I(string_of_int((int_of_string j) / (int_of_string i)))) st end
    							 					 |_-> Stack.push d st; Stack.push (ERROR ":error:") st;)    							 		
    				  |_-> Stack.push (ERROR ":error:") st;)								
(*---------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    			|"rem"-> if (Stack.length st < 2) then Stack.push (ERROR ":error:") st
            	 else (match (Stack.top st) with        		    								  		
    				  |I i-> let c = Stack.pop st in (match (Stack.top st) with
    							 					 |I j-> if (i = "0") then begin Stack.push c st; Stack.push (ERROR ":error:") st; end
    							 					        else begin Stack.pop st; Stack.push (I(string_of_int((int_of_string j) 
    							 					                                                  mod (int_of_string i)))) st; end
    							 					 |_-> Stack.push c st; Stack.push (ERROR ":error:") st;)    							 		
    				  |_-> Stack.push (ERROR ":error:") st;)									 								
(*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    		|"neg"-> if(Stack.is_empty st) then Stack.push (ERROR ":error:") st  
				 else (match (Stack.top st) with
				 	  |I i-> Stack.pop st; Stack.push (I(string_of_int(-int_of_string(i)))) st
				 	  |_-> Stack.push (ERROR ":error:") st)	
(*----------------------------------------------------------------------------------------------------------------------------------------------------*)				 	  			|"swap"-> if(Stack.length st < 2) then Stack.push (ERROR ":error:") st
				  else begin let t = Stack.pop st in let t1 = Stack.pop st in Stack.push t st; Stack.push t1 st; end
(*----------------------------------------------------------------------------------------------------------------------------------------------------------*)
		|"cat"-> if(Stack.length st < 2) then Stack.push (ERROR ":error:") st
				 else (match (Stack.top st) with 
				 	  |S s-> let cat = Stack.pop st in (match (Stack.top st) with
				 	  								   |S s1-> Stack.pop st; Stack.push (S(s1 ^ s)) st
				 	  								   |_-> Stack.push cat st; Stack.push (ERROR ":error:") st)
				 	  |_-> Stack.push (ERROR ":error:") st) 
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------*)
		|"and"-> if(Stack.length st < 2) then Stack.push (ERROR ":error:") st
				 else (match (Stack.top st) with 
				 	  |B b-> let and1 = Stack.pop st in (match (Stack.top st) with
				 	  								   |B b1-> Stack.pop st; if (boolean b && boolean b1) then Stack.push (B ":true:") st 
				 	  								                                                      else Stack.push (B ":false:") st
				 	  								   |_-> Stack.push and1 st; Stack.push (ERROR ":error:") st)
				 	  |_-> Stack.push (ERROR ":error:") st) 
(*-------------------------------------------------------------------------------------------------------------------------------------------------------*)				 	  
		|"or"-> if(Stack.length st < 2) then Stack.push (ERROR ":error:") st
				else (match (Stack.top st) with 
				 	  |B b-> let or1 = Stack.pop st in (match (Stack.top st) with
				 	  								   |B b1-> Stack.pop st; if (boolean b || boolean b1) then Stack.push (B ":true:") st 
				 	  								                                                      else Stack.push (B ":false:") st
				 	  								   |_-> Stack.push or1 st; Stack.push (ERROR ":error:") st)
				 	  |_-> Stack.push (ERROR ":error:") st) 
(*------------------------------------------------------------------------------------------------------------------------------------------------------------*)		
		|"not"->if(Stack.is_empty st) then Stack.push (ERROR ":error:") st
				else (match (Stack.top st) with 
				 	  |B b-> Stack.pop st; if(boolean b) then Stack.push (B ":false:") st
				 	  					                 else Stack.push (B ":true:") st
				 	  |_-> Stack.push (ERROR ":error:") st) 
(*---------------------------------------------------------------------------------------------------------------------------------------------------*)				 	  				
		|"equal"->if(Stack.length st < 2) then Stack.push (ERROR ":error:") st
				  else (match (Stack.top st) with 
				 	  |I i-> let equal = Stack.pop st in (match (Stack.top st) with
				 	  								   |I i1-> Stack.pop st; if (i = i1) then Stack.push (B ":true:") st 
				 	  								                                     else Stack.push (B ":false:") st
				 	  								   |_-> Stack.push equal st; Stack.push (ERROR ":error:") st)
				 	  |_-> Stack.push (ERROR ":error:") st) 
(*------------------------------------------------------------------------------------------------------------------------------------------------------*)				 	  
		|"lessThan"->if(Stack.length st < 2) then Stack.push (ERROR ":error:") st
				     else (match (Stack.top st) with 
				 	  |I i-> let lt = Stack.pop st in (match (Stack.top st) with
				 	  								   |I i1-> Stack.pop st; if (i1 < i) then Stack.push (B ":true:") st 
				 	  								                                     else Stack.push (B ":false:") st
				 	  								   |_-> Stack.push lt st; Stack.push (ERROR ":error:") st)
				 	  |_-> Stack.push (ERROR ":error:") st)
	    |_->()
   in
    
     let rec start (string_list: string list) : unit =   
    match string_list with                                  
    |[]-> ()
                                             
    |hd::tl -> match (String.split_on_char ' ' hd) with
               |[]-> start tl
               |[a]-> oneComm a; start tl

(*------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    							 								
    							 
    		   |[a;b]->  (match a with 
    		   			  |"push"-> if (b = ":error:" || b = ":unit:") then begin Stack.push (P b) st; start tl end 
    		   			  											   else begin Stack.push (ERROR ":error:") st; start tl end
(*---------------------------------------------------------------------------------------------------------------------------------------------------------*)    		   			  											   
    		   			  |"pushi"->if ((is_num b) = false) then begin Stack.push (ERROR ":error:") st; start tl end
    		   			  									else begin Stack.push (I b) st; start tl end 
    		   			  									 
    		   			  |"pushs"->if(b.[0] = '"' && b.[(String.length b) - 1] = '"') then begin Stack.push (S (String.sub b 1 (String.length b - 2))) st; start tl end 
    		   			  															   else begin Stack.push (ERROR ":error:") st; start tl end  
    		   			  																   
    		   			  |"pushn"-> if (is_alpha b.[0] = false && b.[0] != '_') then begin Stack.push (ERROR ":error:") st; start tl end
    		   			  														 else begin Stack.push (N b) st; start tl end
(*-------------------------------------------------------------------------------------------------------------------------------------------------------*)													 
    		   			  |"pushb"-> if (b = ":true:" || b = ":false:") then begin Stack.push (B b) st; start tl end 
    		   			  												else begin Stack.push (ERROR ":error:") st; start tl end
(*------------------------------------------------------------------------------------------------------------------------------------------------------*)    		   			  												
    		   			  |_ -> start tl )
     
    		  |x::y-> (match x with
    		   			 
    		  			 |"pushs"-> let finStr = buildWQ y in if(finStr.[0] = '"' && finStr.[(String.length finStr) - 1] = '"') 
    		  			 		                              then begin let strSP = String.sub finStr 1 (String.length finStr - 2) in  
    		  			 		              					             Stack.push (S strSP) st; start tl end 
    		   			  									  else begin Stack.push (ERROR ":error:") st; start tl end 
    		  			 
    		   			 |_ -> Stack.push (ERROR ":error:") st; start tl )
    in
    
  
  		  start command_lst;
  		  Stack.iter (fun (ERROR p|NOTHING p|S p|I p|N p|B p|P p) -> Printf.fprintf oc "%s\n" p) st 
    	
    ;;
(* use this line to test*)
(* interpreter ("sample_input1.txt", "output.txt") *)
