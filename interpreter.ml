type elem = S of string | I of string | N of string | B of string | P of string | ERROR of string | UNIT of string | NaB

let interpreter ( (inFile : string), (outFile : string )) : unit =
    
  let ic = open_in inFile in

  let oc = open_out outFile in 

  let rec loop_read acc =
      try 
          
          let l = input_line ic in loop_read (l::acc)
      with
      | _of_file -> List.rev acc in

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
 
 let pop lt = 
 	match lt with 
 	|hd::tl-> tl
 	|_-> Printf.printf "%s\n" "empty!"; []
 
 in
 
 let itype comm el1 el2 =
  		  (match comm with 
    		|"add"-> (I (string_of_int((int_of_string el1) + (int_of_string el2))))
    		|"sub"-> (I (string_of_int((int_of_string el2) - (int_of_string el1))))
    		|"mul"-> (I (string_of_int((int_of_string el2) * (int_of_string el1))))
    		|"div"-> if (el1 = "0") then (ERROR ":error:") else (I(string_of_int((int_of_string el2) / (int_of_string el1)))) 
    		|"rem"-> if (el1 = "0") then (ERROR ":error:") else (I(string_of_int((int_of_string el2) mod (int_of_string el1)))) 
    		|"equal"-> if (el1 = el2) then (B ":true:") else (B ":false:") 
			|"lessThan"->if ((int_of_string el2) < (int_of_string el1)) then (B ":true:") else (B ":false:") 
			|_-> (ERROR ":error:"))   
 	in
 
 let btype comm el1 el2 = 
      match comm with 
      |"and"-> if (boolean el1 && boolean el2) then (B ":true:") else (B ":false:") 
	  |"or"->  if (boolean el1 || boolean el2) then (B ":true:") else (B ":false:")
      |_-> (ERROR ":error:")
 in 
 
 let stype comm el1 el2 = 
      match comm with 
      |"cat"-> S(el2 ^ el1) 
      |_-> (ERROR ":error:")
 in  
   
 let rec binding elem1 bl = (*function not working properly*)
 		(match bl with 
 		|[]-> NaB
 		|(a,b)::tl-> (match a with 
 					  |N n-> (match elem1 with 
 					  		 |N n1-> if n = n1 then b else binding elem1 tl
 					  		 |_-> binding elem1 tl)
 					  |_->print_string "nope\n"; binding elem1 tl))
 					 
 		
 in

 let ibtype comm st bl: elem list = 
 		if (List.length st < 2) then (ERROR ":error:")::st  
                 else (match st with
                 	  |top::second::tl-> (match second with
                 	  				 |I i-> (match top with
    							 			 |I j-> ((itype comm j i)::tl)                 
    							 	         |_-> (ERROR ":error:")::st)  
    			                     |B b1-> (match top with
    			      		                  |B b2-> (btype comm b2 b1)::tl                                        
				 	  					      |_-> (ERROR ":error:")::st)
				 	  				 |S s1-> (match top with
    			      		                  |S s2-> (stype comm s2 s1)::tl                                        
				 	  					      |_-> (ERROR ":error:")::st)	    
				 	  			     |N n-> match top with 
				 	  			            |I i2-> (match (binding second bl) with
				 	  			     		        |I i1-> print_string ("reached\n"); (itype comm i2 i1)::tl
				 	  			     		        |NaB->print_string "nab\n"; (ERROR ":error:")::st
				 	  			     		 		|_-> (ERROR ":error:")::st)
				 	  			     		 		
				 	  			     		|S s6-> (match (binding second bl) with
				 	  			     		        |S s5-> print_string ("reached\n"); (stype comm s6 s5)::tl
				 	  			     		        |NaB->print_string "nab\n"; (ERROR ":error:")::st
				 	  			     		 		|_-> (ERROR ":error:")::st)
				 	  			     		 		
				 	  			     		|B b6-> (match (binding second bl) with
				 	  			     		        |B b5-> print_string ("reached\n"); (btype comm b6 b5)::tl
				 	  			     		        |NaB->print_string "nab\n"; (ERROR ":error:")::st
				 	  			     		 		|_-> (ERROR ":error:")::st) 		
				 	  			     		 		 		
				 	  			     		|N n2-> (match binding second bl with
				 	  			     				|I i3-> (match binding top bl with 
				 	  			     			     		|I i4-> (itype comm i4 i3)::tl
				 	  			     			     		|_->(ERROR ":error:")::st)
				 	  			     			     		
				 	  			     			    |S s3-> (match binding top bl with 
				 	  			     			     		|S s4-> (stype comm s4 s3)::tl
				 	  			     			     		|_->(ERROR ":error:")::st)
				 	  			     			     		
				 	  			     			    |B b3-> (match binding top bl with 
				 	  			     			     		|B b4-> (btype comm b4 b3)::tl
				 	  			     			     		|_->(ERROR ":error:")::st)
				 	  			     				 ) 
								 
    			      		         |_-> (ERROR ":error:")::st)  	
    			      		         						 		
    				  |_-> (ERROR ":error:")::st)
 
 in
    
     let rec start (string_list: string list) (st: elem list) (bl: ((elem * elem)) list) : elem list =   
    match string_list with                                  
    |[]-> st
                                             
    |hd::tl -> match (String.split_on_char ' ' hd) with
               |[]-> Printf.printf "%s\n" "reached"; start tl st bl
               |[a]-> (match a with 
               		|"push"-> start tl ((ERROR ":error:")::st) bl
        
(*------------------------------------------------------------------------------------------------------------------------------------------------*)            		   
        			|"pop"-> if ((List.length st) = 0) then start tl ((ERROR ":error:")::st) bl else start tl (pop st) bl				 								
(*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    					|"neg"-> if((List.length st) = 0) then start tl ((ERROR ":error:")::st) bl  
							 else (match (List.hd st) with
				 	  			|I i-> start tl ((I(string_of_int(-int_of_string(i))))::(pop st)) bl
				 	  			|_-> start tl ((ERROR ":error:")::st) bl ) 	
(*----------------------------------------------------------------------------------------------------------------------------------------------------*)				 	  						|"add"-> start tl (ibtype "add" st bl) bl (*if binding then Printf.printf "%s\n" "BINDING" *)
					|"sub"-> start tl (ibtype "sub" st bl) bl 
					|"mul"-> start tl (ibtype "mul" st bl) bl
					|"div"-> start tl (ibtype "div" st bl) bl
					|"rem"-> start tl (ibtype "rem" st bl) bl
					|"equal"-> start tl (ibtype "equal" st bl) bl
					|"lessThan"-> start tl (ibtype "lessThan" st bl) bl
					|"and"-> start tl (ibtype "and" st bl) bl
					|"or"-> start tl (ibtype "or" st bl) bl
					|"cat"-> start tl (ibtype "cat" st bl) bl
		
					|"swap"-> if(List.length st < 2) then begin start tl ((ERROR ":error:")::st) bl end
				  			  else (match st with
				  			  	   |h::h1::t-> start tl (h1::h::t) bl )
(*------------------------------------------------------------------------------------------------------------------------------------------------------------*)		
					|"not"->if(List.length st = 0) then start tl ((ERROR ":error:")::st) bl 
							else (match (List.hd st) with 
				 	  			  |B b-> if(boolean b) then start tl ((B ":false:")::(pop st)) bl 
				 	  					                 else start tl ((B ":true:")::(pop st)) bl 
				 	  			  |_-> start tl ((ERROR ":error:")::st) bl) 
(*---------------------------------------------------------------------------------------------------------------------------------------------------*)				 	  		       
	    			|"bind"-> if(List.length st < 2) then start tl ((ERROR ":error:")::st) bl
	    				      else (match st with
	    				      		|[]-> start tl st bl 
	    				 	        |top::second::n-> (match second with
								 	        		   |N n1-> (match top with 
								 	        		   		   |B b-> start tl ((UNIT ":unit:")::n) ((second, top)::bl)
							 	  	 				  		   |I i-> start tl ((UNIT ":unit:")::n) ((second, top)::bl) 
							 	  	 				  		   |S s-> start tl ((UNIT ":unit:")::n) ((second, top)::bl)
							 	  	 				  		   |N n2-> (match binding top bl with
							 	  	 				  		   		   |B b1-> start tl ((UNIT ":unit:")::n) ((second, (B b1))::bl)
							 	  	 				  		   		   |I i1-> start tl ((UNIT ":unit:")::n) ((second, (I i1))::bl) 
							 	  	 				  		   		   |S s1-> start tl ((UNIT ":unit:")::n) ((second, (S s1))::bl) 
							 	  	 				  		   		   |UNIT u1-> start tl ((UNIT ":unit:")::n) ((second, (UNIT u1))::bl)
							 	  	 				  		   		   |_-> start tl ((ERROR ":error:")::st) bl )
							 	  	 				  		   |UNIT u-> start tl ((UNIT ":unit:")::n) ((second, top)::bl)
								 	        		   		   |_->start tl ((ERROR ":error:")::st) bl )
								 	        		  
							 	  	 				   |_-> start tl ((ERROR ":error:")::st) bl )
						 	  	 
				 	     
				 	  	 
				 	  	            |_-> start tl st bl )
				 	  	            
				   |"quit"-> start tl st bl
				   |_-> start tl ((ERROR ":error:")::st) bl
	     )

(*------------------------------------------------------------------------------------------------------------------------------------------------------------------*)    							 													 
    		   |[a;b]->  (match a with 
    		   			  |"push"-> if (b = ":error:" || b = ":unit:") then start tl ((P b)::st) bl 
    		   			  											   else start tl ((ERROR ":error:")::st) bl
(*---------------------------------------------------------------------------------------------------------------------------------------------------------*)    		   			  											   
    		   			  |"pushi"->if ((is_num b) = false) then start tl ((ERROR ":error:")::st) bl else start tl ((I b)::st) bl  
    		   			  									 
    		   			  |"pushs"->if(b.[0] = '"' && b.[(String.length b) - 1] = '"') then start tl ((S (String.sub b 1 (String.length b - 2)))::st) bl  
    		   			  															   else start tl ((ERROR ":error:")::st) bl  
    		   			  																   
    		   			  |"pushn"-> if (is_alpha b.[0] = true || b.[0] = '_') then start tl ((N b)::st) bl 
    		   			  													   else start tl ((ERROR ":error:")::st) bl 
(*-------------------------------------------------------------------------------------------------------------------------------------------------------*)													 
    		   			  |"pushb"-> if (b = ":true:" || b = ":false:") then start tl ((B b)::st) bl 
    		   			  												else start tl ((ERROR ":error:")::st) bl
(*------------------------------------------------------------------------------------------------------------------------------------------------------*)    		   			  												
    		   			  |_ -> start tl st bl)
     
    		  |x::y-> (match x with
    		   			 
    		  			 |"pushs"-> let finStr = buildWQ y in if(finStr.[0] = '"' && finStr.[(String.length finStr) - 1] = '"') 
    		  			 		                              then let strSP = String.sub finStr 1 (String.length finStr - 2) in  
    		  			 		              					             start tl ((S strSP)::st) bl  
    		   			  									  else start tl ((ERROR ":error:")::st) bl  
    		  			 
    		   			 |_ -> start tl ((ERROR ":error:")::st) bl )
    		   			 
    in

  		 let st = start (command_lst) ([]) ([]) in 
  		  List.iter (fun (UNIT p|ERROR p|S p|I p|N p|B p|P p) -> Printf.fprintf oc "%s\n" p) st ;	
    ;;
 interpreter ("sample_input1.txt", "output.txt")
