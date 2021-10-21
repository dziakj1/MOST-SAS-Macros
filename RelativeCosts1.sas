%MACRO RelativeCosts1(	number_of_factors=,
						desired_fract_resolution=4,
						min_target_d_per_factor=.2,
						condition_costlier_than_subject=1,  /* should be 1 if true, 0 if false;*/
						max_cost_ratio= 100,
						num_graph_points = 20,
						max_graph_ratio = 5) ;
	/* Macro by John Dziak for The Methodology Center, 
	   The Pennsylvania State University
	   Jan. 2, 2009   */
	%MACRO PrintSizes;	
		PRINT("Doing separate experiments on each of the &number_of_factors factors requires at least:");
		PRINT(CONCAT(	CHAR(separate_expers_subjects[1],10,0),
						" subjects total, i.e. ",
						CHAR(separate_expers_sub_per_cell[1],5,0),
						" subjects in each of ",
						CHAR(separate_expers_conditions[1],5,0),
						" cells." ));
		PRINT(""); 
		PRINT("A comparative setup with &number_of_factors groups plus a control group requires at least:");
		PRINT(CONCAT(	CHAR(single_factor_subjects[1],10,0),
						" subjects total, i.e. ",
						CHAR(single_factor_sub_per_cell[1],5,0),
						" subjects in each of ",
						CHAR(single_factor_conditions[1],5,0),
						" cells." ));
		PRINT(""); 
		PRINT("A 2^&number_of_factors complete factorial experiment requires at least:");
		PRINT(CONCAT(	CHAR(complete_factorial_subjects[1],10,0),
						" subjects total, i.e. ",
						CHAR(complete_factorial_sub_per_cell[1],5,0),
						" subjects in each of ",
						CHAR(complete_factorial_conditions[1],5,0),
						" cells." ));
		PRINT(""); 
		PRINT("A resolution &desired_fract_resolution fractional factorial experiment with &number_of_factors factors requires at least:");
		PRINT(CONCAT(	CHAR(fract_factorial_subjects[1],10,0),
						" subjects total, i.e. ",
						CHAR(fract_factorial_sub_per_cell[1],5,0),
						" subjects in each of ",
						CHAR(fract_factorial_conditions[1],5,0),
						" cells." ));
		PRINT(""); 
	%MEND;
	%PUT &condition_costlier_than_subject;
	%PUT %EVAL(&condition_costlier_than_subject);
	%LET input_error = 0; 
	%IF &desired_fract_resolution < %EVAL(3) %THEN %DO; 
		%PUT "ERROR! The requested resolution is too low.";
		%LET input_error=1; 
	%END;
	%IF &desired_fract_resolution > %EVAL(6) %THEN %DO;
		%PUT "ERROR! The requested resolution is too high.";
		%LET input_error=1; 
	%END;
	%IF &max_cost_ratio < %EVAL(1)           %THEN %DO; 
		%PUT "ERROR! The requested maximum cost ratio is too low.";
		%LET input_error=1; 
	%END;
	%IF &min_target_d_per_factor < %SYSEVALF(0.01)  %THEN %DO; 
		%PUT "ERROR! The requested target effect size is too low.";
		%PUT &min_target_d_per_factor ;
		%LET input_error=1; 
	%END;
	%IF &number_of_factors < %EVAL(2)        %THEN %DO; 
		%PUT "ERROR! The requested number of factors is too low.";
		%LET input_error=1; 
	%END;
	%IF &number_of_factors > %EVAL(20)       %THEN %DO; 
		%PUT "ERROR! The requested number of factors is too high.";
		%LET input_error=1; 
	%END;
	%IF &input_error=1 %THEN %DO; 
									%PUT "ERROR! Invalid input to this macro -- results highly suspect"; 
									%ABORT;
							%END;
	%LOCAL variance_adjustment;
	%LET variance_adjustment = 1; /* for now; may allow pretest in the future;*/
	PROC IML;
		/*  Number of cells needed to get a given resolution                         */
		/*                     Resolution                                            */
		/*     #Factors	2		 3		4		5		6			Full  ;          */
		/******************************************************************;         */
		M =	{	 1		2		 2		2		2		2			  2	    ,
				 2		2		 4		4		4		4		      4	    ,
				 3		2		 4		8		8		8		      8	    ,
				 4		2		 8		8		16		16		     16	    ,
				 5		2		 8		16		16		32		     32	    ,
				 6		2		 8		16		32		32		     64	    ,
				 7		2		 8		16		64		64		    128	    ,
				 8		2		16		16		64		128		    256	    ,
				 9		2		16		32		128		128		    512	    ,
				10		2		16		32		128		256		   1024	    ,
				11		2		16		32		128		256		   2048	    ,
				12		2		16		32		256		256		   4096	    ,
				13		2		16		32		256		512		   8192	    ,
				14		2		16		32		256		512		  16384	    ,
				15		2		16		32		256		512		  32768	    ,
				16		2		32		32		256		512		  65536	    ,
				17		2		32		64		256		512		 131072	    ,
				18		2		32		64		512		512		 262144	    ,
				19		2		32		64		512		1024	 524288	    ,
				20		2		32		64		512		1024	1048576	    
			};
		fract_conditions = M[&number_of_factors,&desired_fract_resolution];
		CREATE n_conditions_and_subjects VAR {fract_conditions};
		APPEND;
	QUIT;
	DATA n_conditions_and_subjects; 
		SET n_conditions_and_subjects;
		n_star = CEIL(2*&variance_adjustment*( 0.841621 + 1.959964)**2/(&min_target_d_per_factor**2));
		separate_expers_conditions = &number_of_factors*2;
		single_factor_conditions = &number_of_factors+1;
		complete_factorial_conditions = 2**&number_of_factors;

		fract_factorial_conditions = fract_conditions;
		separate_expers_subjects = n_star * separate_expers_conditions;
		single_factor_subjects = n_star * single_factor_conditions;
		complete_factorial_min = 2*n_star;
		complete_factorial_subjects = CEIL(complete_factorial_min/
											complete_factorial_conditions)*
										complete_factorial_conditions;
		fract_factorial_min = 2*n_star;
		fract_factorial_subjects = CEIL(fract_factorial_min/
											fract_factorial_conditions)*
										fract_factorial_conditions;
		separate_expers_sub_per_cell = separate_expers_subjects/
											separate_expers_conditions;
		single_factor_sub_per_cell = single_factor_subjects/
											single_factor_conditions;
		complete_factorial_sub_per_cell = complete_factorial_subjects/
												complete_factorial_conditions;
		fract_factorial_sub_per_cell = fract_factorial_subjects/
												fract_factorial_conditions;
		temp=1;
		OUTPUT;
	RUN;
	PROC IML;
		RESET NOCENTER NOLOG PRINTADV=0;
		USE n_conditions_and_subjects;
		READ ALL; 
		RESET LOG; %PrintSizes; RESET NOLOG; %PrintSizes; 
	QUIT;
	DATA cost_ratio_for_graph_macro;
		DO I = 0 TO &num_graph_points;
			temp = 1;
			cost_ratio_for_graph_macro = &max_cost_ratio*I/10;
			OUTPUT;
		END;
		DROP I;
	RUN;
	DATA calcs_for_graph_macro;
		MERGE cost_ratio_for_graph_macro n_conditions_and_subjects;
		BY temp;
		DROP temp;
		%IF %EVAL(&condition_costlier_than_subject) %THEN %DO;
			separate_expers_cost     =  separate_expers_subjects +
									    separate_expers_conditions *
										cost_ratio_for_graph_macro;
			single_factor_cost       = 	single_factor_subjects +
								    	single_factor_conditions *
										cost_ratio_for_graph_macro;
			complete_factorial_cost   = complete_factorial_subjects +
										complete_factorial_conditions *
										cost_ratio_for_graph_macro;
			fract_factorial_cost   = fract_factorial_subjects +
										fract_factorial_conditions *
										cost_ratio_for_graph_macro;
		%END;
		%ELSE %DO;
			separate_expers_cost     =  separate_expers_conditions +
									    separate_expers_subjects *
										cost_ratio_for_graph_macro;
			single_factor_cost       = 	single_factor_conditions +
								    	single_factor_subjects *
										cost_ratio_for_graph_macro;
			complete_factorial_cost   = complete_factorial_conditions +
										complete_factorial_subjects *
										cost_ratio_for_graph_macro;
			fract_factorial_cost   = 	fract_factorial_conditions +
										fract_factorial_subjects *
										cost_ratio_for_graph_macro;
		%END;
	RUN;
	PROC IML;
		%* Remove parts of the graph that represent unrealistic design options;
		USE calcs_for_graph_macro;
		READ ALL ;
		CLOSE calcs_for_graph_macro;
		max_costs = J(4,1,0);
		temp = &num_graph_points * 100000 + 42;
		max_costs[1] = separate_expers_cost[&num_graph_points];
		max_costs[2] = single_factor_cost[&num_graph_points];
		max_costs[3] = complete_factorial_cost[&num_graph_points];
		max_costs[4] = fract_factorial_cost[&num_graph_points];
		q = max_costs;
		min_max_cost = max_costs[><];
		max_max_cost = max_costs[<>];
		max_max_cost_to_show = (min_max_cost*&max_graph_ratio)
								>< max_max_cost; 
		IF separate_expers_cost[<>] > max_max_cost_to_show THEN 
			separate_expers_cost[LOC(separate_expers_cost
								  > max_max_cost_to_show)] = .;
		IF single_factor_cost[<>] > max_max_cost_to_show THEN 
			single_factor_cost[LOC(single_factor_cost
								  > max_max_cost_to_show)] = .;
		IF complete_factorial_cost[<>] > max_max_cost_to_show THEN 
			complete_factorial_cost[LOC(complete_factorial_cost
								  > max_max_cost_to_show)] = .;
		IF fract_factorial_cost[<>] > max_max_cost_to_show THEN 
			fract_factorial_cost[LOC(fract_factorial_cost
								  > max_max_cost_to_show)] = .;
		EDIT calcs_for_graph_macro;
		REPLACE ALL ;
		CLOSE calcs_for_graph_macro;
	RUN;
	PROC GPLOT DATA=calcs_for_graph_macro;
		AXIS1 LABEL=("Cost");
		%IF %EVAL(&condition_costlier_than_subject) %THEN %DO;
			AXIS2 LABEL=("Overhead cost per condition");
			NOTE J=L "relative to per-subject cost fixed at 1 unit";
			TITLE1 "Costs of Different Approaches"; 
			LABEL separate_expers_cost="&number_of_factors separate experiments" 
				  single_factor_cost="Single factor, &number_of_factors+1 levels" 
				  complete_factorial_cost="Complete 2^&number_of_factors factorial" 
				  fract_factorial_cost="Fract. fact., &number_of_factors factors, resol.&desired_fract_resolution" 		
				  cost_ratio_for_graph_macro="Per-subject cost (relative to per-condition cost=$1)";
		%END;
		%ELSE %DO;
			AXIS2 LABEL=("Cost per individual subject");
			NOTE J=L "relative to per-condition overhead cost fixed at 1 unit";
			TITLE1 "Costs of Different Approaches"; 
			LABEL separate_expers_cost="&number_of_factors separate experiments" 
				  single_factor_cost="Single factor, &number_of_factors+1 levels" 
				  complete_factorial_cost="Complete 2^&number_of_factors factorial" 
				  fract_factorial_cost="Fract. fact., &number_of_factors factors, resol.&desired_fract_resolution" 		
				  cost_ratio_for_graph_macro="Per-subject cost (relative to per-condition cost=$1)";
		%END;
		SYMBOL1 I=JOIN V=SQUARE C=black H=.5;
		SYMBOL2 I=JOIN V=TRIANGLE C=red H=.5;
		SYMBOL3 I=JOIN V=CIRCLE C=green H=.5;
		SYMBOL4 I=JOIN V=STAR C=blue H=.5; 
		LEGEND1  LABEL=("");
		PLOT   separate_expers_cost*cost_ratio_for_graph_macro=1  
	           single_factor_cost*cost_ratio_for_graph_macro=2  
		       complete_factorial_cost*cost_ratio_for_graph_macro=3  
	  	       fract_factorial_cost*cost_ratio_for_graph_macro=4 /
					OVERLAY LEGEND=LEGEND1 HAXIS=AXIS2 VAXIS=AXIS1;
	RUN;
	QUIT;
/*	%symdel(number_of_factors);
	%symdel(desired_fract_resolution);
	%symdel(min_target_d_per_factor);
	%symdel(condition_costlier_than_subject);
	%symdel(max_cost_ratio);
	%symdel(num_graph_points);
	%symdel(max_graph_ratio);
	%symdel(input_error);
	%symdel(variance_adjustment);*/
%MEND;
