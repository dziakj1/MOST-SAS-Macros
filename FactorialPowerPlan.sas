%MACRO FactorialPowerPlan( alpha = .05,
                           assignment = unclustered,
                           change_score_icc =,
                           cluster_size =,
                           cluster_size_sd =,
                           d_main =,
                           effect_size_ratio =,
                           icc =,
                           model_order = 1,
                           nclusters =,
                           nfactors = 1,
                           ntotal =,
                           power = ,
                           pre_post_corr =,
                           pretest =,
                           raw_coef =,
                           raw_main =,
                           sigma_y = ,
                           std_coef =);
    /** FactorialPowerPlan Macro
        Revised August 20, 2013
        By John J. Dziak, The Methodology Center
        (c) 2013 Pennsylvania State University **/
    %LET ErrorMessage = ;
    /** Define some internal support functions **/ 
    %MACRO GetNumeric(name); 
        IF (LENGTHN(" &&&name")<1) THEN DO;
            has_specified_&name = 0;
            &name = .;
        END; ELSE DO;
            has_specified_&name = 1;
            IF (INDEX(" &&&name",".")=0) THEN DO;
                &name = INPUT(" &&&name",15.0);
            END; ELSE DO;
                &name = INPUT(" &&&name",15.6);
            END;
        END;
    %MEND;
    %MACRO GetString(name); 
        IF (LENGTHN(" &&&name")<1) THEN DO;
            has_specified_&name = 0;
        END; ELSE DO;
            has_specified_&name = 1;
        END;
        input_&name = STRIP("%TRIM(&&&name)");
        input_&name = LOWCASE(input_&name);
    %MEND;
    %MACRO ProcessInput;
        /* How many effect size measures did the user specify? */
        num_effect_sizes_specified = has_specified_d_main +
                                      has_specified_effect_size_ratio +
                                      has_specified_raw_coef +
                                      has_specified_raw_main +
                                      has_specified_std_coef;
        has_specified_some_effect_size = 1*(num_effect_sizes_specified>0);
        IF num_effect_sizes_specified>1 THEN DO;
            MacroError = 1;
            DataErrorCode = 1;
        END;
        /* How many sample size measures did the user specify? */
        num_sample_sizes_specified = has_specified_ntotal + 
                                     has_specified_nclusters;
        has_specified_some_sample_size = 1*(num_sample_sizes_specified>0);
        IF num_sample_sizes_specified>1 THEN DO;
            MacroError = 1;
            DataErrorCode = 2;
        END;
        /* How many of the "big three" pieces of information 
           (effect size measure, target power, sample size measure)
           did the user specify? */
        to_find = "????????????????";
        IF (has_specified_some_effect_size=0) THEN DO;
            to_find = "effect_size";
        END;
        IF (has_specified_power=0) THEN DO;
            to_find = "power";
        END;
        IF (has_specified_some_sample_size=0) THEN DO;
            to_find = "sample_size";
        END; 
        num_big_three_specified = has_specified_some_effect_size + 
                                  has_specified_power +
                                  has_specified_some_sample_size;
        IF num_big_three_specified^=2 THEN DO;
            to_find = "????????????????";
            MacroError = 1;
            DataErrorCode = 3;
        END;
        /* What type of assignment did the user specify? */
        assignment = "????????????????"; 
        IF ( input_assignment = "unclustered" |
              input_assignment = "independent" |
              input_assignment = "" |
              input_assignment = " " ) THEN DO;
            assignment = "unclustered";
        END; ELSE IF ((input_assignment = "betweenclusters")|
                      (input_assignment = "between_clusters")|
                      (input_assignment = "between")) THEN DO;
            assignment = "between_clusters";
        END; ELSE IF ((input_assignment = "withinclusters")|
                      (input_assignment = "within_clusters")|
                      (input_assignment = "within")) THEN DO; 
            assignment = "within_clusters ";
       /* END; ELSE IF ((input_assignment = "partially_nested")|
                      (input_assignment = "partly_nested")) THEN DO; 
            assignment = "partially_nested";
        END; ELSE IF ((input_assignment = "created_clusters")|
                      (input_assignment = "posttest_clusters")) THEN DO; 
            assignment = "created_clusters"; */
        END; ELSE DO; 
            MacroError = 1;
            DataErrorCode = 4;
        END;
        IF assignment = "unclustered" THEN DO;
            IF (has_specified_cluster_size=1) THEN DO;
                MacroError = 1;
                DataErrorCode = 5;
            END;  
            IF (has_specified_cluster_size_SD=1) THEN DO;
                MacroError = 1;
                DataErrorCode = 6;
            END;  
            IF (has_specified_nclusters=1) THEN DO;
                MacroError = 1;
                DataErrorCode = 7;
            END;  
        END;
        IF assignment = "within_clusters" THEN DO;
            IF (has_specified_cluster_size_SD=1) THEN DO;
                MacroError = 1;
                DataErrorCode = 20;
            END;
        END;
        /* What type of pretest analysis did the user specify? */
        pretest = "????????????????"; 
        IF (  (input_pretest = "no_pretest") |
              (input_pretest = "none") |
              (input_pretest = "no") |
              (input_pretest = "") |
              (input_pretest = " ") ) THEN DO;
              pretest    = "none";
        END; ELSE IF ((input_pretest = "covariate")|
                      (input_pretest = "ancova")) THEN DO;
            pretest = "covariate";
        END; ELSE IF ((input_pretest = "yes") |
                      (input_pretest = "repeated") |
                      (input_pretest = "repeated_measure") |
                      (input_pretest = "repeated_measures")) THEN DO; 
            pretest = "repeated_measure";
        END; ELSE DO; 
            MacroError = 1;
            DataErrorCode = 8;
        END;
        /* Has the user nonsensically input blanks to replace arguments that already have defaults? */
        IF (has_specified_alpha = 0) THEN DO;
            MacroError = 1;
            DataErrorCode = 9;
        END;
        IF (has_specified_model_order = 0) THEN DO;
            MacroError = 1;
            DataErrorCode = 10;
        END;
        IF (has_specified_nfactors = 0) THEN DO;
            MacroError = 1;
            DataErrorCode = 11;
        END;
        /* Handle the possibility of missing sigma_y.*/
        /* If the user provides a standardized measure, then pretend the variance is 1. */      
        /* If the user doesn't specify the variance, but only wants detectable effect */
        /* sizes, then treat the variance as 1.*/ 
        /* Otherwise call an error. */
        IF (has_specified_sigma_y = 0) THEN DO;
            IF (to_find = "effect_size" | 
                has_specified_d_main |
                has_specified_std_coef |
                has_specified_effect_size_ratio) THEN DO;  
                sigma_y = 1.0;
            END; ELSE DO;
                MacroError = 1;
                DataErrorCode = 12; 
            END; 
        END; 
        /* Is any of the input nonsensical? */
        IF ((model_order<1)|(model_order>10)) THEN DO;
            MacroError = 1;
            DataErrorCode = 13;
        END;
        IF ((nfactors<1)|(nfactors>99)) THEN DO;
            MacroError = 1;
            DataErrorCode = 14;
        END;
        IF (model_order>nfactors) THEN DO;
            MacroError = 1;
            DataErrorCode = 15;    
        END;
        IF (pretest^="none") THEN DO;
            IF (has_specified_pre_post_corr=1) THEN DO;
                IF ((pre_post_corr<0)|(pre_post_corr>=1)) THEN DO;
                    MacroError = 1;
                    DataErrorCode = 16;
                END;
            END; ELSE DO;
                MacroError = 1;
                DataErrorCode = 17; 
            END;
        END;
        /* Calculate "sigma_effective" */
        IF (assignment="unclustered") THEN DO;
            IF pretest="none" THEN sigma_effective = sigma_y;
            IF pretest="covariate" THEN DO; 
                sigma_effective = sigma_y*SQRT(1-pre_post_corr**2);
            END;
        END;
        IF ((assignment="between_clusters")|(assignment="within_clusters")) THEN DO;
            IF (pretest="none") THEN DO;
                /* In this case, error variance and marginal variance are equal. */
                sigma_effective = sigma_y;
            END; ELSE IF (pretest="covariate") THEN DO;
                IF (assignment="between_clusters") THEN DO;
                    MacroError = 1;
                    DataErrorCode = 19;
                END; ELSE DO;
                    sigma_effective = sigma_y*SQRT(1-pre_post_corr**2);
                END;
            END; ELSE IF (pretest="repeated_measure") THEN DO;
                sigma_effective = sigma_y;
            END;
        END; 
        /* Calculate "the_coef" */      
        IF has_specified_d_main THEN DO; the_coef = .5*d_main*sigma_y; END;
        IF has_specified_std_coef THEN DO; the_coef = std_coef*sigma_y; END;
        IF has_specified_effect_size_ratio THEN DO; the_coef = SQRT(effect_size_ratio); END;
        IF has_specified_raw_coef THEN the_coef = raw_coef;
        IF has_specified_raw_main THEN the_coef = .5*raw_main;
        the_coef = ABS(the_coef);
    %MEND;
    %MACRO CalculateNumberRegressionCoefs;
        nregcoefs = 1; /* start with intercept */
        DO k = 1 TO model_order;
            nregcoefs = nregcoefs + COMB(nfactors,k);
                       /* add in number of k-way effects */
        END;
        IF ((pretest="covariate")&
            (assignment^="betweenclusters")) THEN DO;
            nregcoefs = nregcoefs + 1; 
                       /* add one for the covariate, if there is one and if
                          it is at a measurement level at which the extra df 
                          appreciably affects the power. */
        END;
    %MEND;
    /** Handle the input **/
    DATA fpp_inputs;
        FILE PRINT;
        /** Read in the input **/
        %GetString(assignment);
        %GetString(pretest);
        %GetNumeric(alpha);
        %GetNumeric(change_score_icc);
        %GetNumeric(cluster_size);
        %GetNumeric(cluster_size_sd);
        %GetNumeric(d_main);
        %GetNumeric(effect_size_ratio);
        %GetNumeric(icc);
        %GetNumeric(model_order);
        %GetNumeric(nclusters);
        %GetNumeric(nfactors);
        %GetNumeric(ntotal);
        %GetNumeric(pre_post_corr);
        %GetNumeric(raw_coef);
        %GetNumeric(raw_main);
        %GetNumeric(sigma_y);
        %GetNumeric(std_coef);
        %GetNumeric(power);
        MacroError = 0;
        DataErrorCode = .;
        %ProcessInput; 
        /**************************************/
        %CalculateNumberRegressionCoefs;
        DROP num_big_three_specified k;
     RUN;
     /**************************************/
     PROC IML;
        FILE PRINT;
        START HandleDataErrors(DataErrorCode);
            PUT "Macro error " DataErrorCode;
            IF DataErrorCode = 0 THEN DO;
                PUT "Error: Can't do that yet.";
            END;
            IF DataErrorCode = 1 THEN DO;
                PUT "Error: Only one effect size measure should be used.";
            END;
            IF DataErrorCode = 2 THEN DO;
                PUT "Error: Only one sample size measure should be used.";
            END;
            IF DataErrorCode = 3 THEN DO;
                PUT "Error: Exactly two of the following should be specified:";
                PUT "an effect size, a sample size, and a power.  The third will be";
                PUT "the result to be calculated.";
            END;
            IF DataErrorCode = 4 THEN DO;
                PUT "Error: Could not understand the specified assignment type.";
            END;
            IF DataErrorCode = 5 THEN DO;
                PUT "Error:  If the subjects are not clustered then please do";
                PUT "not specify a cluster size.";
            END;
            IF DataErrorCode = 6 THEN DO;
                PUT "Error:  If the subjects are not clustered then please do";
                PUT "not specify a cluster size standard deviation.";
            END;
            IF DataErrorCode = 7 THEN DO;
                PUT "Error:  If the subjects are not clustered then please do";
                PUT "not specify a number of clusters.";
            END;
            IF DataErrorCode = 8 THEN DO;
                PUT "Macro error: Could not understand the specified pretest type.";
            END;
            IF DataErrorCode = 9 THEN DO;
                PUT "Macro error: Please specify a non-blank alpha.";
            END;
            IF DataErrorCode = 10 THEN DO;
                PUT "Macro error: Please specify a non-blank model order.";
            END;
            IF DataErrorCode = 11 THEN DO;
                PUT "Macro error: Please specify a non-blank number of factors.";
            END;
            IF DataErrorCode = 12 THEN DO;
                PUT "Macro error: Please specify a non-blank sigma_y.";
            END;
            IF DataErrorCode = 13 THEN DO;
                PUT "Please choose a different value for model_order.";
            END;
            IF DataErrorCode = 14 THEN DO;
                PUT "Please choose a different value for nfactors.";
            END;
            IF DataErrorCode = 15 THEN DO;
                PUT "Macro error: The specified model order is too high for the specified ";
                PUT "number of factors.";  
            END;
            IF DataErrorCode = 16 THEN DO;
                PUT "Macro error: The pretest-posttest correlation should be between 0 and 1.";
            END;
            IF DataErrorCode = 17 THEN DO;
                PUT "Macro error: The pretest-posttest correlation needs to be specified."; 
            END;
            IF DataErrorCode = 19 THEN DO;
                PUT "Macro error: Not able to calculate power for this model setup."; 
            END;
            IF DataErrorCode = 20 THEN DO;
                PUT "Error:  If assignment is within clusters then please do";
                PUT "not specify a cluster size standard deviation.";
            END;
            PUT "------------------------------------------------------------"; 
            ABORT;
        FINISH HandleDataErrors;
        START CalculatePowerUnclustered( alpha,
                                      nfactors,
                                      nregcoefs,
                                      ntotal,
                                      sigma_effective,
                                      the_coef,
                                      silent);     
             num = ntotal*(the_coef**2);
             den = sigma_effective**2;
             lambda = num/den;
             IF (lambda = .) THEN DO;
                 PUT "Macro error: Could not calculate noncentrality parameter.";
                 ABORT;
             END;
             /* Would this be more accurate if it was 
                  lambda = ((ntotal-nregcoefs)*(the_coef**2))/(sigma_effective**2);
                  analogously to Cohen (1988)? */
             df1 = 1;
             df2 = ntotal-nregcoefs;
             IF (df2 < 1) THEN DO;
                 PUT "Macro error: The sample size would be too small to fit the model.";
                 ABORT;
             END;
             crit = FINV(1-alpha,df1,df2,0);
             power = ROUND(1-PROBF(crit,df1,df2,MIN(100,lambda)),.0001);
             IF (silent=0) THEN DO;
                 PUT "The calculated power is" power;
                 ncells = 2**nfactors;
                 IF (ntotal<ncells) THEN DO;
                     PUT "However, a complete factorial requires at least " ncells " subjects.";
                 END;
             END;
             RETURN(power);
        FINISH CalculatePowerUnclustered;
        START CalculateNUnclustered( alpha,
                                     nfactors,
                                     nregcoefs,
                                     power,
                                     sigma_effective,
                                     the_coef );                                          
            bottom = nregcoefs+1;
            top = 10000000;
            BinarySearchDone = 0;
            DO WHILE(BinarySearchDone = 0);
                middle = CEIL(bottom+.5*(top-bottom));
                fmiddle = CalculatePowerUnclustered( alpha,
                                                     nfactors,
                                                     nregcoefs,
                                                     middle, /* for ntotal */
                                                     sigma_effective,
                                                     the_coef,
                                                     1 );  
                IF middle=top THEN DO; 
                     BinarySearchDone = 1;
                END; ELSE DO; 
                    IF (fmiddle<power) THEN DO; bottom = middle; END;
                    IF fmiddle = power THEN DO; bottom = middle; top=middle; END;
                    IF (fmiddle>power) THEN DO; top = middle; END;
                END; 
            END;
            ntotal = middle; 
            PUT "The calculated sample size is" ntotal " subjects.";
            ncells = 2**nfactors;
            IF (ntotal<ncells) THEN DO;
                PUT "However, a complete factorial requires at least " ncells " subjects.";
            END; 
            RETURN(ntotal);
        RUN;
        FINISH CalculateNUnclustered;
        START CalculateDetectableUnclustered( alpha,
                                     nfactors,
                                     nregcoefs,
                                     ntotal,
                                     power,
                                     sigma_effective,
                                     sigma_y);                                          
            bottom = 0;
            top = 5*sigma_y;
            BinarySearchDone = 0;
            DO WHILE(BinarySearchDone = 0);
                middle = bottom+.5*(top-bottom);
                fmiddle = CalculatePowerUnclustered( alpha,
                                                  nfactors,
                                                  nregcoefs,
                                                  ntotal,
                                                  sigma_effective,
                                                  middle, /* for the_coef */
                                                  1);  
                IF (ABS(middle-top)<1e-5) THEN DO; 
                     BinarySearchDone = 1;
                END; ELSE DO; 
                    IF (fmiddle<power) THEN DO; bottom = middle; END;
                    IF fmiddle = power THEN DO; bottom = middle; top=middle; END;
                    IF (fmiddle>power) THEN DO; top = middle; END;
                END; 
            END;
            the_coef = middle;
            IF fmiddle<(power-.01) THEN DO;
                PUT "Macro error: Could not find a detectable effect size";
                ABORT; 
            END;
            PUT "The detectable effect size is estimated as follows:";
            IF ((sigma_y < 0.999999)|(sigma_y > 1.000001)) THEN DO;
                /*******/
                calculated_raw_coef = the_coef;
                PUT "  As an unstandardized regression coefficient for either";
                PUT "  a main effect or an interaction:" @60 calculated_raw_coef 8.4;
                /*******/
                calculated_raw_main = 2*the_coef;
                PUT "  As an unstandardized mean difference for a main effect:"
                   @60 calculated_raw_main 8.4;
                /*******/
                calculated_raw_ixn = 4*the_coef; 
                PUT "  As an unstandardized difference in differences for ";
                PUT "  a 2-way interaction:" @60 calculated_raw_ixn  8.4;
                /*******/
            END;
            calculated_std_coef = the_coef/sigma_y;
            PUT "  As a standardized regression coefficient for either";
            PUT "  a main effect or an interaction:"  @60 calculated_std_coef 8.4;  
            /*******/
            calculated_d_main = 2*the_coef/sigma_y;
            PUT "  As a standardized mean difference (Cohen d) for a ";
            PUT "  main effect:" @60 calculated_d_main 8.4;
            /*******/
            calculated_d_ixn = 4*the_coef/sigma_y;
            PUT "  As a standardized difference in differences for ";
            PUT "  a 2-way interaction:" @60 calculated_d_ixn 8.4;
            /*******/
            calculated_effect_size_ratio = (the_coef**2)/(sigma_y**2);
            PUT "  As a standardized effect size ratio (Cohen f squared) ";
            PUT "  for a main effect or interaction:" 
               @60 calculated_effect_size_ratio 8.4;          
            /*******/
            ncells = 2**nfactors;
            IF (ntotal<ncells) THEN DO;
                PUT "However, a complete factorial requires at least " ncells " subjects.";
            END;
            RETURN(the_coef);
        RUN;
        FINISH CalculateDetectableUnclustered;
        START CalculatePowerTwoLevel( alpha,
                                      assignment,
                                      cluster_size,
                                      cluster_size_sd,
                                      icc,
                                      nclusters,
                                      nfactors,
                                      nregcoefs,
                                      sigma_effective,
                                      the_coef,
                                      silent );
             num = cluster_size*nclusters*the_coef*the_coef;
             cluster_size_cv = cluster_size_sd/cluster_size;
             IF (assignment="between_clusters") THEN DO;
                 nadj = cluster_size*(1+cluster_size_cv**2);
                 deff = (1+(nadj-1)*icc);
                 df2 = nclusters - nregcoefs;
             END; ELSE DO;
                 deff = 1;
                 df2 = FLOOR(nclusters*cluster_size) - nregcoefs; 
             END;
             IF (df2 < 1) THEN DO;
                 PUT "Macro error: The sample size would be too small to fit the model.";
                 ABORT;
             END;             
             den = (sigma_effective**2)*deff;
             lambda = num/den;
             IF (lambda = .) THEN DO;
                 PUT "Macro error: Could not calculate noncentrality parameter.";
                 ABORT;
             END;
             df1 = 1;
             crit = FINV(1-alpha,df1,df2,0); 
             power = ROUND(1-PROBF(crit,df1,df2,MIN(100,lambda)),.0001);
             IF (silent=0) THEN DO;
                 PUT "The calculated power is" power;
                 IF  (assignment="between_clusters") THEN DO;
                    ncells = 2**nfactors;
                    IF (nclusters<ncells) THEN DO;
                        PUT "However, a complete factorial requires at least " ncells " clusters.";
                    END;
                 END;
             END;
             RETURN(power);
        FINISH CalculatePowerTwoLevel;
        START CalculateNTwoLevel(alpha,
                            assignment,
                            cluster_size,
                            cluster_size_sd,
                            icc,
                            nfactors,
                            nregcoefs,
                            power,
                            sigma_effective,
                            the_coef);
            IF assignment="within_clusters" THEN DO;
                bottom = CEIL((nregcoefs+1)/cluster_size);
            END; ELSE DO;
                bottom = nregcoefs+1;
            END;
            top = 10000000;
            BinarySearchDone = 0;
            DO WHILE(BinarySearchDone = 0);
                middle = CEIL(bottom+.5*(top-bottom));
                fmiddle = CalculatePowerTwoLevel(alpha,
                                    assignment,
                                    cluster_size,
                                    cluster_size_sd,
                                    icc,
                                    middle, /* for nclusters */
                                    nfactors,
                                    nregcoefs,
                                    sigma_effective,
                                    the_coef,
                                    1);
           /*     PUT bottom middle top fmiddle power; */
                IF middle=top THEN DO; 
                     BinarySearchDone = 1;
                END; ELSE DO; 
                    IF (fmiddle<power) THEN DO; bottom = middle; END;
                    IF fmiddle = power THEN DO; bottom = middle; top=middle; END;
                    IF (fmiddle>power) THEN DO; top = middle; END;
                END; 
            END;
            nclusters = middle; 
            ncells = 2**nfactors;
            IF (cluster_size^=1) THEN DO;
                PUT "The calculated sample size is" nclusters " clusters.";
            END; ELSE DO;
                PUT "The calculated sample size is" nclusters " subjects.";
            END;   
            ncells = 2**nfactors;
            IF  (assignment="between_clusters") THEN DO;
                IF (nclusters<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least  " ncells " clusters.";
                END;
            END;
            IF  (assignment="within_clusters") THEN DO;
                ntotal = nclusters * cluster_size;
                IF (ntotal<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least " ncells " subjects.";
                END;
            END;
            IF  (assignment="unclustered") THEN DO;
                IF (nclusters<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least  " ncells " subjects.";
                END;
            END;
            RETURN(nclusters);
        FINISH CalculateNTwoLevel;
        START CalculateDetectableTwoLevel(alpha,
                                        assignment,
                                        cluster_size,
                                        cluster_size_sd,
                                        icc,
                                        nclusters,
                                        nfactors,
                                        nregcoefs,
                                        power,
                                        sigma_effective,
                                        sigma_y);
            bottom = 0;
            top = 5*sigma_y;
            BinarySearchDone = 0;
            DO WHILE(BinarySearchDone = 0);
                middle = bottom+.5*(top-bottom);
                fmiddle = CalculatePowerTwoLevel(alpha,
                                                assignment,
                                                cluster_size,
                                                cluster_size_sd,
                                                icc,
                                                nclusters,
                                                nfactors,
                                                nregcoefs,
                                                sigma_effective,
                                                middle, /* for the_coef */
                                                1);
                IF (ABS(middle-top)<1e-5) THEN DO;
                     BinarySearchDone = 1;
                END; ELSE DO;
                    IF (fmiddle<power) THEN DO; bottom = middle; END;
                    IF fmiddle = power THEN DO; bottom = middle; top=middle; END;
                    IF (fmiddle>power) THEN DO; top = middle; END;
                END;
            END;
            the_coef = middle;
            IF fmiddle<(power-.01) THEN DO;
                PUT "Macro error: Could not find a detectable effect size";
                ABORT;
            END;
            PUT "The detectable effect size is estimated as follows:";
            IF ((sigma_y < 0.999999)|(sigma_y > 1.000001)) THEN DO;
                /*******/
                calculated_raw_coef = the_coef;
                PUT "  As an unstandardized regression coefficient for either";
                PUT "  a main effect or an interaction:" @60 calculated_raw_coef 8.4;
                /*******/
                calculated_raw_main = 2*the_coef;
                PUT "  As an unstandardized mean difference for a main effect:"
                   @60 calculated_raw_main 8.4;
                /*******/
                calculated_raw_ixn = 4*the_coef;
                PUT "  As an unstandardized difference in differences for ";
                PUT "  a 2-way interaction:" @60 calculated_raw_ixn  8.4;
                /*******/
            END;
            calculated_std_coef = the_coef/sigma_y;
            PUT "  As a standardized regression coefficient for either";
            PUT "  a main effect or an interaction:"  @60 calculated_std_coef 8.4;
            /*******/
            calculated_d_main = 2*the_coef/sigma_y;
            PUT "  As a standardized mean difference (Cohen d) for a ";
            PUT "  main effect:" @60 calculated_d_main 8.4;
            /*******/
            calculated_d_ixn = 4*the_coef/sigma_y;
            PUT "  As a standardized difference in differences for ";
            PUT "  a 2-way interaction:" @60 calculated_d_ixn 8.4;
            /*******/
            calculated_effect_size_ratio = (the_coef**2)/(sigma_y**2);
            PUT "  As a standardized effect size ratio (Cohen f squared) ";
            PUT "  for a main effect or interaction:"
               @60 calculated_effect_size_ratio 8.4;
            /*******/
            ncells = 2**nfactors;
            IF  (assignment="between_clusters") THEN DO;
                IF (nclusters<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least " ncells " clusters.";
                END;
            END;
            RETURN(the_coef);
        FINISH CalculateDetectableTwoLevel;
        START CalculatePowerThreeLevel( alpha,
                                      assignment,
                                      change_score_icc,
                                      cluster_size,
                                      cluster_size_sd,
                                      icc,
                                      nclusters,
                                      nfactors,
                                      nregcoefs,
                                      pre_post_corr,
                                      sigma_y,
                                      the_coef,
                                      silent);
             cluster_size_cv = cluster_size_sd/cluster_size;
             nadj = cluster_size*(1+cluster_size_cv**2);
             deff = (1+(nadj-1)*change_score_icc);
             IF ((assignment="between_clusters")|assignment="unclustered") THEN DO;
                 df2 = nclusters - nregcoefs;
             END;
             IF (assignment="within_clusters") THEN DO;
                 df2 = FLOOR(nclusters*cluster_size) - nregcoefs;
             END;
             IF (df2 < 1) THEN DO;
                 PUT "Macro error: The sample size would be too small to fit the model.";
                 ABORT;
             END;
             num = cluster_size*nclusters*the_coef*the_coef*(1-change_score_icc);
             den = 2*(sigma_y**2)*(1-pre_post_corr)*(1-icc)*deff;
             lambda = num/den;
             IF (lambda = .) THEN DO;
                 PUT "Macro error: Could not calculate noncentrality parameter.";
                 ABORT;
             END;
             df1 = 1;
             crit = FINV(1-alpha,df1,df2,0);
             power = ROUND(1-PROBF(crit,df1,df2,MIN(100,lambda)),.0001);
             IF (silent=0) THEN DO;
                 PUT "The calculated power is" power;
                 ncells = 2**nfactors;
                 ntotal = nclusters*cluster_size;
                 IF  (assignment="between_clusters") THEN DO;
                     IF (nclusters<ncells) THEN DO;
                         PUT "However, a complete factorial requires at least " ncells " clusters.";
                     END; 
                 END; ELSE DO;
                     IF (ntotal<ncells) THEN DO;
                         PUT "However, a complete factorial requires at least " ncells " subjects.";
                     END; 
                 END; 
             END;
             RETURN(power);
        FINISH CalculatePowerThreeLevel;
        START CalculateNThreeLevel(alpha,
                            assignment,
                            change_score_icc,
                            cluster_size,
                            cluster_size_sd,
                            icc,
                            nfactors,
                            nregcoefs,
                            power,
                            pre_post_corr,
                            sigma_y,
                            the_coef);
            IF (assignment="between_clusters") THEN DO;
                bottom = nregcoefs+1;
            END; ELSE DO;
                bottom = CEIL((nregcoefs+1)/cluster_size);
            END;
            top = 10000000;
            BinarySearchDone = 0;
            DO WHILE(BinarySearchDone = 0);
                middle = CEIL(bottom+.5*(top-bottom));
                fmiddle = CalculatePowerThreeLevel(alpha,
                                        assignment,
                                        change_score_icc,
                                        cluster_size,
                                        cluster_size_sd,
                                        icc,
                                        middle, /* for nclusters */
                                        nfactors,     
                                        nregcoefs,
                                        pre_post_corr,
                                        sigma_y,
                                        the_coef,
                                        1);
                IF middle=top THEN DO;
                     BinarySearchDone = 1;
                END; ELSE DO;
                    IF (fmiddle<power) THEN DO; bottom = middle; END;
                    IF fmiddle = power THEN DO; bottom = middle; top=middle; END;
                    IF (fmiddle>power) THEN DO; top = middle; END;
                END;
            END;
            nclusters = middle;
            ncells = 2**nfactors;
            IF (cluster_size^=1) THEN DO;
                PUT "The calculated sample size is" nclusters " clusters.";
            END; ELSE DO;
                PUT "The calculated sample size is" nclusters " subjects.";
            END;
            IF (assignment="unclustered") THEN DO;
                IF (nclusters<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least " ncells " subjects.";
                END;
            END;
            IF (assignment="between_clusters") THEN DO;
                IF (nclusters<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least " ncells " clusters.";
                END;
            END;
            IF (assignment="within_clusters") THEN DO;
                IF (nclusters*cluster_size<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least " ncells " total subjects.";
                END;
            END;
            RETURN(nclusters);
        FINISH CalculateNThreeLevel;
        START CalculateDetectableThreeLevel(alpha,
                                            assignment,
                                            change_score_icc,
                                            cluster_size,
                                            cluster_size_sd,
                                            icc,
                                            nclusters,
                                            nfactors, 
                                            nregcoefs,
                                            power,
                                            pre_post_corr,
                                            sigma_y);
            bottom = 0;
            top = 5*sigma_y;
            BinarySearchDone = 0;
            DO WHILE(BinarySearchDone = 0);
                middle = bottom+.5*(top-bottom);
                fmiddle = CalculatePowerThreeLevel( alpha,
                                      assignment,
                                      change_score_icc,
                                      cluster_size,
                                      cluster_size_sd,
                                      icc,
                                      nclusters,
                                      nfactors,
                                      nregcoefs,
                                      pre_post_corr,
                                      sigma_y,
                                      middle, /* for the_coef */
                                      1);
                IF (ABS(middle-top)<1e-5) THEN DO;
                     BinarySearchDone = 1;
                END; ELSE DO;
                    IF (fmiddle<power) THEN DO; bottom = middle; END;
                    IF fmiddle = power THEN DO; bottom = middle; top=middle; END;
                    IF (fmiddle>power) THEN DO; top = middle; END;
                END;
            END;
            the_coef = middle;
            IF fmiddle<(power-.01) THEN DO;
                PUT "Macro error: Could not find a detectable effect size";
                ABORT;
            END;
            PUT "The detectable effect size is estimated as follows:";
            IF ((sigma_y < 0.999999)|(sigma_y > 1.000001)) THEN DO;
                /*******/
                calculated_raw_coef = the_coef;
                PUT "  As an unstandardized regression coefficient for either";
                PUT "  a main effect or an interaction:" @60 calculated_raw_coef 8.4;
                /*******/
                calculated_raw_main = 2*the_coef;
                PUT "  As an unstandardized mean difference for a main effect:"
                   @60 calculated_raw_main 8.4;
                /*******/
                calculated_raw_ixn = 4*the_coef;
                PUT "  As an unstandardized difference in differences for ";
                PUT "  a 2-way interaction:" @60 calculated_raw_ixn  8.4;
                /*******/
            END;
            calculated_std_coef = the_coef/sigma_y;
            PUT "  As a standardized regression coefficient for either";
            PUT "  a main effect or an interaction:"  @60 calculated_std_coef 8.4;
            /*******/
            calculated_d_main = 2*the_coef/sigma_y;
            PUT "  As a standardized mean difference (Cohen d) for a ";
            PUT "  main effect:" @60 calculated_d_main 8.4;
            /*******/
            calculated_d_ixn = 4*the_coef/sigma_y;
            PUT "  As a standardized difference in differences for ";
            PUT "  a 2-way interaction:" @60 calculated_d_ixn 8.4;
            /*******/
            calculated_effect_size_ratio = (the_coef**2)/(sigma_y**2);
            PUT "  As a standardized effect size ratio (Cohen f squared) ";
            PUT "  for a main effect or interaction:"
               @60 calculated_effect_size_ratio 8.4;
            /*******/
            RETURN(the_coef);
            ncells = 2**nfactors;
            ntotal = nclusters*cluster_size;
            IF  (assignment="between_clusters") THEN DO;
                IF (nclusters<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least " ncells " clusters.";
                END; 
            END; ELSE DO;
                IF (ntotal<ncells) THEN DO;
                    PUT "However, a complete factorial requires at least " ncells " subjects.";
                END; 
            END; 
        FINISH CalculateDetectableThreeLevel;
        /**************************************/
        /*** Main IML code ***/
        success = 0;
        power = .;
        nclusters = .;
        ntotal = .;
        raw_regression_coefficient = .;
        USE fpp_inputs;
           READ VAR { DataErrorCode
                      has_specified_assignment assignment
                      has_specified_alpha alpha 
                      has_specified_change_score_icc change_score_icc 
                      has_specified_cluster_size cluster_size 
                      has_specified_cluster_size_sd cluster_size_sd 
                      has_specified_d_main d_main 
                      has_specified_effect_size_ratio effect_size_ratio 
                      has_specified_icc icc 
                      has_specified_model_order model_order 
                      has_specified_nclusters nclusters 
                      has_specified_nfactors nfactors 
                      has_specified_ntotal ntotal 
                      has_specified_pretest pretest 
                      has_specified_pre_post_corr pre_post_corr 
                      has_specified_raw_coef raw_coef 
                      has_specified_raw_main raw_main 
                      has_specified_sigma_y sigma_y 
                      has_specified_std_coef std_coef 
                      has_specified_power power 
                      num_effect_sizes_specified has_specified_some_effect_size 
                      num_sample_sizes_specified has_specified_some_sample_size 
                      MacroError
                      to_find 
                      assignment 
                      pretest 
                      nregcoefs 
                      the_coef
                      sigma_effective
            };
        CLOSE fpp_inputs; 
        PUT "------------------------------------------------------------"; 
        PUT "FactorialPowerPlan Macro";
        PUT "The Methodology Center";
        PUT "(c) 2012 Pennsylvania State University";
        PUT "------------------------------------------------------------"; 
        IF MacroError=1 THEN DO;
            CALL HandleDataErrors(DataErrorCode);
        END;
		/*** Display input information for the user's records ***/
		PUT "Assumptions:";
        PUT "There are " nfactors 3. " dichotomous factors.";
        IF assignment="unclustered" THEN PUT "There is independent random assignment.";
        IF assignment="within_clusters" THEN PUT "There is random assignment of individuals for each cluster (within-clusters effects).";
        IF assignment="between_clusters" THEN PUT "There is random assignment of clusters (between-clusters effects).";
		IF pretest="covariate" THEN PUT "There is a pretest modeled as a covariate.";
		IF pretest="repeated_measure " THEN PUT "There is a pretest modeled as a repeated measure.";
        IF model_order=1 THEN PUT "Analysis will be based on main effects only.";
		IF model_order=2 THEN PUT "Analysis will be based on main effects and 2-way interactions.";
		IF model_order=3 THEN PUT "Analysis will be based on main effects, 2-way, and 3-way interactions.";
		IF has_specified_power THEN DO;
			PUT "Desired power:" power 9.3;
		END;
		PUT "Two-sided alpha:" alpha 9.2;
		IF has_specified_cluster_size THEN DO;
			PUT "Cluster size:" cluster_size 9.2;
		 END; 
		IF has_specified_cluster_size_sd THEN DO;
			PUT "Cluster size standard deviation:" cluster_size_sd 9.2;
		END;
		IF has_specified_nclusters THEN DO;
			PUT "Number of clusters:" nclusters 7.;
		END;
		IF has_specified_ntotal THEN DO;
			PUT "Total number of participants:" ntotal 9.;
		END;
		IF has_specified_d_main THEN DO;
			PUT "Effect size as standardized mean difference:" d_main 9.2;
		END;
		IF has_specified_effect_size_ratio THEN DO;
			PUT "Effect size as variance ratio:" effect_size_ratio 6.2;
		END;
		IF has_specified_raw_coef THEN DO;
			PUT "Effect size as unstandardized regression coefficient:" raw_coef 9.2;
		END;
		IF has_specified_raw_main THEN DO;
			PUT "Effect size as unstandardized difference in means:" raw_main 9.2;
		END;
		IF has_specified_std_coef THEN DO; 
			PUT "Effect size as standardized regression coefficient:" std_coef 9.2;
		END;
		IF has_specified_icc THEN DO;
			PUT "Intraclass correlation of response variable:" icc 9.3;
		END; 
		IF has_specified_change_score_icc THEN DO;
			PUT "Intraclass correlation of change scores:" change_score_icc 9.3;
		END;
		IF has_specified_pre_post_corr THEN DO;
			PUT "Pretest-posttest correlation:" pre_post_corr 9.3;
		END;
		IF has_specified_sigma_y THEN DO;
			Put "Assumed standard deviation for the response variable is " sigma_y 9.2;
		END;
		IF (to_find = "effect_size") THEN DO;
            PUT "Attempting to calculate the estimated detectable effect size.";
        END;
        IF (to_find = "power") THEN DO;
            PUT "Attempting to calculate the estimated power.";
        END;
        IF (to_find = "sample_size") THEN DO;
            PUT "Attempting to calculate the estimated required sample size.";
        END;
        PUT "------------------------------------------------------------"; 
        PUT "Results:";
        /*** One-level (subject) model ***/
        IF (assignment="unclustered"&
            ((pretest="none")|(pretest="covariate"))) THEN DO;
            IF (to_find="power") THEN DO; 
                 power = CalculatePowerUnclustered( alpha,
                                                 nfactors,
                                                 nregcoefs,
                                                 ntotal,
                                                 sigma_effective,
                                                 the_coef,
                                                 0); 
                 success = 1; 
            END;
            IF (to_find="sample_size") THEN DO;
                ntotal = CalculateNUnclustered( alpha,
                                             nfactors,
                                             nregcoefs,
                                             power,
                                             sigma_effective,
                                             the_coef ); 
                 success = 1; 
            END;
            IF (to_find="effect_size") THEN DO;
                the_coef = CalculateDetectableUnclustered( alpha,
                                                           nfactors,
                                                           nregcoefs,
                                                           ntotal,
                                                           power,
                                                           sigma_effective,
                                                           sigma_y);  
                 success = 1; 
            END;
        END;
        /*** Two-level (cluster, subject) models ***/
        IF (((assignment="between_clusters")&(pretest="none"))
            |((assignment="within_clusters")&((pretest="none")|(pretest="covariate")))) THEN DO;
             IF (has_specified_cluster_size = 0) THEN DO;
                 PUT "Macro error: The cluster size must be specified."; 
                 ABORT; 
             END;
             IF (has_specified_cluster_size_sd = 0) THEN DO;
                 cluster_size_sd = 0;
             END; 
             cluster_size_cv = cluster_size_sd/cluster_size;
             IF (has_specified_icc = 0) THEN DO;
                 PUT "Macro error: icc must be specified.";
                 ABORT;
             END;
             IF (pretest="none") THEN DO;
                 sigma_effective = sigma_y;
             END; ELSE DO;
                sigma_effective = sigma_y*SQRT(1-pre_post_corr**2);
             END;  
             IF (to_find="power") THEN DO; 
                  power = CalculatePowerTwoLevel(alpha,
                            assignment,
                            cluster_size,
                            cluster_size_sd,
                            icc,
                            nclusters,
                            nfactors,
                            nregcoefs,
                            sigma_effective,
                            the_coef,
                            0);
                 success = 1; 
            END;
            IF (to_find="sample_size") THEN DO;
                 nclusters = CalculateNTwoLevel(alpha,
                            assignment,
                            cluster_size,
                            cluster_size_sd,
                            icc,
                            nfactors,
                            nregcoefs,
                            power,
                            sigma_effective,
                            the_coef);
                 ntotal = nclusters*cluster_size;
                 success = 1; 
            END;
            IF (to_find="effect_size") THEN DO;
                 the_coef = CalculateDetectableTwoLevel(alpha,
                                                        assignment,
                                                        cluster_size,
                                                        cluster_size_sd,
                                                        icc,
                                                        nclusters,
                                                        nfactors,
                                                        nregcoefs,
                                                        power,
                                                        sigma_effective,
                                                        sigma_y);
                 success = 1; 
            END;
        END;
        /*** Repeated measures models: either two-level (subject, time) models or three-level (cluster, subject, time) models ***/
        IF (((assignment="between_clusters")|(assignment="within_clusters")|(assignment="unclustered"))&
             (pretest="repeated_measure")) THEN DO;
            IF (assignment="unclustered") THEN DO;
                cluster_size = 1;
                cluster_size_sd = 0;
                cluster_size_cv = 0;
                icc = 0;
                change_score_icc = 0;
                nclusters = ntotal;
            END;
            IF (assignment="within_clusters") THEN DO;
                IF (has_specified_cluster_size = 0) THEN DO;
                    PUT "Macro error: The cluster size must be specified.";
                    ABORT;
                END;
                IF (has_specified_cluster_size_sd = 0) THEN DO;
                    cluster_size_sd = 0;
                END;
                cluster_size_cv = cluster_size_sd/cluster_size;
                IF (has_specified_icc = 0) THEN DO;
                    PUT "Macro error: icc must be specified.";
                    ABORT;
                END;
                IF (has_specified_change_score_icc = 1) THEN DO;
                    PUT "Macro error: change_score_icc should not be specified for this model.";
                    ABORT;
                END;
                change_score_icc = 0;
                IF (has_specified_pre_post_corr = 0) THEN DO;
                    PUT "Macro error: pre_post_corr must be specified.";
                    ABORT;
                END;
            END;
            IF (assignment="between_clusters") THEN DO;
                IF (has_specified_cluster_size = 0) THEN DO;
                    PUT "Macro error: The cluster size must be specified.";
                    ABORT;
                END;
                IF (has_specified_cluster_size_sd = 0) THEN DO;
                    cluster_size_sd = 0;
                END;
                cluster_size_cv = cluster_size_sd/cluster_size;
                IF (has_specified_icc = 0) THEN DO;
                    PUT "Macro error: icc must be specified.";
                    ABORT;
                END;
                IF (has_specified_change_score_icc = 0) THEN DO;
                    PUT "Macro error: change_score_icc must be specified.";
                    ABORT;
                END;
                IF (has_specified_pre_post_corr = 0) THEN DO;
                    PUT "Macro error: pre_post_corr must be specified.";
                    ABORT;
                END;
             END;
             IF (to_find="power") THEN DO;
                 power = CalculatePowerThreeLevel(alpha,
                            assignment,
                            change_score_icc,
                            cluster_size,
                            cluster_size_sd,
                            icc,
                            nclusters,
                            nfactors,
                            nregcoefs,
                            pre_post_corr,
                            sigma_y,
                            the_coef,
                            0);
                 success = 1;
            END;
            IF (to_find="sample_size") THEN DO;
                 nclusters = CalculateNThreeLevel(alpha,
                            assignment,
                            change_score_icc,
                            cluster_size,
                            cluster_size_sd,
                            icc,
                            nfactors,
                            nregcoefs,
                            power,
                            pre_post_corr,
                            sigma_y,
                            the_coef);
                 success = 1;
                 IF (assignment="unclustered") THEN DO;
                     ntotal = nclusters;
                 END; ELSE DO;
                     ntotal = nclusters*cluster_size;
                 END;
            END;
            IF (to_find="effect_size") THEN DO;
                 the_coef = CalculateDetectableThreeLevel(alpha,
                                                        assignment,
                                                        change_score_icc,
                                                        cluster_size,
                                                        cluster_size_sd,
                                                        icc,
                                                        nclusters,
                                                        nfactors,
                                                        nregcoefs,
                                                        power,
                                                        pre_post_corr,
                                                        sigma_y);
                 success = 1;
            END;
        END;
        /*** Finish up ***/
        IF (to_find="power") THEN DO;
             CREATE ffp_output VAR {power};
                 APPEND;
             CLOSE ffp_output;
        END;
        IF (to_find="sample_size") THEN DO;
             IF assignment="unclustered" THEN DO;
                 CREATE ffp_output VAR {ntotal};
                     APPEND;
                 CLOSE ffp_output;
             END; ELSE DO;
                 CREATE ffp_output VAR {nclusters ntotal};
                     APPEND;
                 CLOSE ffp_output;
             END;
        END;
        IF (to_find="effect_size") THEN DO;
             raw_regression_coefficient=the_coef;
             std_regression_coefficient=the_coef/sigma_y;
             IF (has_specified_sigma_y=0) THEN DO; 
                            /* Don't output the raw coefficient if you only assumed that the variance was 1.*/ 
                       CREATE ffp_output VAR {std_regression_coefficient};
                         APPEND;
                     CLOSE ffp_output; 
             END; ELSE DO;
                     CREATE ffp_output VAR {raw_regression_coefficient std_regression_coefficient};
                         APPEND;
                     CLOSE ffp_output;
             END;
        END;
        IF (success = 0) THEN DO;
            PUT "Macro error: I'm sorry, I can't do that kind of calculation or";
            PUT "that kind of model yet.";
            ABORT;
        END;
        PUT "------------------------------------------------------------"; 
    QUIT;
%MEND;

