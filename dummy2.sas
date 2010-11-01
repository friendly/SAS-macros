/*
More substantial changes:
- I changed the handling of missing values:  Now when the variable is
  missing, all the dummies will be missing
- I added the format option which allows, among other things, to create a
  dummy variable for a range of values

Essentially "aesthetic" changes:
- I have changed the default for the prefix to be the name of the 
original
variable, followed by an underscore
- I added special keywords, BLANK and VARNAME, as possible prefixes
- instead of NAME=VAL, I made a parameter numval=T/F (default=F)
- I have changed the special keywords for base to "low" and "high" 
(instead
of _first_ and _last_), and made the default low
- I added more documentation

Anyway, since 90% of the work on this macro is yours, I thought it was only
fair to send you my version, in case you wish to incorporate any of my code.
I am also including the documentation I posted on our local Intranet. Note
that we are working in SAS Version 8, and so I made no attempt to keep
variable names down to 8 characters.

Kathy Sykora, Biostatistician
Institute for Clinical Evaluative Sciences
G106-2075 Bayview Ave, Toronto, ON, M4N 3M5
tel: 416-480-6100 ext 3881 (fax: 416-480-6048)
kathy.sykora@ices.on.ca

*/
/******************************************************************
*
* MACRO:        DUMMY
*
* PURPOSE:      Given a character or discrete numeric variable, 
*		the DUMMY macro creates dummy (0/1) variables to 
*      		represent the levels of the original variable.
*
*		If the original variable has c levels, (c-1)
* 		new variables are produced (or c variables, 
*		if FULLRANK=0)  When the original variable
*		is missing, all dummy variables will be
*		missing.
*
*		Several variables can be "dummied" during one
*		invocation of the macro.
*
* CREATED:      February 2001
*
* NOTE:		This macro is based very very closely on 
*		dummy.sas written by Michael Friendly of
*		York University.  (friendly@yorku.ca).
*		For the original code and documenation, see
*		http://hotspur.psych.yorku.ca/SCS/vcd/dummy.html.
*
* REQUIRED PARAMETER:	var
*
* OPTIONAL PARAMETERS:	data, out, prefix, numval, 
*			base, format,fullrank
* 
****************   INSTRUCTIONS   **********************************
*
*--- You MUST specify ---
*
* var           The input variable(s) may be numeric or character.  
*
*--- You also may specify ---
*
* DATA=    The name of the input dataset.  If not specified, the most
*          recently created dataset is used. Data set options may
*	   be used.
* 
* OUT=     The name of the output dataset.  If not specified, the new
*          variables are appended to the input dataset.
*
* PREFIX=  Prefix(s) used to create the names of dummy variables.  The
*          default is the variable name followed by an underscore.  
*          Specifying prefix=blank means that there will be no prefix.
*	   Specifying prefix=varname will go back to the default - e.g.
*          when several variables with different prefixes are being 
*          specified.
*
* NUMVAL=  By default, numval=F, and the suffix for the dummy names 
*	   are the variable values.  If you specify numval=T, the dummy
*          variables will be named instead by appending consecutive 
*          numbers.  (Useful when variable contains special characters.)  
*
* BASE=    Indicates the level of the baseline category, which is given
*          values of 0 on all the dummy variables.  BASE=LOW 
*          specifies that the lowest value of the VAR= variable is the
*          baseline group, BASE=HIGH specifies the highest value of
*          the variable.  Otherwise, you can specify BASE=value to
*          make a different value the baseline group.  If the value is 
*          non-numeric, it must be in quotation marks.  The default is
*	   LOW.
*
* FORMAT   A predefined format may be used for two purposes:  One, to 
*          name the dummy variables, and two, to create dummy variables
*          which are indicators for ranges of the input variable.  
* 	   Variables using the format option must be listed first.
*
*          Examples: 
*
*          * This code will create variable Male;
*              proc format;
*                 value $sex M=Male F=Female;
*              %dummy (var=sex, 
*		       data=mydata, 
*                      format=$sex, 
*                      prefix=blank)
*
*   	   * This code will create variables age_under65, 
*	     age_bet65_85, and age_over85, as well as 
*	     regular dummy variables for region;
*	       proc format;
*	          value agefmt  
*                    0-64=under65 65-85=bet65_85 86-max=over85;
*	       %dummy (var=age region,
*                      data=somedata,
*                      format=agefmt,
*                      fullrank=F)
*
* FULLRANK  T/F, where T indicates that the indicator for the BASE 
*	    category is eliminated.  The fullrank parameter applies
*	    to all the input variables.  The default is T.  
*
* CONTACT:      Kathy Sykora
*
*************   EXAMPLES   ****************************************
*
* With the input data set,
* 
*  data test;
*    input y group $ @@;
*   cards;
*   10  A  12  A   13  A   18  B  19  B  16  C  21  C  19  C  
*   ;
*
* The macro statement:
*
*         %dummy ( data = test, var = group, base=high) ;
*
* produces two new variables, GROUP_A and GROUP_B.  Group C is the baseline
* category (corresponding to BASE=HIGH)
*
*                       OBS     Y    GROUP    GROUP_A    GROUP_B
*                        1     10      A         1         0
*                        2     12      A         1         0
*                        3     13      A         1         0
*                        4     18      B         0         1
*                        5     19      B         0         1
*                        6     16      C         0         0
*                        7     21      C         0         0
*                        8     19      C         0         0
*
* Other examples:
*
* %dummy (data=proj.data, var=sex, prefix=BLANK)
*
* %dummy (data=patients,
*         var=physnum,
*         numval=T,
*         fullrank=F)
*
* %dummy (data=library.cohort,
*         var=sex region,
*         prefix=blank varname,
*         base="M" 7)
*
* IN THIS EXAMPLE, NOTE THAT IF LAST VARIABLE TAKES DEFAULT OPTIONS,
* ONLY THE NON-DEFAULT ONES NEED TO BE SPECIFIED.
* %dummy (data=somedata,
*         var=age sex region,
*	  format=agefmt $sex,
*	  prefix=varname blank,
*	  base=low low high)
*******************************************************************/;

%macro dummy( 
        var= ,            /* variable(s) to be dummied */
        data=&syslast,    /* name of input dataset */
        out=&data,        /* name of output dataset */
        base=low,         /* base category/ies */
        prefix = varname, /* prefix(es) for dummy variable names */
        format =,	        /* format used to categorize variable */
        numval=F,         /* Should values be numbered? */
        fullrank=T        /* Should base value get a dummy? */
        );

******************************************************************
* Check for missing var parameter;
******************************************************************;
   %if (%length(&var) = 0) %then %do;
       %put ERROR: DUMMY: VAR= must be specified;
       %goto done;
   %end;

******************************************************************
* Clean up parameter values - change them to upper case;
******************************************************************;
%let abort = 0;
%let base = %upcase(&base);
%let data = %upcase(&data);

******************************************************************
* Initialize output data set;
******************************************************************;
data &out;
   set &data;
run;

******************************************************************
* j indexs variables, vari is the current variable name;
******************************************************************;
%local j vari;
%let j=1;

******************************************************************
* Find the current variable name;
******************************************************************;
%let vari= %scan(&var,    &j, %str( ));

******************************************************************;
* Loop over variables; 
******************************************************************;
%do %while(&vari ^= );

******************************************************************
* Find the current prefix for dummies;
******************************************************************;
%let pre = %scan(&prefix, &j, %str( ));
%if %upcase(&pre) = VARNAME | &pre = %then %let pre=&vari._;
* Keyword BLANK for prefix indicates no prefix;
%if %upcase(&pre)=BLANK %then %let pre=;

******************************************************************
* Find the current base for dummies;
******************************************************************;
%let baseval = %scan(&base, &j, %str( ));
%if &baseval = %then %let baseval=HIGH;

******************************************************************
* Find the format for dummies;
******************************************************************;
%let fmt = %scan(&format, &j, %str( ));

******************************************************************;
* Determine values of variable to be dummied;
******************************************************************;
proc summary data = &data nway ;
     class &vari ;
     %if %length(&fmt) gt 0 %then %do;
        format &vari &fmt..;
     %end;

     output out = _cats_ ( keep = &vari ) ;

%if &syserr > 4 %then %let abort=1; 
%if &abort %then %goto DONE;

   ************************************************************;
   * Eliminate base category, unless otherwise specified; 
   ************************************************************;
   %if &fullrank=T %then %do;
      data _cats_;
         set _cats_ end=_eof_;
         %if &baseval=LOW %then %str( if _n_ = 1 then delete;);
         %else 
             %if &baseval=HIGH %then %str( if _eof_ then delete;);
                %else        %str(if &vari = &baseval then delete;);
        
        run;
   %end;

******************************************************************;
* How many values were there?;
******************************************************************;
data _null_ ;
  set _cats_ nobs = numvals ;
  if _n_ = 1 then do;
  
     ************************************************************;
     * If there are no non-baseline values - abort macro; 
     ************************************************************;
     call symput('abort',trim( left( put( (numvals=0), best.)))) ;

     ************************************************************;
     * Place number of dummies into macro variable num;
     ************************************************************;
     call symput( 'num', trim( left( put( numvals, best. ) ) ) ) ;
  end;

   ************************************************************;
   * Number the values, place in macro variables c##; 
   ************************************************************;
  call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
                       trim(left(&vari)) ) ;

  %if %length(&fmt) gt 0 %then %do;
     call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
                       trim(left(put(&vari,&fmt..)) ) );
  %end;
    
run ;

%if &syserr > 4 %then %let abort=1; 
%if &abort %then %goto DONE;

******************************************************************
* Name the dummy variables for the j-th input variable;
* vl"j" will contain a string with all the values OF VARI
*    except the base one
******************************************************************;

******************************************************************;
* Usual way: Dummy variables names end with value being indicated;
*****************************************************************;
%if &numval = F %then %do ;
   %let vl&j =; 
   %do k=1 %to &num; 
      %let vl&j = &&vl&j  &pre&&c&k; 
   %end; ;

   data &out;
      set &out ;	
      array __d ( &num ) %do k=1 %to &num ;   
                             &pre&&c&k
                         %end ; ;

%end ; 

%else %do ;
   ************************************************************;
   * Another way: Dummy variables end with consecutive numbers;
   ************************************************************;
   %let vl&j =; 
   %do k=1 %to &num; 
      %let vl&j = &&vl&j  &pre.&k; 
   %end; ;

   data &out
      ( rename = ( %do k=1 %to &num ;
                       d&k = &pre.&k
                   %end ; ) ) ;
     set &out ;
     
     array __d ( &num ) d1-d&num ; 
%end ;


******************************************************************
* Assign values to the dummy variables for the j-th input variable;
******************************************************************;
     do j = 1 to &num ; /* initialize to 0 */      
        __d(j) = 0 ;
     end ;


     %if %length(&fmt) eq 0 %then %do;
     ***********************************************************;
     * Case 1:  No format;
     ***********************************************************;
        if &vari = "&c1" then __d ( 1 ) = 1 ;  /* create dummies */
        %do i = 2 %to &num ;       
           else if &vari="&&c&i" then __d ( &i ) = 1 ;
        %end;
     %end;

     %else %do;
     ***********************************************************;
     * Case 2:  with format;
     ***********************************************************;
        if put(&vari,&fmt..) = "&c1" then __d ( 1 ) = 1 ;
        %do i = 2 %to &num ;       
           else if put(&vari,&fmt..)="&&c&i" then __d ( &i ) = 1;
        %end;
     %end;

     ***********************************************************;
     * Handle missing values;
     ***********************************************************;
     if missing(&vari) then do j=1 to &num;
        __d(j)=.;
     end;

  drop j;
run ;

******************************************************************
* Find the next variable;
******************************************************************;
%let j=%eval(&j+1);
%let vari= %scan(&var, &j, %str( ));

******************************************************************
* End of loop;
******************************************************************;
%end;  

%done:
   %if &abort %then %put ERROR: The DUMMY macro ended abnormally.;
%mend dummy;

