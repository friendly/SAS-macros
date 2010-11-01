/**************************************************************************/
/* %mixcas written by Ian Whitlock. */

%macro mixcase ( var , dlim=' &-*+,/;' , target=, upper=II III ) ;
         drop __i ;
      %if %quote(&target) ^= %then
      %do ;
          &target = &var ;
          %let var = &target ;
      %end ;
      &var = lowcase ( &var ) ;
      substr (&var,1,1) = upcase(substr(&var,1,1)) ;
      do __i = 1 to length ( &var ) - 1 ;
        if index ( &dlim , substr(&var,__i,1) ) then
           substr(&var,__i+1,1) = upcase(substr(&var,__i+1,1)) ;
      end ;
	*-- SPECIAL CASES: ALL UPPER CASE ;
	IF INDEX(&var,' Iii')>0 THEN
		SUBSTR(&var,INDEX(&var,' Iii'),4)=' III';
	IF INDEX(&var,' Ii')>0 THEN
		SUBSTR(&var,INDEX(&var,' Ii'),3)=' II';

	*-- ALL LOWER CASE;
	IF INDEX(&var,' Or ')>0 THEN
		SUBSTR(&var,INDEX(&var,' Or '),4)=' or ';
	IF INDEX(&var,' For ')>0 THEN
		SUBSTR(&var,INDEX(&var,' For '),5)=' for ';
	IF INDEX(&var,'-Et-')>0 THEN
		SUBSTR(&var,INDEX(&var,'-Et-'),4)='-et-';
	IF INDEX(&var,'-Du-')>0 THEN
		SUBSTR(&var,INDEX(&var,'-Du-'),4)='-du-';
	IF INDEX(&var,'-De-')>0 THEN
		SUBSTR(&var,INDEX(&var,'-De-'),4)='-de-';
%mend mixcase ;
/*
Example:
* input the CPT Anesthesia and Surgical procedure coding list
data cpt;
    codes='cpt';
    infile temp pad;
    input @1 code $7. @10 proced $40.;
    run;

data cpt;
   set cpt;

   %mixcase(proced,target = mixcase);
   run;

Output:
CODE      PROCED                          MIXCASE

"00104"   Anesth for Electroshock         ANESTH FOR ELECTROSHOCK
"00844"   Anesth, Pelvis Surgery          ANESTH, PELVIS SURGERY
"82784"   Assay Gammaglobulin IgM         ASSAY GAMMAGLOBULIN IgM
*/
