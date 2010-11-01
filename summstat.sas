*----------------------------------------------------------*
  SUMMARY Macro -
    Formats and arranges proc SUMMARY output
  SAS SUGI, 1985 p.338
*----------------------------------------------------------*;
* (In CMS, a macro can have only 7 letters);
 
%MACRO SUMMARY (
   DATA = _LAST_,       /* Input dataset                            */
   OUT=_DATA_,          /* Output dataset                           */
   CLASS=, VAR=,        /* Vars for CLASS & VAR stmts               */
   STAT=MEAN,           /* Desired SUMMARY statistics               */
   FREQ=,               /* FREQ variable, if needed                 */
   SUMMOPT=,            /* Proc SUMMARY options e.g., MISSING       */
   PRINT=YES,           /* Printed output desired                   */
   PRINTOPT=,           /* Proc PRINT options                       */
   COLUMNS=STAT,        /* or VAR or CLASS - columns for dataset    */
   ROWSORT=CLASS        /* or VAR or STAT - primary output row sort */
   );
options dquote nonotes;
 
%* local variables & nested macros----;
%LOCAL NUMSTAT NUMCLASS NUMVAR ERROR TEMP X;
%LET ERROR= 0;
 
%MACRO ERROR (MESSAGE);
   %LET ERROR=1;
   %PUT ERROR: &MESSAGE..;
%MEND ERROR;
 
%MACRO VALLIST (LIST, VARNAME);
   %IF %INDEX(%QUOTE( &LIST ),%QUOTE( &&&VARNAME )) = 0
    OR %INDEX(%QUOTE(&&&VARNAME),%STR( )) >0
    %THEN %DO;
        %LET ERROR = 1;
        %PUT      ERROR: "&&&VARNAME" is invalid value for "&VARNAME".;
        %PUT %STR(       Valid values are: &LIST..);
    %END;
%MEND VALLIST;
 
%* Break up STAT= list (desired statistics);
%LET NUMSTAT=0;
%LET TEMP = %SCAN(%QUOTE( &STAT),1);
%DO %WHILE (%QUOTE(&TEMP) NE    );
    %LET NUMSTAT = %EVAL(&NUMSTAT + 1);
    %LOCAL STAT&NUMSTAT;
    %LET STAT&NUMSTAT = &TEMP;
    %LET TEMP = %SCAN(%QUOTE(&STAT), &NUMSTAT+1);
%END;
 
%* Get number of CLASS and VAR variables;
data _null_;
   if _N_ = 0 then set &DATA;
   __1 = 0;
   __2 = 0;
   call SYMPUT ('NUMCLASS',left(put( nmiss (OF __1 __2 &CLASS),3. ) ));
   call SYMPUT ('NUMVAR',  left(put( nmiss (OF __1 __2 &VAR  ),3. ) ));
   stop;
run;
%*PUT %STR( NUMCLASS=  &NUMCLASS  NUMVAR=  &NUMVAR );
 
%* Check for user errors;
%IF &NUMSTAT = 0 %THEN %ERROR( %STR(NULL "STAT=" list specified) );
%IF &NUMSTAT > 20
     %THEN %ERROR( %STR(No more than 20 statistics may be specified) );
%LET TEMP = N NMISS MEAN STD MIN MAX RANGE SUM VAR USS CSS CV STDERR T PRT;
%DO X = 1 %TO &NUMSTAT;
    %VALLIST (&TEMP, STAT&X);
%END;
 
%IF %QUOTE( &VAR) =     %THEN %ERROR( %STR("VAR=" must be specified) );
%VALLIST (YES NO, PRINT);
%VALLIST (STAT VAR CLASS, COLUMNS);
%VALLIST (STAT VAR CLASS, ROWSORT);
%IF %QUOTE (&COLUMNS) = %QUOTE (&ROWSORT)
     %THEN %ERROR ( %STR(COLUMNS and ROWSORT may not be the same) );
%IF %QUOTE (&COLUMNS) = CLASS & &NUMCLASS NE 1
     %THEN %ERROR ( %STR(MUST be exactly 1 CLASS variable if COLUMNS=CLASS));
%IF &ERROR %THEN %DO;
     options OBS=0 NOREPLACE;
     %GOTO ENDMAC;
%END;
 
%* Run proc SUMMARY;
proc summary data=&DATA  &SUMMOPT;
     %IF &CLASS NE    %THEN %DO;
     class &CLASS;
     %END;
     var   &VAR;
     %IF &FREQ  NE    %THEN %DO;
     freq  &FREQ;
     %END;
   output  out=&OUT   &STAT1=
     %DO X=2 %TO &NUMSTAT;
       &&STAT&X=__&X._1-__&X._&NUMVAR
     %END;
       ;
run;
 
options notes;
%* Reformat output from proc SUMMARY;
data &OUT;
     set _LAST_;
     keep &CLASS &VAR _TYPE_  _FREQ_  STAT  __S_ORD;
     length STAT $6
            __S_ORD 2;
     label STAT = 'STATISTIC'
           __S_ORD='Order of requested statistics';
 
     array __STAT1 &VAR;
  %DO X = 1 %TO &NUMSTAT;
     %IF &X > 1 %THEN %DO;
     array __STAT&X  __&X._1 - __&X._&NUMVAR ;
     do over __STAT1;
         __STAT1 = __STAT&X;
         end;
     %END;
     STAT = "&&STAT&X"; __S_ORD = &X; output;
   %END;
run;
 
%* Now transpose and/or sort if necessary;
%IF &COLUMNS = VAR %THEN %DO;
    %IF ROWSORT = STAT %THEN %DO;
       proc sort;
       by __S_ORD _TYPE_ &CLASS;
       run;
    %END;
    %* If ROWSORT is CLASS, its already in order;
%END;
 
%ELSE %DO;    %* Columns = STAT or CLASS;
    proc transpose  data=_LAST_
                    out =&OUT (rename=(_NAME_=VARIABLE _LABEL_=LABEL));
    var &VAR;
    by _TYPE_ &CLASS _FREQ_;
    id STAT;
    run;
 
    %* Create VAR with order of original variables, for possible sorting;
    data &OUT;
         label VARIABLE=VARIABLE
               LABEL=LABEL;
         length LABEL $40;
         set _LAST_;
         by _TYPE_ &CLASS _FREQ_;
         length __V_ORD 2;
         label  __V_ORD = 'Order of requested variables';
         if FIRST._FREQ_  then __V_ORD = 0;
         __V_ORD + 1;
    run;
    %IF &ROWSORT = VAR or &COLUMNS = CLASS %THEN %DO;
    proc sort;
         by __V_ORD  _TYPE_  &CLASS;
         run;
    %END;
 
    %IF &COLUMNS = CLASS %THEN %DO;
    proc transpose  data=_LAST_
                    out= &OUT (rename=(_NAME_=STAT));
         by __V_ORD variable label;
         id &CLASS;
         var &STAT;
         run;
    %* Recreate VAR with order of requested statistics;
    data &OUT;
         label STAT= 'Statistic';
         set _LAST_;
         by __V_ORD;
         length __S_ORD 2;
         label  __S_ORD = 'Order of requested statistics';
         if FIRST.__V_ORD then __S_ORD = 0;
         __S_ORD + 1;
         run;
 
    %IF &ROWSORT = STAT %THEN %DO;
    proc sort;
         by __S_ORD  __V_ORD;
         run;
     %END;
  %END;
%END;
 
%* NOw print, if desired;
%IF &PRINT = YES %THEN %DO;
    %IF %INDEX( &SUMMOPT,NWAY ) = 0 %THEN %LET CLASS = _TYPE_ &CLASS;
    %IF &COLUMNS=CLASS %THEN
        %LET PRINTOPT = (DROP=__V_ORD __S_ORD) &PRINTOPT;
    proc print data = _LAST_ &PRINTOPT;
    %IF &COLUMNS = VAR & &ROWSORT = CLASS %THEN %DO;
         by &CLASS;
         id STAT;
         var &VAR:
    %END;
    %ELSE %IF &COLUMNS = VAR & &ROWSORT = STAT %THEN %DO;
         by STAT notsorted;
         id &CLASS;
         var &VAR;
    %END;
    %ELSE %IF &COLUMNS = STAT & &ROWSORT = CLASS %THEN %DO;
         by &CLASS;
         id VARIABLE LABEL;
         var &STAT;
    %END;
    %ELSE %IF &COLUMNS = STAT & &ROWSORT = VAR   %THEN %DO;
         by VARIABLE LABEL notsorted;
         id &CLASS;
         var &STAT;
    %END;
    %ELSE %IF &COLUMNS = CLASS & &ROWSORT = VAR   %THEN %DO;
         by VARIABLE LABEL notsorted;
         id STAT;
      *  drop __V_ORD __S_ORD;
    %END;
    %ELSE %IF &COLUMNS = CLASS & &ROWSORT = STAT  %THEN %DO;
         by STAT notsorted;
         id VARIABLE LABEL;
      *  drop __V_ORD __S_ORD;
    %END;
%END;
 
%ENDMAC:
%MEND SUMMARY;
 
 
* Test data for SUMMARY macro;
 
data DEMO;
     length RACE $8;
     label HIGHSCH='High School Average'
           EDUCATN='Years of Education';
     do RACE = 'WHITE', 'BLACK', 'ASIAN', 'HISPANIC';
        do OBS = 1 to 50;
        EDUCATN= round(8 + 5 * uniform(0) + 4* normal (0));
        INCOME = 10000 + 2*EDUCATN + 1000 * NORMAL (0);
        HIGHSCH= 30 + 20 * UNIFORM (0) + 50*normal(0);
        output;
        end;
     end;
run;
options ls=80;run;
%SUMMARY (data=DEMO, out=sumry,
       VAR=INCOME EDUCATN HIGHSCH, CLASS=RACE,
       STAT=MEAN MIN MAX, COLUMNS=CLASS, ROWSORT=STAT,
       SUMMOPT=NWAY MISSING);
proc print data=sumry;
 
%SUMMARY (data=DEMO,
       VAR=INCOME EDUCATN HIGHSCH, CLASS=RACE,
       STAT=MEAN MIN MAX, COLUMNS=STAT, ROWSORT=VAR,
       SUMMOPT=NWAY MISSING);
