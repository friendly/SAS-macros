/* --------------------------------------------------------------------------
     NAME:   %makefmt                       May 26, 1994  by Michael Gibson
     DESC:   This macro creates a format from a SAS data set or view.
             It does a SORT NODUPKEY to make sure formatting unique ranges.
 
     USAGE:  %makefmt( NAME, DSN, START=, LABEL=
                                 <, TYPE=,  PERM=, END=, OTHER=, PRINT= >  );
 
     PARMS:  - Required -
             NAME -  Name of format.  ( "$" not needed see "TYPE" below ).
                     ( ie:  MYFMT  equates to $MYFMT for character formats or
                                              MYFMT for numeric formats. )
 
             DSN  -  Name of SAS data set or view used to create format.
                     Single level name assumes "work."
                     ( ie:    "sasuser.myfmt"  pulls from libref "SASUSER"
                       while  "myfmt"  pulls from "WORK." )
 
             START - Name of the variable used for the start of the range
                     values or single value. ( left-of-equal-sign value )
                     ( ie:  START=MODEL where MODEL is a variable on DSN )
 
             LABEL - Name of the variable used for the formatted-value (label)
                     or a quoted value.  ( right-of-equal-sign value )
                     ( ie: LABEL=DESC where DESC is a variable on DSN or
                           LABEL="YES"    see selection example below )
 
             - Optional _
             TYPE -  Type of format.  N = Numeric Format
                                      P = Picture Format
                                      I = Numeric Informat
                                      C = Character Format   ( Default )
                                      J = Character Informat
                     When type is "C" or "J" the resulting format is prefixed
                     with a dollar sign ( $ ).
 
             PERM -  Flag to create a permanent or temporary format.
                                      N = temporary format created. ( Default )
                                      Y = permanent format created must have
                                          libref "LIBRARY" allocated via
                                          libname statement.
 
             END   - Name of the variable used for the end of the range
                     values. ( left-of-equal-sign value )

             OTHER - A quoted value for the OTHER option of formats.
 
             PRINT - Flag to print formats as they are created.
                                      N = Do not print format. ( Default )
                                      Y = Print the format.
 
     EXAMPLES:  %makefmt( myfmt, sasuser.myfmt, start=model, label=desc );
                      Generates a character format called "$myfmt" from dsn
                      "sasuser.myfmt" using the variable "model" for range
                      values and the variable "desc" for label values.
 
                %makefmt( myfmt, sasuser.myfmt, start=model, label="YES" );
                      Generates a character format called "$myfmt" from dsn
                      "sasuser.myfmt" using the variable "model" for range
                      values and assigns "YES" as label values constants.
 
  ------------------------------------------------------------------------- */

%macro makefmt
   ( fmtname,  dsn,     start=,  label=,
     type=C,   perm=N,  end=,    other=,  print=N );
 
   %let type  = %upcase( &type  );
   %let perm  = %upcase( &perm  );
   %let print = %upcase( &print );
   %let other = %quote( &other );
   %if %substr( &fmtname, 1,1 ) eq %str($) %then %do;
       %let type = C;
       %let fmtname = %substr( &fmtname, 2 );
   %end;
 
   DATA WORK._FMT_;       /* create a dsn used on cntlin= of PROC FORMAT */
        LENGTH  START   LABEL $200;
        RETAIN FMTNAME "&fmtname"  TYPE "&type";
        KEEP FMTNAME  TYPE  START  LABEL
             %if &other ne %str( ) %then %str( HLO);
             %if &end   ne %str( ) %then %str( END);
             %str(;);
        SET &dsn      END=EOF;
        START = &start;
       %if &end   ne %str( ) %then %str( END=&end;);
        LABEL = &label;
 
        OUTPUT;
        %if &other ne %str( ) %then %do;
            IF EOF THEN DO;
               START = 'OTHER';
               LABEL = "&other";
               HLO='O';
               OUTPUT;
            END;
        %end;
    RUN;
 
    PROC SORT DATA=WORK._FMT_ NODUPKEY;    /* remove dups on the start value */
         by START;
    RUN;
 
    PROC FORMAT CNTLIN=WORK._FMT_
         %if &perm  eq %str(Y) %then %str( LIBRARY=LIBRARY );
         %if &print eq %str(Y) %then %str( FMTLIB );
         %str(;);
    RUN;
 %mend makefmt;
/*
-------------------------------- end --------------------------------- 
------------------------------------------------------------------------
  Michael R. Gibson                         The opinions expressed are
  StorageTek, Louisville CO  USA            those of my own and do not
  michael_gibson@stortek.com                reflect those of my employer.
*/

