/*--------------------------------------------------------------------*/
/* title:  Checks for existence of a variable in a SAS dataset        */
/* %varexist(dataset, var)                                            */
/*    Returns 1 or 0 depending on whether the dataset exists          */
/*--------------------------------------------------------------------*/
%macro varexist( inset, varname );
        %local
                vexist
                ;
        %let dsid=%sysfunc( open( &inset, i));
        %if &dsid %then %do;
                %let vexist=%sysfunc( varnum( &dsid, &varname));
                %let rc=%sysfunc( close( &dsid));
        %end;
        
        &vexist
        
%mend varexist;
