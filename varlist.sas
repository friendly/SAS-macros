%macro varlist(data=, list=);

/* Make the result global and clear any previous value */
%global longlist;
%let longlist=;
proc sql noprint;

/* Set up a view that contains only the required varaibles */
     create view NAMES as
     select * from &data(keep=&list)
;
/* Retrieve the names from the data dictionary and store in LONGLIST */
     select name into :longlist separated by ' '
     from dictionary.columns
     where libname="WORK" and memname="NAMES"
     and memtype='VIEW'
;
%mend;

/* Test the result on a well known data set:
%varlist(data=sasuser.class, list=sex--weight);
%put &longlist;

%varlist(data=sasuser.class, list=h:);
%put &longlist;
*/