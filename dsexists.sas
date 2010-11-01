/*--------------------------------------------------------------------*/
/* title:  Checks for existence of a SAS dataset                      */
/* %dsexists(dataset)                                          .      */
/*    Returns 1 or 0 depending on whether the dataset exists          */
/*--------------------------------------------------------------------*/
%macro dsexists(dataset,memtype=DATA);
   %global isthere;

   %if (%length(%scan(&dataset,2,.))) %then %do;
      %let lib = %scan(&dataset,1,.);
      %let ds  = %scan(&dataset,2,.);
      %end;
   %else %do;
      %let lib = WORK;
      %let ds  = &dataset;
      %end;
  proc sql noprint;
   select count(*)
     into :isthere from dictionary.members
        where   (libname ? upcase("&lib"))
              & (memname ? upcase("&ds" ))
              & (memtype ? upcase("&memtype"));
  quit;
  &isthere
%mend;
