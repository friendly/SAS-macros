
/* Name:   byparm.sas                                               */
/* Title:  Process a sequence of SAS Steps by By-Groups             */
/*                                                                  */
/* Format:  %BYPARM( SAS_DataSet_Name , BY_Variable_List );         */
/*                                                                  */
/* SAS_DataSet_Name    specifies the SAS DataSet to process.        */
/*   If omitted, the last created SAS Data Set is used.             */
/*                                                                  */
/* BY_Variable_List    specifies a list of BY Variables describing  */
/*   the sort order.  The BY List may include the keywords:         */
/*   DESCENDING and NOTSORTED.  If the BY List is omitted, the      */
/*   entire SAS DataSet is processed in one pass.                   */
/*                                                                  */
/* The %BYPARM macro require the user to create a SAS Macro named   */
/*    %USER that contains the SAS Steps to be processed.            */
/*    Do NOT use the DATA= option on any of the PROC statements to  */
/*    specify the SAS DataSet to process. The SAS Dataset _TEMP_    */
/*    contains that portion of the original SAS data Set containing */
/*    the current BY Group.  If necessary, you may code the PROC    */
/*    Statement as:                                                 */
/*         PROC xxxx DATA=_TEMP_   other_info;                      */
/* If each SAS PROC Step has a "BY &BY;"  statement included, SAS   */
/*    will print BY Line titles at the top of every page of output. */
/* The %USER macro may begin with a suitable DATA _NULL_ step to    */
/*    extract the current values of the BY variables.  If the       */
/*    original SAS Data Set is to be processed according to the     */
/*    BY statement:     BY CHAR NUM;                                */
/*    the following DATA _NULL_ step will create macro variables    */
/*    containing the current BY Group values.                       */
/*          data _null_;                                            */
/*               set;                                               */
/*               call symput("C_VAL", char );                       */
/*               call symput("N_VAL", put(num, best4.) );           */
/*               stop;                                              */
/*           run;                                                   */
/*                                                                  */
/* Example %USER macro:                                             */
/*        %macro user;                                              */
/*          *Place SAS Steps here;                                  */
/*           proc print;                                            */
/*                by &by;                                           */
/*           run;                                                   */
/*        %mend user;                                               */

%macro byparm(data,by);

%if &data=  %then %let data = %scan(&sysdsn,1).%scan(&sysdsn,2);
%let data = %upcase(&data);

%let byx = %upcase(&by XXXXXXXXX);
%let byn = 0;                              /* Number of BY vars */
%let byvars = ;                            /* List of BY vars */
%let byequal = ;                           /* List of BY vars, each */
%let i = 0;                                /*    followed by = sign */

%do %until( &byvar = XXXXXXXXX );          /* Remove NOTSORTED and   */
    %let i = %eval(&i + 1);                /*    DESCENDING options  */
    %let byvar = %scan( &byx, &i );        /*    from the list of BY */
    %if &byvar ^= NOTSORTED                /*    vars                */
        %then %if &byvar ^= DESCENDING
        %then %if &byvar ^= XXXXXXXXX
        %then %do;
              %let byn = %eval(&byn + 1);
              %let byvars = &byvars &byvar;
              %let byequal = &byequal &byvar=;
              %end;
    %end;
/* Create an INDEX data set containing first and last obs for each */
/*    BY group                                                     */
%if &byn = 0 %then %do;
    %put %str( );
    %put WARNING: No BY= option specified. The entire SAS Data set is processed.
         ;
    %put %str( );
    %let groupn = 1;                       /* Number of BY groups */
    data _index_;
         _i_ = 1;
         set &data point=_i_ nobs=_nobs_;
         _first_ = 1;
         _last_ = _nobs_;
         keep _first_ _last_ ;
         output;
         stop;
    run;
    %end;
%else %do;
    %let lastby = %scan(&byvars, &byn);
    data _index_;
         retain _first_ _last_ _groupn_ 0;
         set &data end=_end_ nobs=_nobs_;
         by &by;
         if first.&lastby then _first_ = _n_;
         if last.&lastby
            then do;
                 _groupn_ + 1;
                 _last_ = _n_;
                 output;
                 end;
                                           /* Number of BY groups */
         if _end_ then call symput( 'groupn', left(put(_groupn_, 8.)) );
         keep _first_ _last_ &byvars;
    run;
    %end;
/* Repeat for each BY group */
%do group = 1 %to &groupn;
    data _null_;
         _i_ = &group;
         set _index_  point=_i_;
         put 'NOTE: Processing bygroup ' _i_ +3 &byequal;
         put 'NOTE: Obs: ' _first_ 'through ' _last_  " of SAS Data Set &data";
         call symput( 'first', left(put(_first_, 8.)) );
         call symput( 'last' , left(put(_last_ , 8.)) );
         stop;
    run;
    /* Subset original SAS Data Set */
    data _temp_;
         set &data( firstobs=&first obs=&last);
    run;
    /* Invoke USER routine */
    %user;
%end;
%mend byparm;

