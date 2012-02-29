 /*-----------------------------------------------------------------*
  |     Name: datachk.sas                                           |
  |    Title: Perform basic data checking on a dataset              |
        Doc: http://www.datavis.ca/sasmac/datachk.html        
  | ----------------------------------------------------------------|
  |    Procs: contents sort transpose means standard print datasets |
  |           rank                                                  |
  |  Macdefs: datachk hilo                                          |
  |  Macrefs: hilo splot                                            |
  | ----------------------------------------------------------------|
  |   Author: Michael Friendly <friendly@yorku.ca>                  |
  |  Created: 12 Aug 1991 15:46:41                                  |
  |  Revised: 09 Oct 2004 13:34:43                                  |
  |  Version: 1.5                                                   |
  | - Fixed bug when no variable has a label                        |
  | - Cleaned up work data sets                                     |
  | - Added display of obs with at least &nout |z-scores| > &zout   |
  | - Handle missing value codes (.A-.Z)                            |
  | 1.5                                                             |
  | - Replaced splot with boxplot for hi-res plot                   |
  |                                                                 |
  *-----------------------------------------------------------------*/

/*=

=Description:

 The DATACHK macro performs basic data screening/checking on numeric variables
 in a dataset, and is designed to give a compact overview of many variables.
 The output is a small subset of that produced by
 the univariate procedure.  For a each variable, a few statistics
 and the lowest/highest observations are shown in a format like:

  Variable             Stat    Value       Extremes Id


  ATBAT                N            322          16 Tony Armas
                       Miss           0          19 Cliff Johnson
  Times at Bat         Mean    380.9286          19 Terry Kennedy
                       Std      153.405          20 Mike Schmidt
                       Skew    -0.07806
                                                 663 Joe Carter
                                                 677 Don Mattingly
                                                 680 Kirby Puckett
                                                 687 T Fernandez
                     --------------------------------------------


 In addition, a schematic plot (boxplot) is shown for the standard
 scores for all variables when the SPLOT=Y option is given (assuming
 the BOXPLOT macro is available.)

 If a CLASS= variable is specified, this output is produced for each
 value of the CLASS= variable.

 The macro also prints a table of standardized (Z) scores, for all
 observations which have at least NOUT z-scores greater than ZOUT
 in absolute value.

=Usage:

 The DATACHK macro takes keyword arguments.  The VAR= variable list
 is required.

==Parameters:

* DATA=_last_        Name of input data set

* VAR=               Variable(s) to be screened.  You may use any of the
                     shorthands for variable lists, e.g., X1-X20,
					 STATA--STATZ, _NUMERIC_.

* CLASS=             Class/grouping variable

* ID=                Name of id variable, used to label observations
                     in the output.

* OUT=_hilo_         Name of the output dataset, containing the
                     highest and lowest observations on each variable.

* LS=80              Output linesize, mainly used for the boxplots
                     produced by the SPLOT option.

* LOHI=4             Number of low/high obs printed

* ZOUT=2             Z-score for treating an obs as unusual

* NOUT=3             Number of |z|>&zout to print. Use NOUT=0 to
                     suppress this output.

* SPLOT=yes          Do boxplot of std scores? 

 =*/

%macro datachk(
   data=_last_,    /* name of input data set                 */
   var=,           /* variable(s) to be screened             */
   class=,         /* class/grouping variable                */
   id=,            /* name of id variable                    */
   out=_hilo_,     /* name of the output dataset             */
   ls=80,          /* linesize  for %splot                   */
   lohi=4,         /* number of low/high obs printed         */
   zout=2,         /* z-score for treating an obs as unusual */
   nout=3,         /* number of |z|>&zout to print           */
   splot=yes,      /* do boxplot of std scores?              */
   idheight=1
   );

%let abort=0;
%let work=;
%if %upcase(&data)=_LAST_ %then %let data = &syslast;

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes /*validvarname=upcase */;
		%end;
	%else %do;
	   options nonotes;
		%end;

%if %length(&var) =0 %then %do;
   %put DATACHK: No variable(s) have been specified;
   %put %str(       )Specify a VAR= variable.;
    %let abort=1;
   %goto done;
   %end;

%*-- Make sure at least one variable has a label (so id _label_ wont fail);
data _new_;
    set &data;
    keep &var;

proc contents data=_new_ out=_work_ noprint;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%let nlabs=0;
data _null_;
    set _work_(keep=name label) end=eof;
    if label^=' '
        then nlabs + 1;
        else var=name;
*   put label= nlabs=;
    if eof then do;
        call symput('nlabs', put(nlabs,4.));
        call symput('onevar', var);
        end;
    run;
*put DATACHK: nlabs= &nlabs onevar= &onevar;
data _new_;
    set &data;
    %if &nlabs=0 %then %do;
        label &onevar = "&onevar";
        %end;
    %if %length(&id) =0 %then %do;
        _id_ = _n_;
        %let id=_id_;
        %end;
%let data=_new_;

%if %length(&class) >0 %then %do;
   proc sort data=&data out=_sorted_;
       by &class;
   data _sorted_;
      set &data;
      run;
   %let data=_sorted_;
    %let work = &work _sorted_;
   %end;

proc transpose data=&data  out=_trans_;
  var &var;
  by &id notsorted;
  %if %length(&class) >0 %then %do;
  copy &class;
  %end;

proc sort;
   by &class _name_;

%put DATACHK: Summarizing the variables...;
proc means data=_trans_ noprint;
   var col1;
   by &class _name_;
   id _label_;
   output out=_stat_ n=n nmiss=nmiss mean=mean std=std skewness=skew;
data _stat_;
   set _stat_;
   rename _freq_=nobs;
*proc print;

*-- Find lowest and highest observations;
%put DATACHK: Finding the &lohi Highest/Lowest values...;

%hilo(in=_trans_, out=_trup_, var=col1, id=&id, class=&class, dir=UP,
      num=&lohi);
%hilo(in=_trans_, out=_trdn_, var=col1, id=&id, class=&class, dir=DN,
      num=&lohi);

%put DATACHK: Printing the report...;
data _hilo_;
   merge _stat_ _trup_ _trdn_;
   by &class _name_;
   file print linesleft=lines;
   drop c1-c6 s1-s5 dash flag;
   array s{*} $4 s1-s5 ( 'N' 'Miss' 'Mean' 'Std' 'Skew' );
   array cols{*} c1-c6 ( 22 30 42 51 60 72 );
   if _n_=1 then do;
      put 'Variable'
          @c1 'Stat' @c2 'Value' @c3 'Extremes' @c4 'Id'//;
      end;
   if lines < 12 then put _page_;
   dash = repeat('-',c6-c2);
	flag = '  ';
	if abs(skew)>2 then flag='**';
	else if abs(skew)>1 then flag='*';
   put _name_ $20.  @c1 s1 @c2 n     best8. @c3 lo1 best8. @c4 idlo1 /
                    @c1 s2 @c2 nmiss best8. @c3 lo2 best8. @c4 idlo2 /
       _label_ $20. @c1 s3 @c2 Mean  best8. @c3 lo3 best8. @c4 idlo3 /
                    @c1 s4 @c2 std   best8. @c3 lo4 best8. @c4 idlo4 /
                    @c1 s5 @c2 skew  best8. +1 flag                  /
                                            @c3 hi4 best8. @c4 idhi4 /
                                            @c3 hi3 best8. @c4 idhi3 /
                                            @c3 hi2 best8. @c4 idhi2 /
                                            @c3 hi1 best8. @c4 idhi1 /
                    @c1 dash //
       ;

%if %upcase(%substr(&splot,1,1))=Y %then %do;
/* Convert to standard scores for comparative boxplots */
%put DATACHK: Preparing boxplot of standard scores...;
proc standard data=_trans_ out=_stand_ m=0 s=1;
   var col1;
   label col1='Standard score'
    _name_='Variable';
   by _name_;
	%* Exclude a numeric ID;
	%if %length(&id) %then %do;
*		where (_name_ ^= "&id");
		%end;
run;
%let work=&work _stand_;
*splot(data=_stand_,class=_name_,var=col1,ls=&ls);
*options nonotes;
axis90 label=(a=90) minor=(number=1);
%boxplot(data=_stand_,class=_name_,var=col1, classlab=Variable,
	vaxis=axis90, id=&id, olabel=far, idheight=&idheight);

/*
proc boxplot data=_stand_;
	plot col1 * _name_ / 
		boxstyle = schematicidfar 
		cboxes=black idheight=&idheight
		vaxis=axis90;
	%if %length(&id) %then %do;
		id &id;
		%end;
	run; quit;
*/
%end;

%if &nout>0 %then %do;
proc standard data=&data out=_new_ m=0 s=1;
   var &var;
    %if %length(&class) >0 %then %do;
        by &class;
        %end;

data _new_;
   set _new_;
    array vars{*} &var;
	 drop i;
    _out_ = 0;
    do i=1 to dim(vars);
        _out_ = sum(_out_, abs(vars(i)) > &zout);
        end;
proc sort;
    by descending _out_;
proc print;
    var &var;
    id &id _out_;
    where (_out_>=&nout);
    format &var 5.2;
    title2 "Observations with at least &nout |z-scores| > &zout";
%end;

proc datasets nolist nowarn;
   delete _new_ _trup_ _trdn_ _stat_ _rank_ _id_ _trans_ _tr_ &work;
   quit;

title2;
%done:;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%if &abort %then %put ERROR: The DATACHK macro ended abnormally.;
%mend;

 /* ---------------------------------------------------------------*/

%macro hilo(in=,          /* input data set                        */
            out=,         /* output data set                       */
            var=col1,     /* variable to analyze                   */
            id=,          /* id variable                           */
            num=,         /* number of extreme observations wanted */
            class=,       /* grouping variable */
            dir=up        /* which extreme: UP or DN               */
                );
%let dir=%upcase(&dir);
%if &dir=UP
    %then %do;
       %let pre=LO;
       %let order=;
    %end;
    %else %do;
       %let pre=HI;
       %let order=DESCENDING;
    %end;

*-- Find lowest or highest num  observations;
proc rank data=&in out=_rank_ ties=low &ORDER;
	where (&var > .Z);
   var &var;
   by &class _name_;
   ranks _rank_;
proc sort data=_rank_;
   by &class  _name_ _rank_;
data _rank_;
   set _rank_;
   if _rank_ ^= .;
data _rank_;
   set _rank_;
   by &class _name_;
   rename &id =_id_;
 * rename col1=_lo_;
   if first._name_ then _r_=0;
   _r_+1;
   if _r_ <= &num;
   drop /*_label_*/ _rank_ _r_;

proc transpose data=_rank_ out=_tr_ prefix=&PRE;
   var &var;
   by &class _name_;
proc transpose data=_rank_ out=_id_ prefix=id&PRE;
   var _id_;
   by &class _name_;
data &out;
   merge _tr_ _id_;
   by &class _name_;
   drop _label_;
*proc print;
%mend;
