 /*--------------------------------------------------------------*
  *    Name: hemreg.sas                                          *
  *   Title: Extract H and E matrices for multivariate regression*
        Doc: http://www.datavis.ca/sasmac/hemreg.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 13 Jan 2004 12:15:10                                *
  * Revised: 03 Nov 2006 09:09:35                                *
  * Version: 1.1                                                 *
  *  - Added SS= (default: SS2 -> SS3 for GLM)                   *
  *  1.1  Added MTEST= parameter and logic for multiple H tests  *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The HEMREG macro extracts hypothesis (H) and error (E) matrices for an
 overall test in a multivariate regression analysis, in a form similar to
 that provided by the OUTSTAT= option with PROC GLM.  This is typically
 used with the HEPLOT macro, or the MPOWER macro for MMRA.

==Method:

 For a multivariate regression analysis, using

   proc glm outstat=stats;
      model y1 y2 y3 = x1-x5;

 PROC GLM will produce 5 separate 3x3, 1 df SSCP matrices for the separate
 predictors X1-X5, in the OUTSTAT= data set, but no SSCP matrix for
 the overall multivariate test. The HEMREG macro uses PROC REG instead,
 obtains the HypothesisSSCP and ErrorSSCP tables using ODS, and massages
 these into the same format used by PROC GLM.


=Usage:

 The HEMREG macro is defined with keyword parameters.  The Y= and
 X= parameters are required.  One or more overall hypotheses involving
 subsets of the X= variables may be specified with the MTEST= parameter.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%hemreg(y=SAT PPVT RAVEN, x=N S NS NA SS);
	%hemreg(y=SAT PPVT RAVEN, x=N S NS NA SS, mtest=%str(N,S,NS), hyp=N:S:NS);
 
==Parameters:

* DATA=       Name of input dataset [Default: DATA=_LAST_]

* Y=          List of response variables.  Must be an explicit,
              blank-seaparated list of variable names, and all variables
			  must be numeric.  

* X=          List of predictor variables.  Must be an explicit,
              blank-seaparated list of variable names, and all variables
			  must be numeric. 

* HYP=        Name for each overall hypothesis tested, corresponding to
              the test(s) specified in the MTEST= parameter (to be used as 
			  the EFFECT= parameter in the HEPLOT macro).  [Default: HYP=H1]

* MTEST=      If MTEST= is not specified (the default), a multivariate test
              of all X= predictors is carried out, giving an overall H matrix.
			  Otherwise, MTEST= can specify one or more multivariate tests of
			  subsets of the predictors, separated by '/', where the variables
			  within each subset are separated by ','. In this case, the embedded
			  ','s must be protected by surrounting the parameter value in
			  %str().  For example, 
			  
			    MTEST = %str(group / x1, x2, x3 / x4, x5)

              In this case you might specify HYP=Group X1:X3 X4:X5 to name
			  the H matrices.

* SS=         Type of SSCP matrices to compute: Either SS1 or SS2, corresponding
              to sequential and partial SS computed by PROC REG. If SS=SS2,
			  the _TYPE_ variable in the output data set is changed to
			  _TYPE_='SS3' to conform with PROC GLM. [Default: SS=SS2] 

* OUT=        The name of output HE dataset [Default: OUT=HE]
                
 =*/


%macro hemreg(
	data=_last_,   /* name of input dataset                 */
	y=,            /* list of response variables            */
	x=,            /* list of predictor variables           */
	hyp=Hyp,       /* name(s) for hypothesis _source_       */
	mtest=,        /* set(s) of predictors to be tested     */
	ss=ss2,        /* type of sums of squares               */
	out=he         /* name of output HE dataset             */
	);
	
%local abort nx ny me;
%let me=HEMREG;
%let ny= %words(&y);
%let nx= %words(&x);
%let abort=0;
%*-- What about an intercept-only model?;
%if &nx=0 or &ny=0 %then %do;
	%put ERROR: The X= and Y= parameters are required;
	%let abort=1;
	%goto done;
	%end;

options nonotes;   
%*-- Kludge to terminate previous step w/o using RUN;
data _null_;
	_tmp_=0;
	run;

%local i mt nt dfh;
%*-- Parse the MTEST= value for separate tests;
%if %length(&mtest) %then %do;
	%let dfh=;
	%let i=1;
	%let mt = %scan(&mtest, &i, %str(/));
	%do %while (%length(&mt)>0);
		%let h&i = %scan(&hyp, &i);
		%if %length(&&h&i)=0 %then %let h&i = H&i;
		%let df&i = %words(%quote(&mt), dlm=%str(, ));
		%let dfh = &dfh &&df&i;
		%let mt&i = &mt;
		%put &me: Testing &&h&i: { &&mt&i }= 0, df=&&df&i;
		%let i = %eval(&i+1);
		%let mt = %scan(&mtest, &i, %str(/));
		%end;
	%*-- Number of MTESTs;
	%let nt = %eval(&i-1);
	%end;
%else %do;
	%let nt = 1;
	%let dfh = &nx;
	%end;


%*--  Extract the H & E matrices from MANOVA test(s);
ods listing close;
proc reg data=&data;
   model &y = &x / &ss ;
	%if %length(&mtest)=0 %then %do;
    	&hyp: mtest        / print ;   /* test all coeffs = 0 */
    	%end;
	%else %do i=1 %to &nt;
		&&h&i: mtest &&mt&i / print;
		%end;

  ods output HypothesisSSCP=H;
  ods output ErrorSSCP=E;
  run;
ods listing;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto done;

/*
title "&me: H and E datasets from REG";
proc print data=H;
proc print data=E;
run;
*/

data _null_;
   call symput('nobs',trim(left(put(_nobs,12.))));
   if 0 then set &data nobs=_nobs;
   stop;
run;

   /*
   Massage the H & E matrices into a form similar to the
   OUTSTAT= dataset from GLM
   */
options notes;
data &out;
	length _source_ _type_ $ 8 _name_ $16;
	set H (in=in1) E(in=in2);
	drop MTest model nl ne;
	array dfh{&nt} _temporary_ (&dfh);
	rename 
	%do i=1 %to &ny;
		col&i = %scan(&y, &i, %str( ))
		%end;
		;
	if in1 then do;  
*		_NAME_ = scan("&y", _n_);
*		_SOURCE_="&hyp";
		_name_ = scan("&y", 1 + mod(_n_-1, &ny), ' ');
		_source_ = MTest;
		%if %length(&mtest) %then %do;
			drop i;
			i = 1+int((_n_-1)/&ny);
			df = dfh[i];
			%end;
		%else %do;
			df= &nx; 
			%end;   
		%if %upcase(&ss) = SS1 %then %do;
			_TYPE_='SS1  ';
			%end;
		%else %do;
			_TYPE_='SS3  ';
			%end;
		output;
		end;
	else do;
		nl+1;
		ne=1+int((nl-1)/&ny);
		_name_ = scan("&y", 1 + mod(_n_-1, &ny), ' ');
		_source_='ERROR';
		_type_='ERROR';
		DF=&nobs - &nx -1 ;
		if ne=1 then output;
		end;
proc print data=&out;
	id _source_ _type_ _name_;
run;
	*-- delete H and E;
proc datasets lib=work memtype=data nolist nowarn;
   delete H E;
   run; quit;

%done:

%mend;


%macro words(string,root=,dlm=%str( ));
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %qscan(%quote(&string),&count,%quote(&dlm));
   %do %while(&word^= );
	%*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(%quote(&string),&count,%quote(&dlm));
   %end;
   %eval(&count-1)
%mend words;
