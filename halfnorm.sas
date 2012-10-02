 /*-------------------------------------------------------------------*
  *    Name: halfnorm.sas                                             *
  *   Title: Half normal plot for generalized linear models           *
        Doc: http://www.datavis.ca/sasmac/halfnorm.html            
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 08 Nov 1998  9:51                                        *
  * Revised: 02 Oct 2012 08:49:52                                     *
  * Version: 1.2-1
  *  1.1  Fixed make ... noprint for V7+                             
     1.2  Added NEGBIN
	      Modified to use output statement rather than ODS
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The HALFNORM macro plots the ordered absolute values of residuals
 from a generalized linear model against expected values of normal
 order statistics.  A simulated envelope, correponding to an
 approximate 95% confidence interval, is added to the plot to aid
 assessment of whether the distribution of residuals corresponds
 to a good-fitting model.

=Usage:

 The HALFNORM macro is called with keyword parameters.  The RESP=
 and MODEL= parameters are required.  The arguments may be listed
 within parentheses in any order, separated by commas. For example:

	%halfnorm(resp=count, class=sex response, model=sex|year|response@2);

 
==Parameters:

* DATA=		Specifies the name of the input data set to be analyzed.
            The default is the last data set created.

* Y=
* RESP=     Specifies the name of the response variable to be analyzed

* TRIALS=   The name of a trials variable, for DIST=BIN, with the data
            in events/trials form.

* MODEL=    Specifies the model formula, the right-had-side of the
            MODEL statement.  You can use the | and @ shorthands.

* CLASS=    Names of any class variables in the model.

* DIST=     Error distribution. One of NORmal, BINomial, POIsson, GAMma or
            NEGbin; can be abbreviated to the first 3 characters.  [Default: DIST=NORMAL].

* LINK=     Link function.  The default is the canonical link for the
            DIST= error distribution.

* OFFSET=   The name(s) of any offset variables in the model.

* MOPT=     Other model options (e.g., NOINT, SCALE=, etc.)

* FREQ=     The name of a frequency variable, when the data are in grouped
            form.

* ID=       The name of a character variable used as an observation
            identifier in the plot.

* OUT=		Specifies the name of the output data set. The output data
            set contains the input variables, absolute residuals (_ARES_),
				half-normal expected value (_Z_),
				[Default: _RES_].

* LABEL=    Specifies whether and how to label observations in the plot.
            LABEL=ALL means that all observations are labelled with the
				ID= variable value; LABEL=NONE means that no observations are
				labelled; LABEL=ABOVE means that observations above the mean
				of the simulations are labelled; LABEL=TOP n means that the
				highest n observations are labelled. [Default: TOP 5]
				
* SEED=     Specifies the seed for the random number generators. SEED=0
            (the default) uses the time-of-day as the seeed, so a
				different set of simulated observations is drawn each time
				the program is run.

* RES=      The type of residual to plot.  Possible values are:
            STRESCHI (adjusted Pearson residual), STRESDEV (adj. deviance
				residual).
            
* NRES=     Number of simulations for the confidence envelope. [Default: 19]

* SYMBOL=   Plotting symbol for residuals. [Default: dot]

* INTERP=   Interpolation for residuals. [Default: none]

* COLOR     Color for residuals. [Default: red]

* NAME=     Graph name in graphics catalog. [Default: halfnorm]

* GOUT=     The name of the graphics catalog. [Default: GSEG]

==Dependencies:

 Requires: labels.sas

 =*/
 
%macro halfnorm(
   data=_last_,    /* Name of input data set                     */
   y=,             /* Name of response variable                  */
   resp=,          /* Name of response variable                  */
   trials=,        /* Name of trials variable (dist=bin only)    */
   model=,         /* Model specification                        */
   class=,         /* Names of class variables                   */
   dist=,          /* Error distribution                         */
   link=,          /* Link function                              */
   offset=,        /* Offset variable(s)                         */
   mopt=,          /* other model options (e.g., NOINT)          */
   freq=,          /* Freq variable                              */
   id=,            /* Name of observation ID variable (char)     */
   out=_res_,      /* output data set                            */
   label=top 5,    /* NONE|ALL|ABOVE|TOP n                       */
   seed=0,         /* Seed for simulated residuals               */
   res=stdresdev,  /* Type of residual to use: strdeschi/stdresdev */
   nres=19,        /* Number of simulations for envelope         */
   symbol=dot,     /* plotting symbol for residuals              */
   interp=none,    /* interpolation for residuals                */
   color=red,      /* color for residuals                        */
   name=halfnorm,  /* graph name in graphics catalog             */
   gout=
);

%let label=%upcase(&label);
%let abort=0;
%if %length(&model) = 0 %then %do;
    %put ERROR: List of model terms (MODEL=) is empty.;
    %let abort=1;
    %goto done;
    %end;

%if %length(&resp) = 0 %then %let resp=&y;
%if %length(&resp) = 0 %then %do;
    %put ERROR: No response (RESP= or Y=) has been specified.;
    %let abort=1;
    %goto done;
    %end;

%let dist=%upcase(&dist);
%if %length(&dist) = 0 %then %do;
    %put WARNING: No distribution (DIST=) has been specified.;
    %put WARNING: GENMOD will use DIST=NORMAL.;
    %end;

%let lres=&res;
/*      %if %upcase(&res)=STRESDEV %then %let lres=Std Deviance Residual; */
      %if %upcase(&res)=STDRESDEV %then %let lres=Std Deviance Residual;
%else %if %upcase(&res)=STRDESCHI %then %let lres=Std Pearson Residual;
%else %if %upcase(&res)=RESDEV   %then %let lres=Deviance Residual;
%else %if %upcase(&res)=RESCHI   %then %let lres=Pearson Residual;
%else %if %upcase(&res)=RESLIK   %then %let lres=Likelihood Residual;
%else %if %upcase(&res)=RESRAW   %then %let lres=Raw Residual;
%else %do;
    %put WARNING: Residual type &res is unknown.  Using RES=STDRESDEV;
    %let res=stdresdev;
	 %let lres=Std Deviance Residual;
    %goto done;
	%end;

%put HALFNORM: Fitting initial model: &resp = &model.;
%*let _print_=OFF;
*ods listing close;
*ods output ObStats=work._obstat_ ;
ods output ParameterEstimates=work.parm(where=(Parameter='Dispersion'));
proc genmod data=&data;
   %if %length(&class)>0 %then %do; class &class; %end;
   %if %length(&freq)>0 %then %do;  freq &freq;   %end;
	%if %length(&trials)=0 %then %do;
	   model &resp         = &model /
		%end;
	%else %do;
	   model &resp/&trials = &model /
		%end;
	%if %length(&dist)>0 %then %do;  dist=&dist %end;
	%if %length(&link)>0 %then %do;  link=&link %end;
	%if %length(&offset)>0 %then %do; offset=&offset  %end;
	%if %length(&mopt)>0 %then %do;  %str(&mopt) %end;
/*		obstats residuals */
		;
	output out=_obstat_ pred=pred &res=res;
  run;



%*-- Find variables listed in model statment;
data _null_;
	length xv $200;
	xv = translate("&model", '    ', '(|)*');
	at= index(xv,'@');
	if at then xv=substr(xv,1,at-1);
	call symput('xvars', trim(left(xv)));
run;
%*put xvars=&xvars;

%*-- Generate simulated response values from the error distribution;
data _obstat_;
	%if %substr(&dist,1,3) = NEG %then %do;
		if _n_  = 1 then set parm(keep = estimate);
		%end;
   merge &data(keep=&resp &trials &freq &class &xvars &offset &id)
	      _obstat_(keep=pred res);

	array _y_{&nres} _y1 - _y&nres;
	drop i seed;
	retain seed &seed;
	do i=1 to dim(_y_);
		%if %substr(&dist,1,3)=NOR %then %do;
			call rannor(seed, _y_[i]);
			_y_[i] = pred + _y_[i];
			%end;
		%else %if %substr(&dist,1,3)=BIN %then %do;
			n=&trials;
			call ranbin(seed, n, pred, _y_[i]);
			%end;
		%else %if %substr(&dist,1,3)=POI %then %do;
			call ranpoi(seed, pred, _y_[i]);
			%end;
		%else %if %substr(&dist,1,3)=GAM %then %do;
			call rangam(seed, pred, _y_[i]);
			%end;
		%else %if %substr(&dist,1,3) = NEG %then %do;
		   teta = 1 / estimate;
		   _y_[i] = rand('NEGBIN', (teta /(teta+pred)), teta);
		%end;
		end;
run;

%if %length(&id)=0 %then %do;
%put WARNING: No ID= was given.  Using observation number.;
data _obstat_;
	set _obstat_;
	_id_ = left(put(_n_,10.));
run;
%let id=_id_;
%end;

%put HALFNORM: Generating &nres simulated residual sets...;
%if %sysevalf(&sysver  >= 7) %then %do;
	ods listing exclude all;
	%end;
options nonotes;
%do i=1 %to &nres;
%*put HALFNORM: Generating residual set &i;
	
*ods output ObStats=work._hres&i(keep=&res rename=(&res=res&i));
proc genmod data=_obstat_;
   %if %length(&class)>0 %then %do; class &class; %end;
   %if %length(&freq)>0 %then %do;  freq &freq;   %end;
	%if %length(&trials)=0 %then %do;
	   model _y&i         = &model /
		%end;
	%else %do;
	   model _y&i/&trials = &model /
		%end;
	%if %length(&dist)>0 %then %do;  dist=&dist %end;
	%if %length(&link)>0 %then %do;  link=&link %end;
	%if %length(&offset)>0 %then %do; offset=&offset  %end;
	%if %length(&mopt)>0 %then %do;  %str(&mopt) %end;
	/*
		obstats residuals
	*/
		;
	output out=work._hres&i &res = res&i;
  run;

%end;  /* End %do i */
*let _print_=ON;
*ods listing;
%if %sysevalf(&sysver  >= 7) %then %do;
	ods listing exclude none;
	%end;

%*-- Merge residuals, calculate absolute values;
data _obstat_;
	merge _obstat_
	%do i=1 %to &nres;
		_hres&i
		%end;
		;
	drop i _y1-_y&nres;
	array _res_{&nres}  res1-res&nres;
	do i=1 to dim(_res_);
		if _res_[i]^=. then _res_[i] = abs(_res_[i]);;
		end;
	_ares_ = abs(res);

proc sort data=_obstat_;
	by _ares_;
run;

%*-- Sort each set of residuals;
proc iml;
start sortcols(X);
	*-- Sort columns, allowing for missing values;
	do i=1 to ncol(X);
		xi = x[,i];
		if any(xi=.) then do;
			mi = xi[loc(xi=.),];
			xi = xi[loc(xi^=.),];
			end;
		else free mi;
		t = xi; r = rank(xi);
		t[r] = xi;
		x[,i] = mi//t;
		end;
	finish;

start symput(macnm,scal);
  *-- give macro variable &"macnm" the value of the scalar ;
  call execute('%let ',macnm,'=',char(scal),';');
finish;

	use _obstat_;
	read all var( "res1" : "res&nres" ) into X;
	nc=0;
	do i=1 to ncol(X);
		if ^all(X[,i]=.) then do;
			Y = Y || X[,i];
			nc = nc+1;
			end;
		end;
	run symput('nc', nc);
	run sortcols(Y);
	create _sorted_  from Y;
	append from Y;
	quit;

%put HALFNORM: There are &nc sorted columns of simulated residuals;
%if &nc=0 %then %do;
	%let abort=1;
	%goto done;
	%end;

data _obstat_;
	merge _obstat_ _sorted_;
	array _res_{*} col1-col&nc;
	drop res1-res&nres;
	
	resmin = min(of col1-col&nc);		
	resmax = max(of col1-col&nc);
	resmean = mean(of col1-col&nc);		
	run;

options notes;
data &out;
	set _obstat_ nobs=nobs end=eof;
	drop col1-col&nres;
	_z_ = probit((_n_ + nobs - .125)/(2*nobs + .5));
	label _z_='Expected value of half normal quantile'
		_ares_="Absolute &lres";
	if eof then call symput('nobs', left(put(nobs,best8.))); 
run;
options nonotes;

%if &label ^= NONE %then %do;
	%if &label=ALL   %then %let subset=1;
	%if &label=ABOVE %then %let subset=(_ares_>resmax);
	%if %scan(&label,1)=TOP %then %do;
		%let which = %scan(&label,2);
		%if %length(&which)=0 %then %let which=5;
		%let subset = (_n_ > %eval(&nobs-&which));
		%end;
%label(data=&out, x=_z_, y=_ares_, text=&id, out=_labels_, pos=4,
	subset=&subset, xoff=-.04);
%end;

title2;

proc gplot data=&out;
	plot _ares_  * _z_ = 1
	     resmean * _z_ = 2 
	     resmin  * _z_ = 3 
	     resmax  * _z_ = 3 / overlay  
		vaxis=axis1 frame vm=1 hm=1 
		%if &label ^= NONE %then %do;
		anno=_labels_
		%end;
		name="&name" des="Half normal plot for &resp in &data";
	symbol1 v=&symbol c=&color i=&interp;
	symbol2 v=none    c=black  i=join;
	symbol3 v=none    c=gray60 i=join l=3;
	axis1 label=(a=90);
run; quit;

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
proc datasets nofs nolist nowarn library=work memtype=(data);
    delete _obstat_
	%do i=1 %to &nres;
		_hres&i
		%end;
	 ;
	 run; quit;

%done:
options notes;
%if &abort %then %put ERROR: The HALFNORM macro ended abnormally.;
%mend;

