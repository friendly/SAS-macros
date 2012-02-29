 /*-------------------------------------------------------------------*
  *    Name: addvar.sas                                               *
  *   Title: Added variable plots for logistic regression             *
        Doc: http://www.datavis.ca/sasmac/addvar.html              
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created: 15 Apr 98 11:16                                          *
  * Revised: 23 Feb 2005 15:45:30                                     *
  * Version: 1.1                                                      *
  *  1.1   Fixed validvarname for V7+                                 *
  *        Changed validvarname=V6 to validvarname=upcase             *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The ADDVAR macro produces added variable plots (TYPE=AVP) for the
 effect of adding a variable to a logistic regression model, or a
 constructed variable plot (TYPE=CVP) for the effect of transforming
 a variable.
 
 For a model with a binary response, Y, and predictors in the list
 X, an added variable plot may be constructed for a new predictor,
 Z, by plotting the residuals of Y given X against the residuals
 of Z given X.  A linear relation in this plot indicates that Z
 should be included in the model, but observations with extreme
 Z-residuals would be highly influential in this decision.  A line
 fitted to this plot should have an intercept approximately zero,
 and a slope approximating the coefficient of Z in the full model.

 The constructed variable plot is designed to detect nonlinear dependence
 of Y on one of the X variables, say X[j].  It is an added variable
 plot for the constructed variable, Z = X[j] log X[j].
 
=Usage:

 The addvar macro is called with keyword parameters.  The X=, Y=,
 and Z= parameters must be specified.  A TRIALS= variable may be
 specified if the data are in events/trials form.  The arguments
 may be listed within parentheses in any order, separated by commas.
 For example:
 
	%addvar(data=icu, y=Died,  x=age admit cancer uncons, z=Systolic,
		id=patient, loptions=order=data noprint);

 This gives an AVP for the variable Systolic, when added to the X=
 variables in the model predicting Y=DIED.
 
==Parameters:

* DATA=		  Specifies the name of the input data set to be analyzed.
              [Default: DATA=_LAST_]

* Y=	   	  Specifies the name of the response variable.

* TRIALS=     Name of trials variable for event/trial

* X=	   	  Specifies the names of the predictor variables in the model

* Z=          Name of the added variable

* ID=         Name of observation ID variable (char)

* LOPTIONS=   Options for PROC LOGISTIC [Default: LOPTIONS=NOPRINT]

* SMOOTH=     Lowess smoothing parameter [Default: SMOOTH=0.5]

* SUBSET=     Subset of points to label [Default: SUBSET=ABS(STUDRES)>2]

* OUT=        Specifies the name of the output data set [Default: OUT=_RES_]

* SYMBOL=     Plotting symbol for points [Default: SYMBOL=DOT]

* INTERP=     Interpolation options for points [Default: INTERP=RL CI=RED]

* TYPE=       Type of plot: AVP or CVP [Default: TYPE=AVP]

* NAME=       Name of graph in graphic catalog [Default: NAME=ADDVAR]

* GOUT=       Name of the graphics catalog

 =*/
 
%macro addvar(
	data=_last_,       /* Name of input data set                  */
	y=,                /* Name of response variable               */
	trials=,           /* Name of trials variable for event/trial  */
	x=,                /* Names of predictors                     */
	z=,                /* Name of the added variable              */
	id=,               /* Name of observation ID variable (char)  */
	loptions=noprint,  /* options for PROC LOGISTIC               */
	smooth=0.5,        /* lowess smoothing parameter              */
	subset=abs(studres)>2,   /* subset of points to label         */
	out=_res_,         /* output data set                         */
	symbol=dot,        /* plotting symbol for points              */
	interp=rl ci=red,  /* interpolation options for points        */
	type=AVP,          /* Type of plot: AVP or CVP                */
	name=addvar,       /* Name of graph in graphic catalog        */
	gout=
	);
	  
	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let type=%upcase(&type);

%let abort=0;
%if %length(&y)=0 | %length(&x)=0
   %then %do;
      %put ERROR: The Y= and X= variables must be specified;
      %let abort=1;
      %goto DONE;
   %end;


*-- Fit the original model, get fit quantities in an ODS;
proc logistic nosimple data=&data &loptions outest=parms;
	%if %length(&trials)=0 %then %do;
	   model &y         = &x;
		%end;
	%else %do;
	   model &y/&trials = &x;
		%end;
   output out=_diag_ pred=p
			difdev=difdev difchisq=difchisq c=c cbar=cbar h=h
			resdev=resdev reschi=reschi;

proc print;
%let zl=&z Residual;
%if &type=CVP %then %do;
	*-- protect against negative values;
	proc univariate data=_diag_ noprint;
		var &z;
		output out=_min_ min=min;
	data _diag_;
		set _diag_;
		if _n_=1 then do;
			set _min_(keep=min);
			set parms(rename=(&z = beta));
			end;
*		_z_ = beta * &z * log(&z);
		_z_ =  &z * log(&z);
	%let zl = Constructed &z*log(&z) Residual;
	%let z = _z_;
	%end; 

data _diag_;
   set _diag_;
   label h = 'Leverage (Hat value)'
			studres = 'Studentized deviance residual';
   format h 3.2;
	studres = resdev / sqrt(1-h);
	drop p n;
	%if %length(&trials)=0 %then %do;
		n=1;
		%end;
	%else %do;
		n=&trials;
		%end;
	yhat = n * p;
	weight = n * p * (1-p);

proc reg data=_diag_ noprint;
	weight weight;
	model &z = &x;
	output out=&out r=zres;

*-- Find slope and intercept in the plot;		
proc reg data=&out outest=_parm_;
*	weight weight;
	model reschi = zres;

data &out;
	set &out;
	zres = zres * sqrt(weight);
	label zres="&zl"
		reschi = "&y Residual";

proc print data=&out;
	%if %length(&id) %then %do;
		id &id;
		%end;
	var &y &trials &x h reschi resdev studres zres;
	format resdev reschi studres zres 6.3;

%label(data=&out, x=zres, y=reschi, text=&id, out=_labels_, pos=-,
	subset=&subset);

*-- Label plot with slope value;
data _parm_;
	set _parm_(keep=zres);
   xsys='1'; ysys='1';
   length text $14 function $8;
   x = 2;   y=4; position='F';
   function = 'LABEL'; color='red';
   text = 'Slope: ' || left(put(zres,7.3));  output;
	%if &type=CVP %then %do;
	power = round(1+zres, 0.5);
	position='C';
   text = 'Power: ' || left(put(power,9.1));  output;
		%end;

data _cross_;
	xsys='2'; ysys='2'; color='red';
	x =0; y=0;  function='move'; output;
	xsys='7';  x= -5; function='draw'; output;
	xsys='7';  x=+10; function='draw'; output;
	xsys='2';  x=  0; function='move'; output;
	ysys='7';  y= -5; function='draw'; output;
	ysys='7';  y=+10; function='draw'; output;

data _labels_;
	length text $14;
	set _labels_ _parm_ _cross_;

%if &smooth>0 %then %do;
	%lowess(data=&out, x=zres, y=reschi, gplot=NO, pplot=NO,
		outanno=_smooth_, silent=YES, f=&smooth, line=20);
	
	data _labels_;
		set _labels_ _smooth_;
	%end;

proc gplot data=&out;
	plot reschi * zres / 
		anno=_labels_ vaxis=axis1 frame vm=1 hm=1
		name="&name" des="Added variable plot for &z in &data";
	symbol1 v=&symbol c=black i=&interp;
	axis1 label=(a=90) offset=(2);
run; quit;
%done:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;

