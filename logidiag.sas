%macro logidiag(
	data=_last_,    /* Name of input data set                  */
	y=,             /* Name of criterion variable              */
	trials=,        /* Name of trials variable                 */
	x=,             /* Names of predictors                     */
	id=,            /* Name of observation ID variable (char)  */
	loptions=noprint,/* options for PROC LOGISTIC              */
	addvar=,
	smooth=0.5,
	subset=abs(studres)>2,
	out=_res_,      /* output data set */
	symbol=dot,
	interp=rl ci=red,
	name=addvar,
	gout=
	);
	  

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
	model &addvar = &x;
	output out=&out r=zres;

*-- Find slope and intercept in the plot;		
proc reg data=&out outest=_parm_;
	model reschi = zres;

data &out;
	set &out;
	zres = zres * sqrt(weight);
	label zres="&addvar Residual"
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
   x = 98;   y=3; position='4';
   function = 'LABEL'; color='red';
   text = 'Slope: ' || left(put(zres,7.3));  output;

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
	%lowess(data=&out, x=zres, y=reschi, gplot=NO, pplot=NO, outanno=_smooth_,
		silent=YES, f=&smooth, line=20);
	
	data _labels_;
		set _labels_ _smooth_;
	%end;

proc gplot data=&out;
	plot reschi * zres / 
		anno=_labels_ vaxis=axis1 frame vm=1 hm=1
		name="&name" des="Added variable plot for &addvar in &data";
	symbol1 v=&symbol c=black i=&interp;
	axis1 label=(a=90) offset=(2);
run; quit;
%mend;

