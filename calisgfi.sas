 /*--------------------------------------------------------------*
  *    Name: calisgfi.sas                                        *
  *   Title: Produce a readable display of CALIS GFI statistics  *
        Doc: http://www.datavis.ca/sasmac/calisgfi.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 03 Dec 2001 11:23:45                                *
  * Revised: 11 Dec 2001 18:01:21                                *
  * Version: 1.0-1                                               *
  *   Added run; statement at end                                *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 PROC CALIS, like other structural equation model software, produces
 a table of goodness of fit statistics that is hard to read because it
 simply presents a long list of many statistics without any structure.
 The CALISGFI macro takes an OUTRAM= data set from PROC CALIS, extracts
 the goodness-of-fit statistics, and produces a more easily understood
 version.

==Method:

 Very ugly contortions are used to extract and reform the statistics
 from the OUTRAM data set.  The main innovation here is to group and
 relabel the plethora of fit statistics into coherent categories. This
 macro was written before ODS was available.  I'm not sure if ODS would
 make this any easier.


=Usage:

 The CALISGFI macro is defined with keyword parameters.  The RAM=
 parameter is required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%calisgfi(ram=ram1, nv=5);
 
==Parameters:

* RAM=        Name of the OUTRAM= data set from PROC CALIS

* NV=         Number of observed variables in the model.  This is
              needed to calculate the DF for the null model, because
              it doesn't appear in the OUTRAM= data set.

* OUT=        Name of output data set

* VERBOSE=    0 or 1.  Mainly for testing/debugging.
                
==Example:

 The following lines read a covariance matrix of four variables,
 X1, X2, Y1, Y2, and fit a two-factor model, capturing the
 GFI statistics in an OUTRAM= dataset, RAM4, which is input
 to the CALISGFI macro.

	data lord(type=cov);
		input _type_ $ _name_ $  x1 x2 y1 y2;
	cards;
	n   .  649       .       .       .
	cov x1 86.3937   .       .       .
	cov x2 57.7751 86.2632   .       .
	cov y1 56.8651 59.3177 97.2850   .
	cov y2 58.8986 59.6683 73.8201 97.8192
	;
	title "Lord's data: H4- unconstrained two-factor model";
	proc calis data=lord cov noprint outram=ram4;
		lineqs  x1 = beta1 F1  + e1,
					x2 = beta2 F1  + e2,
					y1 = beta3 F2  + e3,
					y2 = beta4 F2  + e4;
		std  F1 F2 = 1,
				e1 e2 e3 e4 = ve1 ve2 ve3 ve4;
		cov  F1 F2 = rho;
	run;
	
	%calisgfi(ram=ram4, nv=4);

 =*/
%macro calisgfi(
	ram=,       /* name of the OUTRAM= data set from PROC CALIS */
	nv=,        /* number of observed variables in the model    */
    out=stat,   /* name of output data set                      */
	verbose=0
	);

	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2 o3;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		%let o3 = %sysfunc(getoption(linesize,keyword));
		options nonotes validvarname=upcase ls=85;
		%end;
	%else %do;
	   options nonotes ls=85;
		%end;


*-- Formats for statistics types;
proc format;
	value $typ
		'a1' = 'Absolute Indices'
		'ap' = 'Absolute, Adjusted for Parsimony'
		'a0' = 'Model Information '
		'i1' = 'Incremental, Type 1'
		'i2' = 'Incremental, Type 2'
		'i3' = 'Incremental, Type 3'
		'i'  = 'Incremental Indices (vs. Null Model)';

*-- Create a dataset of labels and other information on the
	gfi statistics.  This is very ugly, but one cannot create
	a dataset with CARDS or DATALINES within a macro;

data stats;
 length _name_ $8 label $45 ref $30;
 _name_='N'; label='Sample size';
  type='a0'; ref=' ';  obs= 1; output;
 _name_='FIT'; label='Fit Function';
  type='a1'; ref=' ';  obs= 2; output;
 _name_='GFI'; label='Goodness of Fit Index';
  type='a1'; ref=' ';  obs= 3; output;
 _name_='AGFI'; label='GFI Adjusted for Degrees of Freedom';
  type='ap'; ref=' ';  obs= 4; output;
 _name_='RMR'; label='Root Mean Square Residual';
  type='a1'; ref=' ';  obs= 5; output;
 _name_='PGFI'; label='Parsimonious GFI';
  type='ap'; ref='Mulaik, 1989';  obs= 6; output;
 _name_='NPARM'; label='Number of parameters';
  type='a0'; ref=' ';  obs= 7; output;
 _name_='DF'; label='Chi-Square DF';
  type='a1'; ref=' ';  obs= 8; output;
 _name_='N_ACT'; label='Number of active constraints';
  type='a0'; ref=' ';  obs= 9; output;
 _name_='CHISQUAR'; label='Chi-Square';
  type='a1'; ref=' ';  obs=10; output;
 _name_='P_CHISQ'; label='Pr > Chi-Square';
  type='a1'; ref=' ';  obs=11; output;
 _name_='CHISQNUL'; label='Null Model Chi-Square';
  type='a0'; ref=' ';  obs=12; output;
 _name_=' '; label='Null Model Chi-Square DF';
  type='a0'; ref=' ';  obs=13; output;
 _name_='RMSEAEST'; label='RMS Error of Approx Estimate (RMSEA)';
  type='ap'; ref='Steigler & Lind, 1980';  obs=14; output;
 _name_='RMSEALOB'; label='RMSEA 90% Lower Confidence Limit';
  type='ap'; ref='Browne & Du Toit 1992';  obs=15; output;
 _name_='RMSEAUPB'; label='RMSEA 90% Upper Confidence Limit';
  type='ap'; ref=' ';  obs=16; output;
 _name_='P_CLOSFT'; label='Probability of Close Fit';
  type='ap'; ref='Browne & Cudeck, 1993';  obs=17; output;
 _name_='ECVI_EST'; label='Expected Cross Validation Index (ECVI)';
  type='ap'; ref=' ';  obs=18; output;
 _name_='ECVI_LOB'; label='ECVI 90% Lower Confidence Limit';
  type='ap'; ref=' ';  obs=19; output;
 _name_='ECVI_UPB'; label='ECVI 90% Upper Confidence Limit';
  type='ap'; ref=' ';  obs=20; output;
 _name_='COMPFITI'; label='Comparative Fit Index';
  type='i'; ref='Bentler, 1989';  obs=21; output;
 _name_='ADJCHISQ'; label='Adjusted Chi-square';
  type='a1'; ref='Browne, 1982';  obs=22; output;
 _name_='P_ACHISQ'; label='Pr > Adjusted Chi-square';
  type='a1'; ref=' ';  obs=23; output;
 _name_='RLSCHISQ'; label='Normal Theory Reweighted LS Chi-Square';
  type='a1'; ref=' ';  obs=24; output;
 _name_='AIC'; label="Akaike's Information Criterion";
  type='ap'; ref='Akaike, 1987';  obs=25; output;
 _name_='CAIC'; label='Consistent AIC';
  type='ap'; ref='Bozdogan, 1987';  obs=26; output;
 _name_='SBC'; label="Schwarz's Bayesian Criterion";
  type='ap'; ref='Schwarz, 1978';  obs=27; output;
 _name_='CENTRALI'; label='Centrality Index';
  type='ap'; ref='McDonald, 1989';  obs=28; output;
 _name_='BB_NONOR'; label='Non-normed Index (Rho)';
  type='i'; ref='Bentler & Bonett, 1980';  obs=29; output;
 _name_='BB_NORMD'; label='Normed Fit Index (NFI)';
  type='i'; ref='Bentler & Bonett, 1980';  obs=30; output;
 _name_='PARSIMON'; label='Parsimonious NFI';
  type='i'; ref='James, Mulaik, & Brett, 1982';  obs=31; output;
 _name_='ZTESTWH'; label='Z-Test for Chi-Square';
  type='a1'; ref='Wilson & Hilferty, 1931';  obs=32; output;
 _name_='BOL_RHO1'; label='Normed Index Rho1';
  type='i'; ref='Bollen, 1986';  obs=33; output;
 _name_='BOL_DEL2'; label='Non-normed Index Delta2';
  type='i'; ref='Bollen, 1988';  obs=34; output;
 _name_='CNHOELT'; label='Critical N';
  type='a1'; ref='Hoelter, 1983';  obs=35; output;

proc sort data=stats;
	by _name_;

data _ram_;
	set &ram;
	keep _name_ _estim_;
	where (_type_='STAT');
*proc print;
proc sort data=_ram_;
	by _name_;
	
data stats;
	merge _ram_(in=inram)
		stats(in=instat);
	by _name_;

	*-- The OUTRAM= data set does not contain the DF for the null model,
	    but this appears in the printed output. Fix it here.;
	%if %length(&nv) %then %do;
		if _name_ = ' ' then do;
			_name_ = 'DFNULL';
			_estim_ = &nv*(&nv-1)/2;
			end;
		%end;


proc sort data=stats;
	by type obs;

%if &verbose>0 %then %do;
	proc print data=stats;
		id _name_;
		by type;
		var label ref _estim_;
		format type $typ.;
	%end;

*-- Transform the stats data set to one with just main measures,
    and supplementary info as other variables.
	 CALIS should really provide an OUTSTAT= data set instead, in
	 this format;
	 
data &out;
	set stats;
	retain df chisquar p_chisq adjchisq p_achisq chisqnul lab;
	retain RMSEAEST RMSEALOB RMSEAUPB ECVI_EST ECVI_LOB ECVI_UPB;
	drop obs lab;
	drop chisquar chisqnul adjchisq RMSEAEST ECVI_EST;
	select (_name_);
		when('CHISQNUL')  do;
			chisqnul=_estim_;
			lab = label;
			end;
		when('DFNULL')  do;
			df=_estim_;
			_estim_=chisqnul;
			pr = 1 - probchi(_estim_, df);
			_name_ = 'CHISQNUL';
			label = lab;
			output;
			end;
		when('DF')        df=_estim_;
		when('CHISQUAR') do;
			chisquar=_estim_;
			lab=label;
			end;
		when('P_CHISQ')   do;
			pr = _estim_;
			_estim_ = chisquar;
			_name_ = 'CHISQUAR';
			label = lab;
			output;
			end;
		when('ADJCHISQ') do;
			adjchisq=_estim_;
			lab=label;
			end;
		when('P_ACHISQ')   do;
			pr = _estim_;
			if pr^=. and adjchisq^=. then do;
				_estim_ = adjchisq;
				_name_ = 'ADJCHISQ';
				label = lab;
				output;
				end;
			end;
		when('RLSCHISQ') do;
			if _estim_ ^=. then do;
				pr = 1 - probchi(_estim_, df);
				end;
			output;
			end;
		when('ZTESTWH') do;
			pr = 1 - probnorm(_estim_);
			output;
			end;
		when('RMSEAEST') do;
			RMSEAEST=_estim_;
			lab=label;
			end;
		when('RMSEALOB') cilo=_estim_;
		when('RMSEAUPB') do;
			if RMSEAEST ^=. then do;
				cihi=_estim_;
				label = lab;
				_name_ = 'RMSEAEST';
				_estim_= RMSEAEST;
				output;
				end;
			end;
		when('ECVI_EST') do;
			ECVI_EST=_estim_;
			lab=label;
			end;
		when('ECVI_LOB') cilo=_estim_;
		when('ECVI_UPB') do;
			if ECVI_EST ^=. then do;
				cihi=_estim_;
				label = lab;
				_name_ = 'ECVI_EST';
				_estim_= ECVI_EST;
				output;
				end;
			end;
		when ('P_CLOSFT')
			/* should put this into the space after the estimate,
			   rather than in the label
			*/
			if _estim_ ^=. then do;
				label = trim(label) || ' [Pr(RMSEA <= 0.05)]';
				output;
				end;
			
		otherwise do;
			df =.;
			if _estim_ ^=. then output;
			end;
		end;

%if &verbose %then %do;
proc print n;
	id _name_;
	var label _estim_ df pr cilo cihi;
	%end;


data _null_;
	set &out;	
	by type;
	head=15;
	line=1; value=47;
	file print;
	if first.type then put / @head  type $typ. /;
	_name_ = upcase(_name_);
	if _name_ not in ('FIT') then do;
		if _estim_ > 1 
			then _estim_ = round(_estim_, .01);
			else _estim_ = round(_estim_, .001);
		end;

	if pr ^=. then do;
			put @line label +0 '(df=' +0 df +(-1) ')' @value _estim_ 8.3 
			+2 '(Pr>ChiSq=' +0 pr 6.3 ')';
		end;
	else if cihi ^=. then do;
			put @line label  @value _estim_ 8.3 
			+2 '90% CI: (' +0 cilo 5.3 +(-1) ',' +0 cihi 5.3 ')';
		end;
	else put @line label  @value _estim_ best8.;
run;

	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2 &o3;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;
