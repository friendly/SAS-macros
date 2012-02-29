 /*--------------------------------------------------------------*
  *    Name: csmpower.sas                                        *
  *   Title: Power estimation for Covariance Structure Models    *
        Doc: http://www.datavis.ca/sasmac/csmpower.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  31 Oct 1997 08:15:57                               *
  * Revised:  22 Jun 2000 16:17:00                               *
  * Version: 1.1                                                 *
  *  1.1  updated for V8 (validvarname)                          *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
csmpower carries out retrospective or prospective power computations
for covariance structure models using the method of MacCallum,
Browne and Sugawara (1996).  Their approach allows for testing a
null hypothesis of 'not-good-fit', so that a significant result
provides support for good fit.

Effect size in this approach is defined in terms of a null hypothesis
and alternative hypothesis value of the root-mean-square error of
approximation (RMSEA) index.  These values, together with the degrees
of freedom (df) for the model being fitted, the sample size (n), and
error rate (alpha), allow power to be calculated.

The values of RMSEA are printed by PROC CALIS as "RMSEA Estimate"
among the many fit statistics.  The statistic also appears in the
OUTRAM= data set. Values of RMSEA <= .05 are typically considered
'close fit'; values .05-.08 are considered 'fair', .08-.10, 'mediocre',
RMSEA > .10, 'poor'.

For a retrospective power analysis, the macro reads an OUTRAM= data set
from a PROC CALIS run, and calculates power for the values of RMSEAEST
and its lower and upper confidence limits.  For a prospective power analysis,
values of RMSEA, DF and N must be provided through the macro arguments.
The macro allows several values of rmseaa, alpha,  df and sample size to be 
specified.  Power is calculated for each combination of these values.

=Usage:

The cspower macro should be called specifying either DATA=the name of
an OUTRAM= dataset obtained from PROC CALIS, or sets of values for
the RMSEAA, DF and N parameters.  If the DATA= parameter is supplied,
the macro ignores values of the RMSEAA, DF and N parameters.

	%csmpower(data=outram_dataset, alpha=.05);

	%csmpower(n=%str(200, 300, 400), df=23);

==Parameters:

DATA=
RMSEA0 = .05
RMSEAA = .10
ALPHA  = .05
DF     = 
N      = %str(40 to 100 by 20, 150 to 400 by 50)
PLOT   = %str(power * n = df)
OUT    = csmpower

==Reference:

MacCallum, R., Browne, M., and Sugawara, H. M. (1996).
	Power Analysis dnd Determination of Sample Size for Covariance
	Structure Modeling, Psychological Methods, 2(1), 130-149.
	http://quantrm2.psy.ohio-state.edu/maccallum/power.htm

 =*/

%macro csmpower(
    data=,
    id=,
	rmsea0 = .05,
	rmseaa = .10,
	alpha  = .05,
	df     = ,
	n      = %str(40 to 100 by 20, 150 to 400 by 50),
	sortby =,
	plot   = %str(power * n = df),
	out    = csmpower
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

%let id=;
%if %length(&data) %then %do;
      data _parms_;
        set &data;
        where ((_type_='STAT') &
               (_name_ in ('DF', 'N', 'RMSEAEST', 'RMSEALOB', 'RMSEAUPB')));
        keep _name_ _estim_;

    data _parms_;
       set _parms_;
       retain N DF;
       select;
          when (_name_='N')       N =_estim_;
          when (_name_='DF')      DF=_estim_;
          when (_name_=: 'RMSEA') do;
             RMSEAA = _estim_;
             output;
             end;
          otherwise;
          end;
    %let id=_name_;
    proc print; 
    title2 "RMSEA values from data set &data";
       run;
    %end;

%*-- Construct data set of parameter values;
data _parms_;
	label rmsea0 = 'Null hypothesis fit value'
	      rmseaa = 'Alt hypothesis fit value'
			df = 'Model degrees of freedom'
			n  = 'Sample size'
			alpha = 'Alpha';

	rmsea0= &rmsea0;
	
	do alpha = &alpha;
    %if %length(&data) %then %do;
        set _parms_;
        output;
        %end;
    %else %do;
		do df = &df;
			do n= &n;
				do rmseaa = &rmseaa;
					output;
				end;
			end;
		end;
    %end;
	end;
	
data &out;
	set _parms_ end=last;
	label	power = 'Power'
	      ncp0  = 'Null Non-centrality'
	      ncpa  = 'Alt Non-centrality';
    ncp0=(n-1)*df*rmsea0**2 ;
    ncpa=(n-1)*df*rmseaa**2 ;
    if rmsea0<rmseaa then do ;
			cval=cinv(1-alpha,df,ncp0) ;
			power=1-probchi(cval,df,ncpa) ;
			end ;
    else do ;
			cval=cinv(alpha,df,ncp0) ;
			power=probchi(cval,df,ncpa) ;
			end ;
    if last then call symput('npow', put(_n_,4.));
	run ;
%put &npow power values recorded in data set &out;

proc sort;
   by alpha df n;
proc print data=csmpower label;
	id alpha df &id;
	by alpha df;
	var n rmsea0 rmseaa power;
    title2 'Power calculations for covariance structure models';


%if %length(&plot)>0 and &npow>1 %then %do;
proc plot;
   plot &plot;
	run;
run;
%end;

%done:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
title2;
%mend;

