 /*-------------------------------------------------------------------*
  *   Name: ordplot.sas                                               *
  *  Title: Diagnose form of discrete frequency distribution          *
  *         Poisson, Binomial, Neg. Binomial, Log series              *
       Doc: http://www.datavis.ca/sasmac/ordplot.html              
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:   9 May 1993 11:30:54                                    *
  * Revised:  09 May 2006 13:17:36                                    *
  * Version:  1.3                                                     *
  *  1.1  Plot y * &count so label will not be required               *
  *  1.2  Wtd LS line in red, default LEGCLR='RED'                    *
  *  1.3   Fixed validvarname for V7+                                 *
  *        Fixed buglet with data=_last_                              *
  *        Changed validvarname=V6 to validvarname=upcase             *
  *        'intercep' -> 'intercept'                                  *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/

 /*=
=Description:
 
 The ORDPLOT macro constructs a plot whose slope and intercept can
 diagnose the form of a discrete frequency distribution.  This is
 a plot of k n(k) / n(k-1) against k, where k is the basic count
 and n(k) is the frequency of occurrence of k. The macro displays
 both a weighted and unweighted least squares line and uses the
 slope and intercept of the weighted line to determine the form
 of the distribution. Rough estimates of the parameters of the
 distribution are also computed from the slope and intercept.

=Usage:

 The ORDPLOT macro is called with keyword parameters. The COUNT=
 and FREQ= variables are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	data horskick;
		input deaths corpsyrs;
		label deaths='Number of Deaths'
			corpsyrs='Number of Corps-Years';
	cards;
			0    109
			1     65
			2     22
			3      3
			4      1
	;
	%ordplot(count=Deaths, freq=corpsyrs);
 
==Parameters:

* DATA=       Name of the input data set. [Default: DATA=_LAST_]

* COUNT=      The name of the basic count variable.

* FREQ=       The name of the variable giving the number of occurrences
              of COUNT.

* LABEL=      Label for the horizontal (COUNT=) variable.  If not specified
              the variable label for the COUNT= variable in the input
				  data set is used.

* LEGLOC=     X,Y location for interpretive legend [Default: LEGLOC=3 88]

* LEGCLR=     legend color [Default: LEGCLR=RED]

* OUT=        The name of the output data set. [Default: OUT=ORDPLOT]

* NAME=       Name of the graphics catalog entry. [Default: NAME=ORDPLOT]

 =*/

%macro ordplot(
     data=_last_,       /* input data set                           */
     count=,            /* basic count variable                     */
     freq=,             /* number of occurrences of count           */
     label=,            /* Horizontal (count) label                 */
	  legloc=3 88,       /* x,y location for interpretive legend     */
	  legclr=red,        /* legend color                             */
	  out=ordplot,       /* The name of the output data set          */
     name=ordplot
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

%if %upcase(&data)=_LAST_ %then %let data = &syslast;

%let abort=0;
%if %length(&count)=0 | %length(&freq)=0
   %then %do;
      %put ERROR: The COUNT= and FREQ= variables must be specified;
      %let abort=1;
      %goto DONE;
   %end;

%*if &label=%str() %then %let label=&count;
proc means data=&data noprint;
   var &count;
   weight &freq;
   output out=sum sumwgt=N sum=sum mean=mean min=min max=max;
*proc print data=sum;

data &out;
   set &data;
   if _n_=1 then set sum(drop=_type_ _freq_);
   drop min max mean sum;
   k = &count;
   nk= &freq;                  * n(k);
   nk1 = lag(&freq);           * n(k-1);
   if nk1 > 0 then y =  k * nk / nk1;
   if nk > 1
      then wk = sqrt(nk-1);            * weight for regression line;
      else wk = 0;
   label y='Frequency ratio' 
    nk='n_k'
	nk1='n_{k-1}'
    wk='Weight';
run;

proc print data=&out label;
   id &count;
   var nk nk1 wk y;
   sum nk;
 
proc reg data=&out outest=parms noprint;
   weight wk;
   model y =  k;
%local lx ly;
%let lx=%scan(&legloc,1);
%let ly=%scan(&legloc,2);

%local int;
%if &sysver < 8
	%then %let int=intercep;
	%else %let int=intercept;

data stats;
   set parms (keep=k &int);
   set sum   (keep=mean min max);
   drop k &int;
   length text type parm $30 function $8;
   xsys='1'; ysys='1';
   x=&lx;   y=&ly;
   function = 'LABEL';
*   size = 1.4;
   color = "&legclr";
   position='3'; text ='slope =   '||put(k,f6.3);      output;
   position='6'; text ='intercept='||put(&int,f6.3); output;
 
   *-- Determine type of distribution;
   select;
      when (abs(k) < .1) do;
         type = 'Poisson';
         parm = 'lambda = '||put(&int,6.3);
         end;
      when (k < -.1) do;
         type = 'Binomial';
         p = (k/(k-1));
         parm = 'p = '||put(p,6.3);
         end;
      otherwise do;  * positive slope;
         if &int >-.05 then do;
            type = 'Negative binomial';
            parm = 'p ='||put(1-k,6.3);
            end;
         else do;
            type = 'Logarithmic series';
            parm = 'theta ='||put(k,6.3);
            end;
         end;
      end;
   y = &ly - 7;
   position='3'; text ='type: ' ||type;   output;
   position='6'; text ='parm: ' ||parm;   output;
 
   *-- Draw (weighted) regression line;
   xsys='2'; ysys='2';  size=3; color='red';
   x=min; y = &int + k * x; function='MOVE'; output;
   x=max; y = &int + k * x; function='DRAW'; output;

proc gplot data=&out;
   plot y * &count / anno=stats vaxis=axis1 haxis=axis2 vm=1
      name="&name"
      des="Ord plot of &count";
   symbol v=- h=2 i=rl l=34 c=black;
   axis1 label=(a=90 r=0
          'Frequency Ratio, (k n(k) / n(k-1))' )
         offset=(3,6);
   axis2 offset=(3) minor=none
	%if %length(&label) %then %do;
         label=("k  (&label)")
			%end;
			;
   run;quit;
%done:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;
