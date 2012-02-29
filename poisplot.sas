/*-------------------------------------------------------------------*
 *    Name: poisplot.sas                                             *
 *   Title: Poissonness plot for discrete distributions              *
       Doc: http://www.datavis.ca/sasmac/poisplot.html            
 *     Ref:                                                          *
 *  Hoaglin & Tukey, Checking the shape of discrete distributions.   *
 *   In Hoaglin, Mosteller & Tukey (Eds.), Exploring data tables,    *
 *   trends, and shapes, NY: Wiley, 1985.                            *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  19 Mar 1991 08:48:26                                    *
 * Revised:  02 May 2007 15:03:54                                    *
 * Version:  1.4-1                                                     *
 *  1.1  Plot y * &count so label will not be required               *
 *       Allow for 0 frequencies                                     *
 *  1.2  Added indicated parameter change plots                      *
 *  1.3  Fixed validvarname for V7+                                  *
 *       Fixed %gskip problem for eps output                         *
 *       Changed validvarname=V6 to validvarname=upcase              *
 *  1.4  Fixed V9 change to INTERCEPT                                *
 *       Fixed careless buglet related to above                      *
 *                                                                   *
 *  From ``Visualizing Categorical Data'', Michael Friendly (2000)   *         
 *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The POISPLOT macro constructs a ``Poissonness plot'' for determining
 if discrete data follows the Poisson distribution.  The plot has a
 linear relation between the count metameter n(k) and the basic
 count, k, when the distribution is Poisson.  An influence
 plot displays the effect of each observed frequency on the choice of
 the Poisson parameter, lambda.
 
=Usage:

 The POISPLOT macro is called with keyword parameters.  The COUNT=
 and FREQ= parameters are required.  
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
	%poisplot(count=Deaths,freq=corpsyrs, plot=dist);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* COUNT=      The name of the basic count variable

* FREQ=       The name of the variable giving the number of occurrences
              of COUNT

* LABEL=      Label for the horizontal (COUNT=) variable.  If not specified
              the variable label for the COUNT= variable in the input
				  data set is used.

* LAMBDA=     Trial value  of the Poisson parameter lambda to level the plot.
              If LAMBDA=0 (the default) the plot is not levelled.

* Z=          Multiplier for error bars [Default: Z=1.96]

* PLOT=       What to plot: DIST and/or INFL [Default: PLOT=DIST INFL]

* HTEXT=      Height of text labels [Default: HTEXT=1.4]

* OUT=        The name of the output data set [Default: OUT=POISPLOT]

* NAME=       Name of the graphics catalog entry [Default: NAME=POISPLT]
                

 =*/
 
%macro poisplot(
     data=_last_,
     count=,            /* basic count variable                     */
     freq=,             /* number of occurrences of count           */
     label=,            /* Horizontal (count) label                 */
     lambda=0,          /* trial value of lambda to level the plot  */
     z=1.96,            /* multiplier for error bars                */
	  plot=DIST INFL,    /* What to plot: DIST and/or INFL           */
	  htext=1.4,
	  out=poisplot,
	  name=poisplt
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
	%*-- Handle V9 changes;
	%if %sysevalf(&sysver  >= 9) %then %do;
		%let intercep = intercept;
		%end;
	%else %do;
		%let intercep = intercep;
		%end;

%let abort=0;
%if %length(&count)=0 | %length(&freq)=0
   %then %do;
      %put ERROR: The COUNT= and FREQ= variables must be specified;
      %let abort=1;
      %goto DONE;
   %end;

%*if &label=%str() %then %let label=&count;
%let plot=%upcase(&plot);
%if %length(&z)=0 %then %let z=0;

proc means data=&data N sum sumwgt mean var noprint;
   var &count;
   weight &freq;
   output out=sum sumwgt=N sum=sum mean=mean;

data &out;
   set &data;
   if _n_=1 then set sum(drop=_type_ _freq_);
	drop kf k;
   k = &count;
   nk= &freq;                     * n(k);
   kf= gamma(k+1);                * k!  ;
 
   *-- if levelling the plot, subtract centering value;
   %if &lambda = 0
       %then %str( level =  0; );
       %else %str( level = &lambda - k * log( &lambda ); );
 
   y =  log(kf * nk / N) + level ;    * poisson metameter;
 
   *-- centered value n*(k), Hoaglin & Tukey, Eqn 9 ;
   p = nk / N;
   if nk >=2 then nkc = nk - .67 - .8*p;
             else nkc = exp(-1);
*	if nk > 0 then;
	   yc=  log(kf * nkc/ N) + level ;
 
   *-- half-length of confidence interval for log(eta-k) [Eqn 10];
	if nk<=1 then do;
		ylo = log(kf*nkc/N) - 2.677 + level;
		yhi = log(kf*nkc/N) + 2.717 - 2.3/N + level;
		h = (yhi-ylo)/2;
		end;
	else do;
		h = &z * sqrt( (1-p) / ( nk-((.47+.25*p)*sqrt(nk)) ) );
		ylo = yc - h;
		yhi = yc + h;
		end;

	*-- Estimated prob and expected frequency;
   phat = exp(-mean) * mean**&count / kf;
   exp  = N * phat;

	*-- Leverage and apparent parameter values (p.402);
   %if &lambda = 0
		%then %str(lev = (&count / mean) - 1;);
		%else %str(lev = (&count / &lambda) - 1;);
	hc = sign(lev) * lev / h;
	vc = sign(lev) * (log(nkc) - log(exp)) / h;
	slope = vc / hc;   *-- (lambda - lambda0);
	label y = 'Count metameter'
		hc = 'Scaled Leverage'
		vc = 'Relative metameter change'
		slope = 'Parameter change';

proc print data=&out;
   id &count;
   var nk y nkc yc h ylo yhi lev hc vc;
   sum nk nkc;
   format y yc h yhi ylo 6.3 lev hc vc 6.2;
 
*-- Calculate goodness of fit chisquare;
data fit;
   set &out;
   chisq= (nk - exp)**2 / exp;
proc print data=fit;
   id &count;
   var nk p phat exp chisq;
   sum nk exp chisq;


%if %index(&plot,DIST) %then %do;
*-- Find slope, intercept of line;
proc reg data=&out outest=parms noprint;
   model y =  &count;
data stats;        *-- Annotate data set to label the plot;
   set parms (keep=&count &intercep);
   set sum   (keep=mean);
   drop &count &intercep;
   length text $30 function $8;
   xsys='1'; ysys='1';
   x=15;
	*-- set label y location based on slope;
	if &count > 0
		then y=96;
		else y=16; 
   function = 'LABEL';
   size = &htext;
   color = 'RED';
   position='3'; text ='slope =   '||put(&count,f6.3);      output;
   position='6'; text ='intercept='||put(&intercep,f6.3); output;
   ek = exp(&count - &lambda);
   y=y-6;;
   position='6'; text ='lambda:  mean = '||put(mean,5.3); output;
   position='9'; text ='       exp(slope) = '||put(ek,5.3); output;

%let order=;
%if &z > 0 %then %do;
data conf;
   set &out;
   drop yc;
   xsys='2'; ysys='2';
   x = &count;    line=33;
   y = yc;   function='MOVE  '; output;
   text='+'; function='SYMBOL'; output;
   y = yhi;  function='DRAW  '; output;
   y = yc;   function='MOVE  '; output;
   y = ylo;  function='DRAW  '; output;
data stats;
   set stats conf;

*-- find range of confidence limits to set y axis extrema;
proc means data=conf noprint;
   var y;
   output out=range min=min max=max;
data _null_;
   set range;
	inc = 1;
	if (max-min)>10 then inc=2;
   min = inc * floor(min/inc);
   max = inc * ceil(max/inc);
   call symput('MIN', left(put(min,2.)));
   call symput('MAX', left(put(max,2.)));
   call symput('INC', left(put(inc,2.)));
run;
%let order = order=(&min to &max by &inc);
%end; /* %if &z */

*-- Poissonness plot;
proc gplot data=&out;
   plot y * &count / anno=stats vaxis=axis1 haxis=axis2
		name="&name"
		des="Poissonness plot of &count";
   symbol v=dot h=2 i=rl c=black;
   axis1 &order
         label=(a=90 r=0 h=1.4
          'Poisson metameter, ln(k!  n(k) / N)')
         value=(h=&htext);
   axis2 offset=(3) minor=none
	%if %length(&label) %then %do;
         label=("k  (&label)")
			%end;
         value=(h=&htext);
	run; quit;
%end; /*%if %index(&plot, DIST) */

*-- Indicated parameter change (infl) plot;
%if %index(&plot,INFL) %then %do;
	%label(data=&out, y=vc,    x=hc, text=&count, out=anno1, size=);
	*-- Draw lines from origin to each point;
	data lines;
		set &out(keep=hc vc);
		xsys='2'; ysys ='2';
		x=0;  y=0;  function='move    '; output;
		x=hc; y=vc; function='draw    '; output;
	data anno1;
		set anno1 lines;

	%label(data=&out, y=slope, x=hc, text=left(put(&count,2.)),
		out=anno2, size=);
	%if %index(&plot,DIST) %then %do;
	%gskip;
	%end;
	symbol v=- h=2 color=black i=none;
	axis1 label=(a=90);

	proc gplot data=&out;
		plot vc * hc  /
			hzero vref=0 lvref=33 anno=anno1 vaxis=axis1 hminor=1 vminor=1
			name="&name"
			des="Parameter change plot of &count";
		run;
	%gskip;
	proc gplot data=&out;
		bubble slope * hc =hc / 
			vref=0 lvref=33 anno=anno2 vaxis=axis1  hminor=1 vminor=1
			bsize=40 bcolor=red bscale=radius
			name="&name"
			des="Parameter change plot of &count";

	run; quit;
%end;
%done:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;
