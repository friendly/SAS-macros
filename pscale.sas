 /*-------------------------------------------------------------------*
  *    Name: pscale.sas                                               *
  *   Title: Construct annotations for a probability scale            *
        Doc: http://www.datavis.ca/sasmac/pscale.html              
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  2 Nov 1995 12:07:41                                     *
  * Revised: 19 Mar 2003 14:26:39                                     *
  * Version: 1.1                                                      *
  *  1.1  Added BYVAR= and BYVAL= for multiple plots                  *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The PSCALE macro constructs an annotate data set to draw an unequally-
 spaced scale of probability values on the vertical axis of a plot
 (at either the left or right).  The probabilities are assumed to 
 correspond to equally-spaced values on a scale corresponding to
 Normal quantiles (using the probit transformation) or Logistic
 quantiles (using the logit transformation).

=Usage:

 The PSCALE macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%pscale(out=pscale);
	proc gplot;
		plot logit * X / anno=pscale;
 
==Parameters:

* ANNO=       Name of annotate data set [Default: ANNO=PSCALE]

* OUT=        Synonym for ANNO=

* SCALE=      Linear scale: logit or probit [Default: SCALE=LOGIT]

* LO=         Low scale value [Default: LO=-3]

* HI=         High scale value [Default: HI=3]

* PROB=       List of probability values to be displayed on the axis,
              in the form of a list acceptable in a DO statement.
              [Default: PROB=%str(.05, .1 to .9 by .1, .95)]

* AT=         X-axis percent for the axis.  AT=100 plots the axis at
              the right; AT=0 plots the axis at the left. [Default: AT=100]

* TICKLEN=    Length of tick marks [Default: TICKLEN=1.3]

* SIZE=       Size of value labels

* FONT=       Font for value labels
                
 =*/
 
%macro pscale(
	anno=pscale,      /* name of annotate data set     */
	out=,             /* synonym for anno=             */
	scale=logit,      /* linear scale: logit or probit */
	lo=-3,            /* low scale value               */
	hi=3,             /* high scale value              */
	prob=%str(.05, .1 to .9 by .1, .95),
	at=100,           /* x-axis percent for the axis   */
	ticklen=1.3,      /* length of tick marks          */
	size=,            /* size of value labels          */
	font=,            /* font for value labels         */
	byvar=,
	byval=
	);

%let scale=%upcase(&scale);
%if %length(&out) %then %let anno=&out;

data &anno;
   xsys = '1';            * percent values for x;
   ysys = '2';            * data values for y;
   length text $4 function $8;
	%if %length(&font) %then %do;
	   style="&font";
		%end;
	%if %length(&size) %then %do;
	   size=&size;
		%end;
	drop prob scale;
	loc = &at;

   %if %length(&byvar) %then %do;
   do &byvar=&byval;
   %end;
   do prob = &prob ;
		%if &scale=LOGIT %then %do;
      	scale = log( prob / (1-prob) );   * convert to logit;
			%end;
		%else %if &scale=PROBIT %then %do;
      	scale = probit( prob );           * convert to normal quantile;
			%end;
		%else %do;
			scale = &scale;
			%end;

      if (&lo <= scale <= &hi) then do;
         y = scale;
         x = &at + sign(50-&at)*&ticklen; function='MOVE';
				output;   * tick marks;
         x = &at;  function='DRAW ';
				output;
         text = put(prob,3.2);
			position='6';  * values;
         function='LABEL  '; output;
         end;
      end;  /* do prob= */
   %if %length(&byvar) %then %do;
	end;
   %end;
	run;
%mend;
