 /*-----------------------------------------------------------------*
  |     Name: equate.sas                                            |
  |    Title: Creates AXIS statements for a GPLOT with equated axes |
         Doc: http://www.datavis.ca/sasmac/equate.html           
  | ----------------------------------------------------------------|
  |    Procs: means gplot                                           |
  |  Macdefs: equate                                                |
  | ----------------------------------------------------------------|
  | Original: Warren Kuhfeld (SAS Sample Library)                   |
  |      Ref: P-179, PROC CORRESP, EXAMPLE 3.                       |
  |   Author: Michael Friendly               <friendly@yorku.ca>    |
  |  Created:  28 Jul 1998 14:13:25                                 |
  |  Revised:  03 Nov 2006 10:32:27                                 |
  |  Version:  1.2                                                  |
  |  1.1 Generates AXIS stmts for use by other macros               |
  |    - Determines XMAX, YMAX from DGSI if not specified           |
  |    - Optionally plots the data (if PLOT=YES)                    |
  |  1.2 Added XINC= and YINC= calculation from data                |
  |    - added XOPTS=, YOPTS= to pass other options to AXIS stmts   |
  |                                                                 |
  | From ``Visualizing Categorical Data'', Michael Friendly (2000)  |         
  *-----------------------------------------------------------------*/
 /*=
=Description:

 The EQUATE macro creates AXIS statements for a GPLOT with equated
 axes, and optionally produces a plot using point labels (supplied
 in an input annotate data set).  It is a modified version of the
 macro appearing in the SAS Sample Library.
                                                            
 It creates an AXIS statement for the vertical variable Y and
 an AXIS statement for horizontal variable X such that an inch
 on the vertical axis represents the same data range as an inch on
 the horizontal axis.  Equated axes are necessary whenever
 distances between points, or angles between vectors from the origin
 are to be interpreted.
	  
=Usage:

 The EQUATE macro takes 15 keyword arguments.  The X= and Y=
 parameters are required.  

 You may wish to reset the defaults below to be more
 suited to your devices.  As well, use GOPTIONS HSIZE= VSIZE=; to
 allow the maximum plot size if you  specify the XMAX= and
 YMAX= parameters as null values.

 As an additional convenience (particularly for use within other
 macros) EQUATE will calculate reasonable tick mark increments
 from the data, to give about 6 tick marks on an axis, if the
 XINC= or YINC= parameters are specified as null values.

==Parameters:

* DATA=       Name of the input data set [Default: DATA=_LAST_]

* ANNO=       Name of an Annotate data set (used only if PLOT=YES).
              [Default: ANNO=&DATA]

* X=          Name of the X variable [Default: X=X]

* Y=          Name of the Y variable [Default: Y=Y]

* XMAX=       Maximum X axis length (inches).  If XMAX= (a null value)
              the macro queries the device driver (using the DSGI)
				  to determine the maximum axis length. [Default: XMAX=6.5]

* YMAX=       Maximum Y axis length (inches).  If YMAX= (a null value)
              the macro queries the device driver (using the DSGI)
				  to determine the maximum axis length. [Default: YMAX=8.5]

* XINC=       X axis tick increment.  If XINC= (a null value),
              the macro calculates an increment from the data
				  which is 1, 2, 2.5, 4, or 5 times a power of 10
				  so that about 6 tick marks will appear on the X
				  axis. [Default: XINC=0.1]

* YINC=       Y axis tick increment.  If XINC= (a null value),
              the macro calculates an increment from the data
				  which is 1, 2, 2.5, 4, or 5 times a power of 10
				  so that about 6 tick marks will appear on the X
				  axis. [Default: YINC=0.1]

* XPEXTRA=    Number of extra X axis tick marks at the high end.
              Use the XPEXTRA= and XMEXTRA= parameters to extend the
              range of the X variable beyond the data values, e.g.,
				  to accommodate labels for points in a plot.
				  [Default: XPEXTRA=0]

* XMEXTRA=    Number of extra X axis tick marks at the low end.
				  [Default: XMEXTRA=0]

* YPEXTRA=    Number of extra Y axis tick marks at the high end.
              Use the YPEYTRA= and YMEYTRA= parameters to extend the
              range of the Y variable beyond the data values, e.g.,
				  to accommodate additional annotations in a plot.
				  [Default: YPEXTRA=0]

* YMEXTRA=    Number of extra Y axis tick marks at the low end.
				  [Default: XMEXTRA=0]

* VAXIS=      Name of the AXIS statement for Y axis [Default: VAXIS=AXIS98]

* HAXIS=      Name of the AXIS statement for X axis [Default: HAXIS=AXIS99]

* PLOT=       Draw the plot? [Default: PLOT=NO]
                

    This macro performs no error checking.                  

 =*/
                                                            
   /*---------------------------------------------------------*/
%macro equate(
			data=_last_,  /* Name of input data set             */
			anno=&data,   /* Name of Annotate data set          */
			x=x,          /* Name of X variable                 */
			y=y,          /* Name of Y variable                 */
			xmax=6.5,     /* maximum x axis inches              */
			ymax=8.5,     /* maximum y axis inches              */
			xinc=0.1,     /* x axis tick increment              */
			yinc=0.1,     /* y axis tick increment              */
			xpextra=0,    /* include extra + end x axis ticks   */
			xmextra=0,    /* include extra - end x axis ticks   */
			ypextra=0,    /* include extra + end y axis ticks   */
			ymextra=0,    /* include extra - end y axis ticks   */
			vaxis=axis98, /* AXIS statement for Y axis          */
			haxis=axis99, /* AXIS statement for X axis          */
			xopts=,       /* additional options for X axis      */
			yopts=,       /* additional options for Y axis      */
			plot=NO       /* Draw the plot?                     */
				  );
 
 
   %if %upcase(&data)=_LAST_ %then %let data=&syslast;
	
   *---Find the Minima and Maxima---;
	options nonotes;
   proc means noprint data=&data;
      var &y &x;
      output out=__temp__ min=ymin xmin max=ymax xmax;
      run;
 
   data _null_;
      set __temp__;
 
      *-- Select increments if values are empty --;
		%if %length(&xinc)=0 %then %do;
			min=xmin;  max=xmax; link doinc; xinc=inc;
         %end;
		%else %do;
	      xinc = &xinc;
			%end;
		
		%if %length(&yinc)=0 %then %do;
			min=ymin;  max=ymax; link doinc; yinc=inc;
         %end;
		%else %do;
	      yinc = &yinc;
			%end;
		
      *---Scale Minima and Maxima to Multiples of the Increments---;
      yinc = &yinc;
      ymin = (floor(ymin / yinc) - (&ymextra)) * yinc;
      xmin = (floor(xmin / xinc) - (&xmextra)) * xinc;
      ymax = (ceil (ymax / yinc) + (&ypextra)) * yinc;
      xmax = (ceil (xmax / xinc) + (&xpextra)) * xinc;
 
		*-- Should check that # tics is reasonable;
		xtic = (xmax - xmin) / xinc;
		ytic = (ymax - ymin) / yinc;
		
		*-- Determine XMAX, YMAX if not specified;
		%if %length(&xmax)=0 or %length(&ymax)=0 %then %do;
			rc=ginit();
			call gask('maxdisp',units,_xmax_,_ymax_,xpix,ypix,rc2);
			rc3 = gterm();
			*-- Convert to inches;
			_xmax_ = _xmax_ * 100 / 2.54;
			_ymax_ = _ymax_ * 100 / 2.54;
			%end;
		%else %do;
			_xmax_ = &xmax;
			_ymax_ = &ymax;
			%end;
			
      *---Compute the Axis Lengths---;
 
      ytox = (ymax - ymin) / (xmax - xmin);
      if ytox le ((_ymax_) / (_xmax_)) then do;
         xlen =  _xmax_;
         ylen = (_xmax_) * ytox;
         end;
      else do;
         ylen = _ymax_;
         xlen = (_ymax_) / ytox;
         end;
 
      *---Write Results to Symbolic Variables---;
 
      call symput('len1',compress(put(ylen, best6.)));
      call symput('len2',compress(put(xlen, best6.)));
      call symput('min1',compress(put(ymin, best6.)));
      call symput('min2',compress(put(xmin, best6.)));
      call symput('max1',compress(put(ymax, best6.)));
      call symput('max2',compress(put(xmax, best6.)));
      call symput('inc1',compress(put(yinc, best6.)));
      call symput('inc2',compress(put(xinc, best6.)));
      return;

doinc:
		*-- Determine increment to give a nice number with about 6 ticks --;
		inc= abs(max - min)/6;
		pow = 10**floor( log10(inc) );
		nice=1000;
		do in = 1, 2, 2.5, 4, 5;
			ut = in * pow;
			if abs(inc-ut) < nice then do;
				nice = abs(inc-ut);
				best = ut;
			end;
		end;
		inc=best;
		return;
run;
 
   options notes;
 
   *---Write the Generated AXIS Statements to the Log---;
 
   %put EQUATE: The following statements were generated.;
   %put &vaxis length=&len1 IN order=(&min1 to &max1 by &inc1) label=(a=90) &yopts%str(;);
   %put &haxis length=&len2 IN order=(&min2 to &max2 by &inc2) &xopts%str(;);
	%put;
 
	*-- Create the AXIS statements;
	&vaxis length=&len1 IN order=&min1 to &max1 by &inc1 label=(a=90) &yopts;
	&haxis length=&len2 IN order=&min2 to &max2 by &inc2 &xopts;

   *---Create the GPLOT---;
   %if %upcase(&plot)=YES %then %do;
   proc gplot data=&data;
      symbol1 v=none;
      plot &y*&x=1 / annotate=&anno frame haxis=&haxis vaxis=&vaxis
                     href=0 vref=0 lvref=3 lhref=3;
      run;
   %end;
%mend equate;
