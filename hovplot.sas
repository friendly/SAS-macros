 /*--------------------------------------------------------------*
  *    Name: hovplot.sas                                         *
  *   Title: Boxplot display of homogeneity of variance tests    *
        Doc: http://www.datavis.ca/sasmac/hovplot.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 10 Jan 2005 12:33:57                                *
  * Revised: 11 Jan 2005 13:41:52                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The HOVPLOT macro provides a graphical display of information
 related to the Levine and Brown-Forsythe tests of homogeneity of 
 variance in factorial ANOVA designs.  
  
 Of the recommended tests of homogeneity of variance, the Levine 
 test and the Brown-Forsythe test take simple forms amenable to
 graphical display.  Both of these are based on an ANOVA of simple 
 functions of a dispersion variable,
 
    Z  = abs (Y - median)      [Brown-Forsythe]
    Z  = abs (Y - mean)        [Levine, TYPE=ABS]
    Z  =     (Y - mean)^2      [Levine, TYPE=SQUARE]

 O'Brien's (1979) test, is a modification of Levine's Z^2, with a more
 complex formula.  The Brown-Forsythe test appears to have the greatest
 power for detecting non-constant variance.

 The HOVPLOT macro displays these quantities by a set of boxplots,
 one for each cell in the design.  Lack of homogeneity of variance
 is indicated by differences in spread across cells.

==Method:

 Statistical tests are provided by PROC GLM using the
 HOVTEST= option on the MEANS statement, but only for one-way
 designs.  The HOVPLOT extends this test to n-way designs by
 combining multiple CLASS= variables into a single combined variable
 (whose values should be distinct) representing all the cells in
 the design.

=Usage:

 The HOVPLOT macro is defined with keyword parameters.  The VAR= and
 CLASS= parameters are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%hovplot(data=animals, var=time, class=Treatmt Poison, sortby=_iqr_);

 
==Parameters:

* DATA=       Input dataset [Default: DATA=_LAST_]

* WHERE=      WHERE clause to subset the data

* CLASS=      Grouping variable(s)

* CLASSFMT=   Format used for the CLASS= variable(s)

* SEP=        Separator character for 2+ CLASS vars [Default: SEP=%str()]

* VAR=        Name of the analysis variable

* ID=         Observation ID variable, used to label extreme observations
              in the boxplots.

* SORTBY=     How to order the classes in the boxplot.  Specify the
              name of a dataset variable, or one of the keywords
              _mean_, _iqr_, _median_, etc. recognized by the
              BOXPLOT macro. [Default: SORTBY=]

* CONNECT=    =0 or line style to connect medians [Default: CONNECT=1]

* NOTCH=      0 or 1, where 1 gives a notched boxplot

* METHOD=     Type of HOV test: one of BF or LEVINE, LEVINE(TYPE=ABS),
              LEVINE(TYPE=SQUARE), or OBRIEN  Use METHOD= (null) or 
			  METHOD=NONE to suppress the numerical test.
			  [Default: METHOD=BF]

* CENTER=     Central value subtracted from Y [Default: CENTER=MEDIAN]

* FUNCTION=   Function of (Y-&center) plotted, either ABS or SQUARE
              [Default: FUNCTION=ABS]

* OUT=        Name of output dataset, a copy of the input dataset, 
              containing additional variables _CLASS_, MEDIAN, MEAN, N

* NAME=       Name for graphic catalog entry [Default: NAME=HOVPLOT]

* GOUT=       The name of the graphics catalog [Default: GOUT=GSEG]
                
=Dependencies:

 Requires: combine.sas, boxplot.sas


 =*/

%macro hovplot(
   data=_LAST_,       /* Input dataset                           */	  
   var=,              /* Analysis variable                       */	  
   id=,               /* Observation ID variable			      */	  
   where=,            /* WHERE clause to subset data		      */	  
   class=,            /* Grouping variable(s)                     */
   classfmt=,         /* Format for class variable(s)             */	  
   sep=,              /* Separator character for 2+ CLASS vars    */	  
   sortby=,           /* Order classes by var, _mean_,...	      */
   notch=0,           /* =0|1, 1=draw notched boxes               */
   connect=1,         /* =0 or line style to connect centers      */	  
   method=bf,         /* Type of HOV test: BF or LEVINE  	      */
   center=median,     /* MEAN (Levine) or MEDIAN (Brown-Forsythe) */
   function=abs,      /* Function of (y-&center) plotted          */
   out=hovout,        /* Name of output dataset                   */ 
   name=HOVPLOT,      /* Name for graphic catalog entry 	      */	  
   gout=              /* The name of the graphics catalog         */
        );


	%let o1 = %sysfunc(getoption(notes));
	%let abort=0;
	%if %length(&var)=0 | %length(&class)=0
	   %then %do;
    	  %put ERROR: The VAR= and CLASS= parameters must be specified;
    	  %let abort=1;
    	  %goto DONE;
	   %end;

	options nonotes;
	%let nc = %words(&class);
	*-- Combine two or more class variables into a single one, for GLM;
	%combine(data=&data, var=&class, result=_class_, where=&where,
		sep=&sep, out=&out);

	%if %length(&method) and %upcase(&method)^=NONE %then %do;
	proc glm data=&out;
		class _class_;
		model &var = _class_; 
		means _class_ / hovtest=&method;
	run;
	%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
	%end;

	*-- Obtain summary statistics for each cell;
	proc summary data=&out nway;
	   class _class_;
	   var &var;
	   output out=_medians_ median=median mean=mean var=var n=n ;
	run;
	%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

	
	proc sort data=&out;
    	by _class_;
	data &out;
    	merge &out _medians_(drop=_type_ _freq_);
    	by _class_;
    	_diff_ = &var - &center;
		%if %upcase(&function) = SQUARE %then %do;
			_diff_ = _diff_ * _diff_;
			%end;
		%else %if &function = OBRIEN %then %do;
			/* using W=0.5 */
			_diff_ = ((n-1.5)*n* _diff_*_diff_ - 0.5*(n-1)*var) / ((n-1)*(n-2));
			%end;
		%else %do;
			_diff_ = &function(_diff_);
			%end;
    	label _diff_ = "&function(&var - &center)"
			/* Should join names of 2+ class variables with &sep */
			_class_ = "&class";
		run;
	%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
	*proc print data=&out;

	%boxplot(data=&out, var=_diff_, class=_class_, id=&id, 
		sortby=&sortby, notch=&notch, 
		connect=&connect,
		classlab=&class,  classfmt=&classfmt,
		name=&name, gout=&gout);

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
proc datasets nofs nolist nowarn library=work memtype=(data);
    delete _medians_;
     run; quit;
options &o1;

%done:
%if &abort %then %put ERROR: The HOVPLOT macro ended abnormally.;
%mend;


