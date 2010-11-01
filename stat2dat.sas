/*= 
 name: STAT2DAT
title: Transform a summary data set to pseudo-observations
  Doc: http://www.datavis.ca/sasmac/stat2dat.html
Version: 1.1
Revised: 2 Apr 1999 

=Description:

Take a dataset containing summary statistics (N, mean, std dev) for
a between groups design and produce a dataset from which PROC GLM
can be run to produce equivalent results.

=Usage:
   %stat2dat(data=inputdataset, out=outputdataset, ..., 
      depvar=Y, freq=freq)

      The input dataset contains one observation for each group.
      Supply the names of variables containing the N, MEAN, and standard
      deviation (STD) for each group (see argument list below);  The
      mean square error (MSE) for a reported ANOVA can be supplied instead
      of individual STD values.  The sample size per cell can be supplied
      as a constant rather than a dataset variable if all groups are of the
      same size.  

      The output dataset can then be used with PROC GLM or PROC ANOVA
      (balanced designs).  It contains all variables from the input dataset
      plus a constructed dependent variable ('Y' by default) and
      a constructed frequency variable ('freq' by default).
      
   proc glm data=outputdataset;
      class classvars;
      freq freq;
      model Y = modelterms;
      
Based on:  David Larsen, Analysis of Variance With Just Summary Statistics
   as Input,  The American Statistician, May 1992, Vol. 46(2), 151-152.
   (David Larson:   dalef@uno.edu)


Michael Friendly	<friendly@yorku.ca>
Psychology Department, York University
Toronto, ONT  M3J 1P3 CANADA
=*/

%macro stat2dat(
   data=_last_,	/* name of input data set */
   class=,		/* names of one or more class varialbes */		
   n=,			/* data set variable containing group N */
   mean=,		/* data set variable containing group mean */
   std=,			/* data set variable containing group standard deviation */
   mse=,			/* or, supply a constant or variable containing MSE */
   verify=0,	/* non-zero to print computed means, to verify the result */
   out=_data_,	/* name of output data set */
   freq=freq,	/* name of constructed frequency variable */
   depvar=Y,	/* name of constructed dependent variable */
	label=,     /* label for dependent variable */
	expand=N);

%if &n=%str() or &mean=%str() %then %do;
   %put ERROR: n= and mean= variables must be supplied;
   %goto fini;
   %end;
      
%if &std=%str() and &mse=%str() %then %do;
   %put ERROR: Either a std=  variables or mse= value must be supplied;
   %goto fini;
   %end;
      
/* Calculate values for depvar and freq for 2 pseudo-observations
   for each group which would yield the same mean, std and total N
*/
data &out;
   set &data;
   drop x1sur x2sur var;
   %if &std ^= %str()
      %then %str( var = &std**2; );
      %else %str( var = &mse;    );
      
	%if %length(&label)>0 %then %do;
		label &depvar = "&label";
	%end;
   if &n > 0 then do;
      x1sur = &mean + sqrt(var/&n);
      x2sur = &n*&mean - (&n -1)*x1sur;
      &depvar=x1sur; &freq=&n-1;  output;
      &depvar=x2sur;    &freq=1;  output;
   end;
   else do;
      put 'ERROR: STAT2DAT- Cannot generate data for non-positive N' ;
      _error_=1;
   end;

%if %substr(%upcase(&expand),1,1) = Y %then %do;
data &out;
   set &out;
	do i=1 to &freq;
		output;
		end;
	drop i;
%end;
   
%if &verify ^= 0 %then %do;
proc means mean std;
   class &class;
   var &depvar;
   freq &freq;
   %end;
   
%fini:;
%mend;	