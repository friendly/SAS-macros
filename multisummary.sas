 /*--------------------------------------------------------------*
  *    Name: multisummary.sas                                    *
  *   Title: Calculate Summary statistics for multiple variables *
        Doc: http://www.datavis.ca/sasmac/multisummary.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 23 Mar 2005 10:23:45                                *
  * Revised: 10 Jan 2012 09:53:31                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The MULTISUMMARY macro produces an output data set containg any
 of the statistics calculated by PROC SUMMARY for any number of
 numeric variables.

=Usage:

 The MULTISUMMARY macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%multisummary();
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* VAR=        The name of the variable to be analyzed [Default: VAR=_NUMERIC_]

* CLASS=      Name(s) of 0 or more class variables

* STATS=      List of names of the output statistics to calculate, e.g., MEAN,
              MEDIAN, Q1, Q3, etc.  Any of the statistic keywords accepted by
              PROC SUMMARY.

* OPTIONS=    Options for PROC SUMMARY, e.g., nway, missing...

* OUT=        The name of the output data set.  The observations in this
              data set correspond to the combinations of the levels of all
			  class variables with the set of statistics specified in the
			  STATS= option.

=Example:

  %include data(guerry);

  %multisummary(data=guerry,
	class=region,
	var=Suicides Infants Crime_prop  Crime_pers,
	stats=Q1 median Q3,
	options=missing nway,
	out=gstats);

proc print data=gstats;

 =*/

%macro multisummary(
	data=_last_,    /* name of input data set */
	var=_numeric_,  /* names of analysis variables                      */
	class=,         /* name(s) of  0 or more class variables            */
	stats=,         /* names of output statistics to calculate          */
	options=,       /* options for proc summary, e.g., nway, missing... */
	out=
	);


%let nstat=%words(&stats);

options nonotes;
proc datasets nolist nowarn;
	delete &out;

%do i=1 %to &nstat;
	%let stat=%scan(&stats, &i);
	proc summary data=&data &options;
	%if %length(&class) %then %do;
		class &class;
		%end;
	var &var;
	output out=_out  &stat= ;

	data _out;
		length _statistic_ $8;
		set _out;
		_statistic_ = "&stat";
		label _statistic_ = 'Name of statistic';
	
	proc append base=&out new=_out;
	run;
	%end;

%if %length(&class) %then %do;
proc sort data=&out;
	by &class;
	%end;

proc datasets nolist nowarn;
	delete _out;
options notes;

%mend;

		
