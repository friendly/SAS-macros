 /*--------------------------------------------------------------*
  *    Name: lackfit.sas                                         *
  *   Title: Lack of fit tests for a regression model            *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 14 Oct 2004 13:01:28                                *
  * Revised: 14 Oct 2004 13:01:28                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The LACKFIT macro integrates the output of PROC REG (the ANOVA
 summary for the model) with the lack of fit test provided by
 PROC RSREG.  There must be at least several observations with
 equal values on the independent (X=) variable(s).

==Method:

 The program uses ODS to extract the relevant tables and data steps
 to combine them and make the output more readable.

=Usage:

 The LACKFIT macro is defined with keyword parameters.  The X= and Y=
 parameters are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%lackfit(data=htwt, x=x, y=y);
 
==Parameters:

* DATA=       Name of input data set [Default: DATA=_LAST_]

* Y=          Name of response variable

* X=          Name(s) of independent variable(s)

* COVAR=      Number of Xs treated as linear terms [Default: COVAR=1]

* OUT=        Lackfit name of output ANOVA table
                
=Example:

  %include data(htwt);
  %lackfit(data=htwt, x=x, y=y);

 =*/
%macro lackfit(
	data=_last_,   /* name of input data set               */
	y=,            /* name of response variable            */
	x=,            /* name(s) of independent variable(s)   */
	covar=1,       /* number of Xs treated as linear terms */
	out=lackfit    /* name of output ANOVA table           */
	);


options nonotes;

*-- Run the analyses, suppressing the output;
ods listing close;
proc reg data=&data ;
	model &y = &x;
	ods output ANOVA = _aov_;
	run; quit;

ods exclude FitStatistics ParameterEstimates;
proc rsreg data=htwt;
   model &y = &x / lackfit 
                 covar=&covar
                 noopt;
   ods output ErrorANOVA=_lof_;
   run; quit;
ods listing;

*-- Format used to order the lines in the ANOVA tables;
proc format;
	value $source 'Model'='1'  'Error'='2'  'Lack of Fit'='3'  
		'Pure Error'='4'  'Corrected Total'='5'  other=' ';
	run;
		
data _aov_;
	set _aov_;
	drop model dependent;
	_line_= put(Source, $source.);

data _lof_;
	length Source $15;
	label Source = 'Source';
	set _lof_;
	drop dependent;
	_line_= put(Source, $source.);
	if _line_ ^=' ';
	Source = '  ' || Source;

*proc print;

*-- Formats to change missings in anova tables to blanks;
proc format;
	value missf ._=' ' other=[8.3];
	value missp ._=' ' other=[pvalue6.4];
	run;
	
*-- Interleave the PROC REG and PROC RSREG anova tables by _line_;
data &out;
	set _aov_ _lof_;
	by _line_;
	drop _line_;
	format MS FValue missf. probf missp.;
	run;

options notes;
proc print data=&out noobs label;

proc datasets nolist nowarn;
	delete _aov_ _lof_;
	run; quit;

%done:
%mend;

	

