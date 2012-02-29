 /*--------------------------------------------------------------*
  *    Name: slice.sas                                           *
  *   Title: Divide a variable into slices, allowing overlap     *
        Doc: http://www.datavis.ca/sasmac/slice.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 12 Nov 2003 09:45:12                                *
  * Revised: 16 Dec 2005 14:41:34                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The SLICE macro divides the values of a variable into subranges
 or slices for use in conditioning plots (coplots or ccmaps). If the 
 variable is character, each distinct value is a slice. Otherwise, 
 the range of the variable is divided into SLICE= ranges, allowing 
 each successive pair to overlap by a fraction, OVERLAP=, so a given
 observation can appear in more than one slice.  In conditioning
 plots, this helps to smooth the conditioning variable(s).

 The result is an output OUT= data set containing an additional variable,
 SLICEVAR= giving the slice value for each observation (which may
 appear 1 or 2 times).

=Usage:

 The SLICE macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
    %slice(data=test, var=x, slices=5);
 
==Parameters:

* DATA=       The name of the input data set

* VAR=        The name of the variable to be sliced

* SLICES=     Number of slices (if VAR is numeric) [Default: SLICES=4]

* OVERLAP=    Overlap between adjacent slices (numeric) [Default: OVERLAP=.25]

* OUT=        The name of the output data set [Default: OUT=_EXPAND_]

* OUTS=       Name of output slice data set.  This contains one observation
              per slice, with the upper/lower values and upper/lower
			  percents of observations in each slice. [Default: OUTS=_SLICES_]

* SLICEVAR=   Name for slice variable [Default: SLICEVAR=SLICE]
                

==Example:

   data test;
	   drop i;
	   do group='A1', 'A2';
		 do i=1 to 5;
           x = int(50 + 25 * normal(12424241));
           output;
    	 end;
	   end;
   run;

   %slice(data=test, var=x, out=nout, slices=5);
   proc print data=nout;
   proc print data=_slices_;

 Produces:

  Obs    slice    lowerx    upperx    n    group       x

	1      1         -6        28     3     A2        -6
	2      1         -6        28     3     A2        23
	3      1         -6        28     3     A1        28
	4      2         28        31     2     A1        28
	5      2         28        31     2     A2        31
	6      3         43        53     2     A1        43
	7      3         43        53     2     A1        53
	8      4         76        77     2     A2        76
	9      4         76        77     2     A1        77
   10      5         92       110     2     A1        92
   11      5         92       110     2     A2       110

 and, the _slices_ data set:
 
  Obs    slice    lowerx    upperx    n     lopct      uppct

   1       1         -6        28     3     0.0000     29.310
   2       2         28        31     2    29.3103     31.897
   3       3         43        53     2    42.2414     50.862
   4       4         76        77     2    70.6897     71.552
   5       5         92       110     2    84.4828    100.000


 =*/
/*-----------------------------------------------------------------*
  Title:   Divide a variable into slices, allowing overlap  

  If the variable is character, each distinct value is a slice.
  Otherwise, the range of the variable is divided into SLICE=
  ranges, allowing each successive pair to overlap by a fraction,
  OVERLAP=.
 *-----------------------------------------------------------------*/
%macro slice(
	data=,          /* name of input data set */
	var=,           /* name of the variable on which to slice */
	slices=4,       /* number of slices (if VAR is numeric) */
	overlap=.25,    /* overlap between adjacent slices (numeric) */
	out=_expand_,   /* name of output data set */
	outs=_slices_,   /* name of output slice data set */
	slicevar=slice  /* name for slice variable */
	);

%if %sysevalf(&sysver  >= 7) %then %do;
    %local o1 o2;
    %let o1 = %sysfunc(getoption(notes));
    %let o2 = %sysfunc(getoption(validvarname,keyword));
    options nonotes validvarname=v7;
    %end;
%else %do;
   options nonotes;
    %end;


%global sltype nslice;
proc sort data=&data;
   by &var;

proc contents data=&data out=_work_ noprint;
%let sltype=;
%let nslice=;
data _null_;
	set _work_;
	if upcase(name)=upcase("&VAR")
		then if type=1 then call symput('sltype', 'NUM');
							else call symput('sltype', 'CHAR');
run;
%*put sltype = &sltype;

%if &sltype=CHAR %then %do;
	*-- Find number of levels of &var and percents;
	proc freq data=&data;
	   tables &var / noprint out=&outs;
	
	data &outs;
		set &outs end=last;
		drop percent;
		rename count=n;
		&slicevar+1;
		lowerx = &var;  upperx=&var;
		lopct+(lag(percent));
		if lopct=. then lopct=0;
		uppct+percent;
		if last then do;
			call symput('nslice', put(&slicevar,2.));
			if &slicevar ^= &slices then
				put 'NOTE:  Using ' &slicevar " slices for the discrete variable &var";
			end;

	data &out;
		merge &data &outs(keep=&var &slicevar lowerx upperx);
		by &var;
	%end;    /* sltype=CHAR */

%else %do;
data &outs;
   keep &slicevar n lowerx upperx;
   f = &overlap;           *-- allowed overlap;
   k = &slices;            *-- number of intervals;

   r = nobs / (k*(1-f) + f);  *-- target number in each;

   do &slicevar = 1 to k;
      values = (&slicevar-1)*(1-f)*r;
      index1 = round( 1 + values);
      index2 = round( r + values);
      set &data point=index1 nobs=nobs; lowerx=&var;
      set &data point=index2;           upperx=&var;
	  n = index2-index1+1;
      output;
      end;
   stop;      *-- STOP is required with POINT=;

/*-------------------------------------------------------*
 | Expand the dataset to include duplicate copies of the |
 | observations which occur in two slices.               |
 *-------------------------------------------------------*/
proc sql;
   create table &out as
      select &outs..* , &data..*
      from &outs , &data
      where (lowerx <= &var) &  (&var<= upperx)
      ;
   quit;

proc summary data=&outs;
	var lowerx upperx;
	output out=_minmax_ min=lomin upmin max=lomax upmax;

data &outs;
	set &outs;
	if _n_=1 then set _minmax_;
	drop _type_ _freq_ lomin upmin lomax upmax;
	lopct = 100*(lowerx-lomin)/(upmax-lomin);
	uppct = 100*(upperx-lomin)/(upmax-lomin);
run;
%end;    /* sltype=NUM */
*proc print data=&outs;

%*-- Restore global options;
%if %sysevalf(&sysver  >= 7) %then %do;
    options &o1 &o2;
    %end;
%else %do;
   options notes;
    %end;
%mend;
