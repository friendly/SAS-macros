 /*--------------------------------------------------------------*
  *    Name: colors.sas                                          *
  *   Title: Produce color chart from a data set of color values *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 10 Apr 98 11:16                                     *
  * Revised: 02 Mar 2005 11:03:11                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The COLORS macro produces a chart displaying a set of color bars
 from an input data set containing names of SAS/GRAPH colors

=Usage:

 The COLORS macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%colors(color=clr, cols=8, rows=10);
 
==Parameters:

 =*/
%macro colors(
	data=_last_,      /* input data set           */
	color=,           /* color variable           */
	cname=,           /* color name variable      */
	rows=,            /* number of rows/page      */
	cols=10,          /* number of columns/row    */
	out=annodata      /* output annotate data set */
	);

%if %length(&color)=0 %then %do;
	%put ERROR:  The COLOR= variable must be specified.;
	%goto done;
	%end;
%let color=%upcase(&color);
%if %index(|COLOR| |TEXT| |STYLE|, |%trim(&color)|) > 0 %then %do;
	%put ERROR:  The COLOR= variable may not be named &color;
	%goto done;
	%end;

%if %upcase(&data)=_LAST_ %then %let data=&syslast;
	
data _null_;
*	if 0 then set &data nobs=nobs;
	set &data(obs=1) nobs=nobs;
	call symput('nobs',trim(left(put(nobs,12.))));
	%if %length(&rows)=0 %then %do;
		%if %length(&cols)=0 %then %do;
			c = min(int(sqrt(nobs)), 16);
			r = ceil(nobs/c);
			call symput('cols',trim(left(put(c,12.))));
			call symput('rows',trim(left(put(r,12.))));
			%end;
		%else %do;
			r = ceil(nobs/&cols);
			call symput('rows',trim(left(put(r,12.))));
			%end;
		%end;
	%else %do;
		%if %length(&cols)=0 %then %do;
			c = ceil(nobs/&rows);
			call symput('cols',trim(left(put(c,12.))));
			%end;
		%end;
	run;


	    
  data &out;
    set &data;
	 drop count &cname;
    count = mod(_n_-1, &rows*&cols)+1;
    page + (count=1);
    call symput("pages", compress(put(page,best.)));
    _row_ = mod(ceil(count/&cols)-1,&rows) + 1;
    _col_ = mod(count-1, &cols);
    xsys = '5';
    ysys = '5';
 
    function = 'MOVE    ';
    x = 100 * (_col_/&cols);
    y = 100 * (1-_row_/&rows);
    output;
 
    function = 'BAR    ';
    x = x+ 100/&cols - 0.5;
    y = y+ ( 90 - 4*&rows)/&rows;
    color = &color;
    style = 'solid   ';
    output;
 
    function = 'LABEL';
    text = &color;
    size = 0.7 + .035*(16-&cols);
    style = '';
    x = 100 * (_col_/&cols);
    position = '3';
    color = '';
    output;
 
	%if %length(&cname)>0 %then %do;
    y = 100 * (1-_row_/&rows);
    text = &cname;
    size = 0.7 + .04*(16-&cols);
    position = '6';
    output;
	 %end;
  run;
 
  data _null_;
    set &out;
    by page;
    if first.page then put 'NOTE: ' page 3. '  ' &color $10. @;
    if last.page then put &color $10.;
  run;
 
  %do page = 1 %to &pages;
    %put NOTE: page &page of &pages;
    data _page_;
      set &out;
      where (page=&page);
    run;
    proc gslide border annotate=_page_;
    * note "&page of &pages  &sysday &sysdate &systime";
      title1 h=1  "Color chart for &color in &data, SAS &sysver &sysscp  &sysdate [&sysdevic driver]"
			j=r "&page/&pages";
    run; quit;
  %end;
	title1;

%done:
%mend colors;
