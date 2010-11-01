 /*--------------------------------------------------------------*
  *    Name: miplot.sas                                          *
  *   Title: Plot X-Y data for missing data from PROC MI         *
        Doc: http://www.datavis.ca/sasmac/miplot.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 05 Oct 2004 14:19:10                                *
  * Revised: 20 Oct 2010 09:16:09                                *
  * Version: 1.2-0                                               *
  * 1.1  Added marginal annotations of the number of missing on  *
  *      X, Y and both to the marginal plot.and imputed plot     *
  *    - Changed MIANNO so that it leaves the IMPUTED data set
  *      unchanged, avoiding errors with MIANALYZE
  *    - In MIANNO, clip error bars to range of X,Y, avoiding
  *      annotate errors
  * 1.2  Added ELLIPSE= to show ellipses for OBS and/or IMPuted
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The MIPLOT macro plots X-Y data with indicators for missing
 observations, and is useful for seeing whether the pattern of missing
 data appears systematic or random with respect to the X, Y variables.
 The TYPE=MARGINAL plot simply shows observations with missing X, Y,
 or both by marginal symbols along the axes.  The TYPE=IMPUTED plot
 requires an imputed data set from PROC MI, and plots error bars (when
 one of X,Y is missing) and diamonds (when both X and Y are missing)
 showing the variation of imputed values for missing data.

=Usage:

 The MIPLOT macro is defined with keyword parameters.  The X=, Y= and
 DATA= parameters must be specified. An ID= variable is required for the
 TYPE=IMPUTED plot.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
    proc mi data=mydata  out=mymi nimpute=10;
        var ...;
        run;
	%miplot(data=mydata, imputed=mymi, ...);
 
==Parameters:

* DATA=       Name of the input data set

* IMPUTED=    Name of data set containing imputations [Default: IMPUTED=_LAST_]

* X=          Name of the X variable

* Y=          Name of the Y variable

* ID=         Observation ID variable

* TYPE=       Plot type: MARGINAL or IMPUTED.  The default is TYPE=IMPUTED if
              an IMPUTED= data set is specified; otherwise MARGINAL.

* LOC=        Central value used for imputed values in the TYPE=IMPUTED plot:
              MEAN or MEDIAN [Default: LOC=MEAN]

* STD=        Length of error bars for imputed values in the TYPE=IMPUTED plot: 
             STD or STDERR [Default: STD=STDERR]

* MULT=       Multiple of error bar length [Default: MULT=1]

* AT=         Percent location on the missing axis used to plot a point in the
              TYPE=MARGINAL plot [Default: AT=1]

* JITTER=     Amount to jitter the percent location for a missing point in the
              TYPE=MARGINAL plot [Default: JITTER=1]

* SYMBOLS=    Plot symbol for non-missing and missing observations
              [Default: SYMBOLS=DOT CIRCLE]

* COLORS=     Symbol colors for non-missing and missing [Default: COLORS=BLACK RED]

* SIZE=       Symbol size(s) [Default: SIZE=1]

* INTERP=     Plot interpolation option [Default: INTERP=NONE]

* COPY=       Variables to copy to output data set

* VAXIS=      AXIS statement for vertical axis

* HAXIS=      AXIS statement for horizontal axis

* ANNO=       Additional annotate data set to be added to the plot

* OUT=        Name of output annotate data set [Default: OUT=MISSANNO]

* OUTSTAT=    Name of output statistics data set when TYPE=IMPUTED
              [Default: OUTSTST=_STATS_]

* NAME=       Name for graphics catalog entry [Default: OUT=Miplot]

 =*/


%macro miplot(
    data=,               /* name of input data set                      */
	imputed=_last_,      /* name of data set containing imputations     */
    x=,                  /* name of X variable                          */
    y=,                  /* name of X variable                          */
	id=,                 /* observation ID variable                     */
	type=,               /* plot type: MARGinal or IMPUted              */
	loc=mean,            /* central value: MEAN or MEDIAN               */
	std=stderr,          /* error bar: STD or STDERR                    */
    mult=1,              /* multiple of error bar length                */
    at=1,                /* % location on missing axis in MARG plot     */
    jitter=1,            /* amount to jitter % location in MARG plot    */
    symbols=dot circle,  /* plot symbol for non-miss and missing        */
    colors=black red,    /* symbol colors for non-miss and missing      */
    size=1,              /* symbol size(s)                              */
	interp=none,         /* plot interpolation option                   */
	copy=,               /* variables to copy to output data set        */
	vaxis=,              /* AXIS statement for vertical axis            */
	haxis=,              /* AXIS statement for horizontal axis          */
	ellipse=,            /* draw ellipse(s) for OBServed, IMPuted ?     */
	anno=,               /* additional annotate data set(s)             */
    out=missanno,        /* name of output annotate data set            */
	outstat=_stats_,     /* name of output statistics data set          */
	name=miplot          /* name for graphics catalog entry             */
);

%if %length(&type)=0 %then %do;
    %if %length(&imputed)
        %then %let type=IMPUTED;
        %else %let type=MARGINAL;
    %end;
%let type = %substr(%upcase(&type,1,4));

%let abort=0;
	%if %length(&x)=0 or %length(&y)=0 %then %do;
	    %put ERROR:  Both X= and Y= variables must be specified.;
		%let abort=1;
	    %goto done;
		%end;

	%if %length(&id)=0 and &type=IMPU %then %do;
	    %put ERROR:  An ID= variable must be specified with TYPE=IMPUTED;
		%let abort=1;
	    %goto done;
		%end;
	%if %length(&data)=0 %then %do;
	    %put ERROR:  A DATA= data set must be specified.;
		%let abort=1;
	    %goto done;
		%end;
	%if %length(&imputed)=0 and &type=IMPU  %then %do;
	    %put ERROR:  An IMPUTED= data set from PROC MI must be specified with TYPE=IMPUTED.;
		%let abort=1;
	    %goto done;
		%end;
	
	%local c1 c2 s1 s2 z1 z2;
	%let c1= %scan(&colors &colors,1);                                                      
	%let c2= %scan(&colors &colors,2);      %if %length(&c2)=0 %then %let c2=&c1;
	%let s1= %scan(&symbols &symbols,1);                                                      
	%let s2= %scan(&symbols &symbols,2);    %if %length(&s2)=0 %then %let s2=&s1;
	%let z1= %scan(&size &size,1,%str( ));                                                      
	%let z2= %scan(&size &size,2,%str( ));  %if %length(&z2)=0 %then %let z2=&z1;

%if &type=IMPU %then %do;
    %mianno(imputed=&imputed, x=&x, y=&y, id=&id, 
	    symbol=&s2, color=&c2, size=&z2, std=&std, mult=&mult,
	    copy=&copy, out=&out);
    %end;
%else %do;
    %missanno(data=&data, x=&x, y=&y, 
	    symbol=&s2, color=&c2, size=&z2, 
	    copy=&copy, out=&out);
    %end;


%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

	%if %length(&vaxis)=0 %then %do;
	axis98 label=(a=90 r=0) offset=(3,3);
	%let vaxis=axis98;
	%end;
	%if %length(&haxis)=0 %then %do;
	axis99 offset=(3,3);
	%let haxis=axis99;
	%end;
	
%if &type=IMPU %then %do;
%let extra=;
%if %length(&ellipse) %then %do;
	%if %index(%upcase(&ellipse), OBS) %then %do;
		%ellipses(data=&data, x=&x, y=&y, colors=&c1, out=_ellobs_, plot=no);
		%let extra = _ellobs_;
		%end;
	%if %index(%upcase(&ellipse), IMP) %then %do;
		%ellipses(data=_stats_, x=_x_, y=_y_, colors=&c2, out=_ellimp_, plot=no);
		%let extra = &extra _ellimp_;
		%end;
	%end;

%if %length(&anno) or %length(&extra) %then %do;
	data &out;
		set &anno &out &extra;
	%end;
  %end;

symbol1 v=&s1 c=&c1 h=&z2 i=&interp;

proc gplot data=&data;
	plot &y * &x / vaxis=&vaxis haxis=&haxis 
		anno=&out name="&name" des="MIPLOT of &data"
	;
	run; quit;
goptions reset=symbol;  *-- cancel prior SYMBOL stmts;
%done:
%if &abort %then %put ERROR: The MIPLOT macro ended abnormally.;
%mend;


/*
Create an annotate data set to show imputed observations 
in a scatterplot by a central value and measure of variability
for each missing variable. 


*/
%macro mianno(
	imputed=_last_, /* name of data set containing imputations     */
    x=,             /* name of X variable                          */
    y=,             /* name of X variable                          */
	id=,            /* observation ID variable                     */
	loc=mean,       /* central value: MEAN or MEDIAN               */
	std=stderr,     /* error bar: STD or STDERR                    */
    mult=1,         /* multiple of error bar length                */
    symbol=circle,  /* plot symbol for a missing observation       */
    color=red,      /* symbol color                                */
    size=1,         /* symbol size                                 */
    when=b,         /* when to draw annotations                    */
	copy=,          /* variables to copy to output data set        */
	outstat=_stats_, /* name of output statistics data set         */
    out=missanno    /* name of output annotate data set            */
);

proc sort data=&imputed out=_impsorted_;
   by &id _imputation_;

*-- Get location and variability measures for &X and &Y;
proc summary data=_impsorted_ nway;
    by &id;
    var &x;
    output out=_sumx_(drop=_type_ _freq_) &loc=_x_ &std=stdx min=_minx_ max=_maxx_;

proc summary data=_impsorted_ nway;
    by &id;
    var &y;
    output out=_sumy_(drop=_type_ _freq_) &loc=_y_ &std=stdy min=_miny_ max=_maxy_;

data &outstat;
	merge _impsorted_ _sumx_ _sumy_;
	by &id;
	drop _Imputation_;
	if first.&id;

data &out;
	set &outstat end=eof;
	where (stdx>0) or (stdy>0);
	length function color text $8;
	drop _x_ _y_ stdx stdy at nx ny nxy _minx_ _maxx_ _miny_ _maxy_;
    xsys = '2';  ysys='2';
    x = _x_;
    y = _y_;
    color = "&color"; when="&when";

    function = 'symbol'; text = "&symbol"; size=&size; output;

	if (stdx>0) then do;
		y = _y_;
    	x = max(_minx_, _x_ - &mult * stdx);
    	function = 'move'; output;
    	x = min(_maxx_, _x_ + &mult * stdx);
    	function = 'draw'; output;
		nx+1;
		end;

	if (stdy>0) then do;
		x = _x_;
    	y = max(_miny_, _y_ - &mult * stdy);
    	function = 'move'; output;
    	y = min(_maxy_, _y_ + &mult * stdy);
    	function = 'draw'; output;
		ny+1;
		end;

	if (stdx>0) and (stdy>0) then do;
    	x = _x_ - &mult * stdx;
		y = _y_;
    	function = 'poly'; output;
    	x = _x_;
		y = _y_ + &mult * stdy;
    	function = 'polycont'; output;
    	x = _x_ + &mult * stdx;
		y = _y_;
    	function = 'polycont'; output;
    	x = _x_;
		y = _y_ - &mult * stdy;
    	function = 'polycont'; output;
		nxy+1;
		end;
	if eof then do;
        xsys = '1';  ysys = '1'; size=.;
		function = 'label';
		at = 1;
		if ny >0 then do;
			y = at; x = 98; position='A';
			text = left(put(ny, 5.));
			output;
			end;
		if nx >0 then do;
			x = at; y = 99; position='F';
			text = left(put(nx, 5.));
			output;
			end;
		if nxy >0 then do;
			x = at; y = at; position='3';
			text = left(put(nxy, 5.));
			output;
			end;
		end;
	run;
%done:

  *-- Clean up datasets no longer needed;
proc datasets nofs nolist library=work memtype=(data);
    delete _sumx_ _sumy_ _impsorted_;
	 run; quit;

%mend;

/*
Create an annotate data set to show missing observations 
in a scatterplot by symbols along the marginal axes
*/

%macro missanno(
    data=_last_,    /* name of input data set                   */
    x=,             /* name of X variable                       */
    y=,             /* name of X variable                       */
    at=1,           /* % location on missing axis               */
    jitter=1,       /* amount to jitter % location              */
    symbol=circle,  /* plot symbol for a missing observation    */
    color=red,      /* symbol color                             */
    size=1,         /* symbol size                              */
    when=b,         /* when to draw annotations                 */
	copy=,          /* variables to copy to output data set     */
	in=,            /* input annotate data set                  */
    out=missxy      /* name of output data set                  */
);
    data &out;
		keep &x &y x y xsys ysys function text size color position &copy;
		retain nx ny nxy;
        set &data end=eof;
        where (&x=.) or (&y=.);
    	length function color text $8 position $1;
        function = 'symbol'; text = "&symbol"; size=&size; when="&when";
        color = "&color";
        if &x = . then do;
            xsys = '1';  ysys = '2';
            x = &at + &jitter * uniform(21342141);
            y = &y;
			nx+1;
			if &y = . then nxy+1;
            output;
            end;
        if &y = . then do;
            xsys = '2';  ysys = '1';
            y = &at + &jitter * uniform(21342141);
            x = &x;
			ny+1;
            output;
            end;
		if eof then do;
			*-- annotate the number of missings for X & Y;
			*put nx= ny= nxy=;
            xsys = '1';  ysys = '1'; size=.;
			function = 'label';
			if ny >0 then do;
				y = &at; x = 98; position='A';
				text = left(put(ny, 5.));
				output;
				end;
			if nx >0 then do;
				x = &at; y = 99; position='F';
				text = left(put(nx, 5.));
				output;
				end;
			if nxy >0 then do;
				if &at < 50 then do;
					position = '3';
					offset = 1;
					end;
				else do;
					position = '7';
					offset = -1;
					end;
				x = &at+offset; y = &at+offset;
				text = left(put(nxy, 5.));
				output;
				end;
			end;
			
	run;

%if %length(&in)>0 %then %do;
    data &out;
        set &in &out;
    %end;
    
%mend;

