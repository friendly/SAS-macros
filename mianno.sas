/*
Create an annotate data set to show imputed observations 
in a scatterplot by a central value and measure of variability
for each missing variable. 


*/
%macro mianno(
	imputed=_last_, /* name of data set containing imputations */
    x=,             /* name of X variable                      */
    y=,             /* name of X variable                      */
	id=,            /* observation ID variable                 */
	loc=mean,       /* central value: MEAN or MEDIAN           */
	std=stderr,     /* error bar: STD or STDERR                */
    symbol=circle,  /* plot symbol for a missing observation   */
    color=red,      /* symbol color                            */
    size=1,         /* symbol size                             */
	copy=,          /* variables to copy to output data set    */
    out=missmi      /* name of output data set                 */
);

proc sort data=&imputed;
   by &id _imputation_;

*-- Get location and variability measures for &X and &Y;
proc summary data=&imputed nway;
    by &id;
    var &x;
    output out=_sumx_(drop=_type_ _freq_) &loc=_x_ &std=stdx;

proc summary data=&imputed nway;
    by &id;
    var &y;
    output out=_sumy_(drop=_type_ _freq_) &loc=_y_ &std=stdy;

data _stats_;
	merge &imputed _sumx_ _sumy_;
	by &id;
	drop _Imputation_;
	if first.&id;

data &out;
	set _stats_;
	where (stdx>0) or (stdy>0);
	length function $8;
	drop _x_ _y_ stdx stdy;
    xsys = '2';  ysys='2';
    x = _x_;
    y = _y_;
    color = "&color";

    function = 'symbol'; text = "&symbol"; size=&size; output;

	if (stdx>0) then do;
		y = _y_;
    	x = _x_ - stdx;
    	function = 'move'; output;
    	x = _x_ + stdx;
    	function = 'draw'; output;
		end;

	if (stdy>0) then do;
		x = _x_;
    	y = _y_ - stdy;
    	function = 'move'; output;
    	y = _y_ + stdy;
    	function = 'draw'; output;
		end;

	if (stdx>0) and (stdy>0) then do;
    	x = _x_ - stdx;
		y = _y_;
    	function = 'poly'; output;
    	x = _x_;
		y = _y_ + stdy;
    	function = 'polycont'; output;
    	x = _x_ + stdx;
		y = _y_;
    	function = 'polycont'; output;
    	x = _x_;
		y = _y_ - stdy;
    	function = 'polycont'; output;
		end;
	run;
%done:

  *-- Clean up datasets no longer needed;
proc datasets nofs nolist library=work memtype=(data);
    delete _sumx_ _sumy_ _stats_;
	 run;

%mend;

%macro miplot(
    data=,               /* name of input data set                  */
	imputed=_last_,      /* name of data set containing imputations */
    x=,                  /* name of X variable                      */
    y=,                  /* name of X variable                      */
	id=,                 /* observation ID variable                 */
	loc=mean,            /* central value: MEAN or MEDIAN           */
	std=stderr,          /* error bar: STD or STDERR                */
    symbols=dot circle,  /* plot symbol for non-miss and missing    */
    colors=black red,    /* symbol colors for non-miss and missing  */
    size=1,              /* symbol size(s)                          */
	copy=,               /* variables to copy to output data set    */
	vaxis=,              /* AXIS statement for vertical axis        */
	haxis=,              /* AXIS statement for horizontal axis      */
	anno=,               /* additional annotate data set            */
    out=missmi           /* name of output annotate data set        */
);

%let abort=0;
	%if %length(&x)=0 or %length(&y)=0 %then %do;
	    %put ERROR:  Both X= and Y= must be specified.;
		%let abort=1;
	    %goto done;
		%end;
	%if %length(&id)=0 %then %do;
	    %put ERROR:  An ID= must be specified.;
		%let abort=1;
	    %goto done;
		%end;
	%if %length(&imputed)=0 %then %do;
	    %put ERROR:  A DATA= data set must be specified.;
		%let abort=1;
	    %goto done;
		%end;
	%if %length(&imputed)=0 %then %do;
	    %put ERROR:  An IMPUTED= data set from PROC MI must be specified.;
		%let abort=1;
	    %goto done;
		%end;
	
	%local c1 c2 s1 s2 z1 z2;
	%let c1= %scan(&colors &colors,1);                                                      
	%let c2= %scan(&colors &colors,2);
	%let s1= %scan(&symbols &symbols,1);                                                      
	%let s2= %scan(&symbols &symbols,2);
	%let z1= %scan(&size &size,1);                                                      
	%let z2= %scan(&size &size,2);

%mianno(imputed=&imputed, x=&x, y=&y, id=&id, 
	symbol=&s2, color=&c2, size=&z2, std=&std, 
	copy=&copy, out=&out);
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

	%if %length(&vaxis)=0 %then %do;
	axis98 label=(a=90 r=0) offset=(3,3);
	%let vaxis=axis98;
	%end;
	%if %length(&haxis)=0 %then %do;
	axis99 offset=(3,3);
	%let haxis=axis99;
	%end;
	
%if %length(&anno) %then %do;
	data &out;
		set &in &out;
	%end;

symbol1 v=&s1 c=&c1 h=&z2;

proc gplot data=&data;
	plot &y * &x / vaxis=&vaxis haxis=&haxis anno=&out
	;
	run; quit;
goptions reset=symbol;  *-- cancel prior SYMBOL stmts;
%done:
%if &abort %then %put ERROR: The MIPLOT macro ended abnormally.;
%mend;

