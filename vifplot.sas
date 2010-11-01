/*******************************************************************
Name:           vifplot
Title:          Partial regression, partial residual and overlaid VIF plots
                Detecting nonlinearity, outliers/influential obs., multicolliniarity.
Requirment:     SAS Release 6.04 or later
Version:        1.0                    
Date: 09/09/96
Revised:
Products:       SAS/BASE SAS/STAT SAS/GRAPH
Author:         George C.J. Fernandez (GCJF@scs.unr.edu)
                (Please send e-mail with any suggestion/comments)

Disclaimer:     This macro is provided "AS IS". There are no warranties 
                expressed or implied.
                
********************************************************************/


%macro vifplot(
	data=_LAST_,
	resp=,
	pred=,
	term=,
	size=1,
	plots=PARTRES PARTREG VIF
	);
  
%let m = %nterm(term=&term);
%let q = %nvar(pred=&pred);
  
%do i = 1 %to &m;

%let vi =%scan(&term, &i);
%let p =%eval(&q - 1);

data vi_1;
	set &data(keep=&pred); run;
data vari;
	set vi_1(drop =&vi );
	array l _numeric_;
	array o o1-o&p;
	do over l;
		o=l;
		end;
	run;

data part;
	merge vari &data ;
	run;


proc reg data=part outest=b;
	var &pred o1-o&p ;
	model &resp =&pred/ noprint ;
	output out=new r=res&i;
	Title 'Multicollinearity diagnostics';
	run;

	model &resp=o1-o&p /noprint; 
	output out=yres r=y_x&i ;
	run;
	model &vi= o1-o&p /noprint;
	output out=one  r=r1&i;
	run;
	model &Vi=/noprint;
	output out =two r=r2&i;
	run;


data ar(keep=ar&i);
	if _n_ = 1 then set b(keep=&VI rename =(&VI=b&i));
	set new;
	ar&i=res&i+(&vi*b&i);
	label ar&i = " Partial Residual: &VI";
run;
proc standard m=0 data=ar out=ar;
	var ar&i;
run;

data three;
	merge  yres one two ar;
	label ar&i  = "Partial Residual: &VI"
         y_x&i = "Partial Regression: &VI"
         r1&i  = "&VI residual";
	run;

/*
goptions  norotate nocell reset=all
         hpos=0 vpos=0 htext=&size ftext=hwcgm001 ctext=blue DEVICE=&DEV nosymbol
         gaccess="sasgastd>&dir\parres&i..cgm" gsfmode= replace noborder
            ;
*/
%if %index(&plots, PARTRES) > 0 %then %do;
proc gplot data=three ;
	title  "Partial Residual plot";
	symbol1 color=red   i=rl l=1  v= 'r' h=1 w=2;
	symbol2 color=blue  i=rq l=1  v=none     w=2 ;

	plot  ar&i*&VI
			ar&i*&vi / 
		overlay VAXIS=axis1 haxis=axis2 frame vminor=0  hminor=0  ;
	axis1 label=( a=90 r=0 h=&size)
					value=(h=&size) offset=(4 pct) ;
	axis2 label=(h=&size "&vi")
				value=(h=&size)
				offset=(4 pct) ;			
	run;
%end;

%if %index(&plots, PARTREG) > 0 %then %do;
proc gplot data=three ;
	title  "Partial Regression plot";
	symbol1 color=blue  i=rl l=1  v= 'e' h=1 w=2;
	symbol2 color=red   i=rq l=1  v=none     w=2 ;

	plot  y_x&i*r1&i 
			y_x&i*r1&i / 
			overlay VAXIS=axis1 haxis=axis2 frame vminor=0  hminor=0  ;
	axis1 label=(a=90 r=0 h=&size)
					value=(h=&size) offset=(4 pct) ;
	axis2 label=(h=&size)
				value=(h=&size)
				offset=(4 pct) ;
	run;
%end;


%if %index(&plots, VIF) > 0 %then %do;
proc gplot data=three ;
	title  "Partial residual and regression plots";
	symbol1 color=red   i=rl l=1 v= 'r'  h=1 w=2;
	symbol2 color=blue  i=rl l=1 v= 'e'  h=1 w=2;
	symbol3 color=red   i=rq l=2             w=2;

	plot ar&i * r2&i  
	    Y_x&i * r1&i
		 ar&i  * r2&i  / 
		 overlay vref=0 href=0 lhref=4 lvref=4
		 VAXIS=axis1 haxis=axis2 frame vminor=0  hminor=0  ;
	axis1 label=(a=90 r=0  h=&size "Partial values: &VI")
					value=(h=&size) offset=(4 pct) ;
	axis2 label=( h=&size "Partial residual- &VI")
				value=(h=&size) offset=(4 pct) ;      
	run;
%end;

%end;

title ;
proc datasets nofs nolist;
delete ar  b  new  one  part   three    two   vari   vi_1  yres ;  
run;
quit;
%mend vifplot;

%macro nterm(term);
  %let i = 1;
  %let vi = %scan(&term,&i);
     %do %while (%length(&vi) > 0);
        %let i = %eval(&i + 1);
        %let vi = %scan(&term,&i);
        %end;
  %eval(&i - 1)
  %put the term number &i;
%mend;

%macro nvar(pred);
  %let j = 1;
  %let vj = %scan(&pred,&j);
     %do %while (%length(&vj) > 0);
        %let j = %eval(&j + 1);
        %let vj = %scan(&pred,&j);
        %end;
  %eval(&j - 1)
  %put the model term number &j;
%mend;

