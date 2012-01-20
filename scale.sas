/*
NOTE:  In V8.2+, this macro can be replaced by PROC STDIZE
 in SAS/STAT, e.g.,
 proc stdize data=in out=out method=range;
   var ...;
   
*/
 /*--------------------------------------------------------------*
  *    Name: scale.sas                                           *
  *   Title: Scale each variable to a given range                *
        Doc: http://www.datavis.ca/sasmac/scale.html       
  *                                                              *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Revised: 28 Sep 2011 14:19:39                                *
  * Version: 1.0-1                                               *
  *--------------------------------------------------------------*/
 
/*----------------------------------------------------*
 |  Scale each variable to range from lo to hi        |
 *----------------------------------------------------*/
%macro scale (
	data=_LAST_,     /* input data set                   */
	var=_NUMERIC_,   /* variables to be scaled           */
	copy=,           /* variables to be copied unchanged */
	id=,             /* id variable                      */
	out=_DATA_ ,     /* output data set                  */
	outstat=,        /* output min/max dataset           */
	minmax=,         /* optional dataset of min/vax values */
	lo=0,            /* low value of new range           */
	hi=1);           /* high value                       */
%local rn;
%let rn=;

*-- Parse variables list if it contains special lists;
%if %index(&var,-) > 0 or "&var"="_NUMERIC_" %then %do;
 data _null_;
 set &data (obs=1);
        %*  convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
     end;
     call symput( 'VAR', trim(_vlist_) );
	  put 'NOTE: VAR= list translated to: VAR=' _vlist_;
 RUN;
%end;

proc iml;
  reset;
     use &data;
     %if &id ^= %str() %then %let rn=rowname=&id;
     read all var {&var} into  x[ &rn colname=vars ];
	 close &data;
     n = nrow( x);
     k = ncol( x);
     %if &minmax ^= %str() %then %do;
        use &minmax;
        read all var{min} into min;
        read all var{max} into max;
        min = j(n,1) * min`;
        max = j(n,1) * max`;
     %end;
     %else %do;
     min = j( n , 1 ) * x[>< ,];
     max = j( n , 1 ) * x[<> ,];
     %end;
     minmax = min[1,]` || max[1,]`;
     print "Min/max for data set &data";
     print minmax[rowname=vars colname={min max}];
     X = &LO + ( &HI - &LO ) * ( X - MIN ) / ( MAX - MIN );
     create &out   from x[ &rn colname=vars ];
     %if &id = %str() %then
        %str(append from x;);
     %else
        %str(append from x[ &rn ];);
     %if &outstat ^= %str() %then
        %do;
		   vl = {min max};
           create &outstat from minmax[ rowname=vars colname=vl];
           append from minmax[rowname=vars];
        %end;
quit;
  %if %length(&copy) >0 %then %do;
  data &out;
     merge &out &data(keep=&copy);
  %end;
%mend;
