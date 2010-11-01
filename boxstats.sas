%macro boxstats(
	data=_last_,
	var=,
	class=,
	ofactor=1.5,            /* IQR multiplier for outside values  */
	out=stats,
	outside=outside
	);

%if %length(&class) %then %do;
	proc sort data=&data out=_sorted_;
		by &class;
	%end;
%else %do;
	data _sorted_;
		set &data;
		_class_=1;
	%let class=_class_;
	%end;
%let indat=_sorted_;

proc univariate data=&indat noprint;
    by &class;
    var &var;
    output out=&out
           n=n q1=_q1_  q3=_q3_  median=_median_ qrange=_iqr_ ;
data &out;
    set &out nobs=grps;
    by &class;
    Lo_Notch = _median_ - 1.58*_iqr_ / sqrt(N);
    Hi_Notch = _median_ + 1.58*_iqr_ / sqrt(N);
run;
*proc print;

data &outside;
    merge &indat &out;
	drop n _q1_ _q3_ _median_ _iqr_ lo_notch hi_notch; 
    by &class;
	
    if &var > .Z;
    _out_=1;
    if &var < (_q1_   -&ofactor*_iqr_) or &var > (_q3_ +&ofactor*_iqr_)
       then _out_=2;
    if &var < (_q1_ -2*&ofactor*_iqr_) or &var > (_q3_ +2*&ofactor*_iqr_)
       then _out_=3;
    run;
	
%mend;

%include data(guerry);

/*
options mprint;
%boxstats(data=guerry, var=Crime_prop);
proc print data=outside;
	where _out_>1;
	id dept Department;
*/	
	
