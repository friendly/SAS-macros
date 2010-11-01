%macro project2(
	run=0,
	n=,
	delay=,
	dose=,
	task=,
	out=design);

%if %length(&delay)=0 | %length(&dose)=0 | %length(&task)=0
	%then %do;
		%put ERROR:  You must specify values for DELAY=, DOSE=, and TASK=;
		%goto done;
		%end;

%expgrid(n=&n, delay=&delay, dose=&dose, task=&task);

data &out;
	set _grid_;
	run;

*proc datasets nolist nowarn;
*	delete _grid_;

%done:
%mend;

options mprint;
%project2(n=4, delay=0 to 60 by 30, dose=0 to 3, task=Easy Hard);
proc print data=design;
