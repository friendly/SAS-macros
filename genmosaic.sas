/* 
  Fit a loglinear model with PROC GENMOD and produce a mosaic display
*/

%macro genmosaic(
	data=&syslast,
	class=,
	count=count,
	model=,
	order=data,
	resid=,
	title=
	);

proc genmod data=&data order=&order;
     class &class;
     model &count = &model / link=log dist=poisson obstats;
	 ods output outstat=_results_
run;

%mosaic(data=_results_, var=&class, count=&count, resid=resid
	title=&title);

%mend;

