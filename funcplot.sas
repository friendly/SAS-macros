/*
Andrews function plot
*/

%macro funcplot(
	data=_last_,
	var=,
	id=,
	inc= 2*pi/100,
	m=,
	s=,
	anno=,
	out=funcplot,
	);

%let nv = %numwords(&var);
%put NV = &nv;

%if %length(&m) > 0 or %length(&s) > 0 %then %do;
	proc standard data=&data out=_temp_
	%if %length(&m) > 0 %then m = &m;
	%if %length(&s) > 0 %then s = &s;
	;
	var &var;
	%let data = _temp_;
%end;

data &out;
	length _id_ $20;
	set &data;
	keep z t _id_;
	_id_ = &id;
	
	*--apply the orthonormal functions of t from -pi to pi;
	pi=3.14159265;
	s=1/sqrt(2);
	inc=&inc;
	do t=-pi to pi by inc;
		z = %scan(&var,1, %str( ));
		%let fn = sin;
		%do i = 2 %to &nv;
			%let v = %scan(&var, &i, %str( ));
			z = z + &fn( &i * t) * &v;
			%if &fn = sin
				%then %let fn = cos;
				%else %let fn = sin;
			%end;
		output;
		end;
run;

proc gplot data=&out 
	%if %length(&anno)>0 %then annotate=&anno;
	 ;
	plot z * t = _id_
		/ vaxis=axis1 vminor=1 frame hminor=1
		haxis=axis2 nolegend;
   axis1 label=none offset=(2,2);
   axis2 order=(-3.14 to 3.14 by 1.57)
	/*
	      major=(t=1 font=greek '-p'  t=2 font=greek '-p/2' t=3 '0'
			       t=4 font=greek 'p/2' t=5 font=greek 'p')
	*/
	  		label=none offset=(2);
	run; quit;
%mend;

%macro numwords(lst);
   %let i = 1;
   %let v = %scan(&lst,&i);
   %do %while (%length(&v) > 0);
      %let i = %eval(&i + 1);
      %let v = %scan(&lst,&i);
      %end;
   %eval(&i - 1)
%mend;
