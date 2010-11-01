%macro starxy(
	data=,
	var=,             /* list of variables, as ordered around the star  */
	x=,               /* X location for the center of the star          */
	y=,               /* Y location for the center of the star          */
	where=,           /* where clause to select observations            */
	keyxy=,           /* X, Y location for variable key                 */
	minray=0.1,       /* minimum ray length                             */
	scale=&minray 1,  /* leave empty if pre-scaled                      */
    rayline=1,        /* Line style(s) of rays                          */
    raythick=1,       /* Line thickness of rays                         */
    a0=0,             /* Angle offset - angle for first ray             */
    color='BLACK',    /* star color, quoted string or variable name     */
    cfill='',         /* Background color                               */
    missing=0,        /* value assigned to missing data, or . to delete */
	radius=10,        /* star radius                                    */
	circle=,
	keyradius=1.4*&radius,
    out=stars         /* name of output data set                         */         
	);

data _null_;
   array p{*} &var ;
   point=1;
   set &data point=point nobs=nobs;
   k = dim(p);
   call symput('NV'  , left(put(k, 2.)));
   stop;     /* Don't forget this ! */

proc univariate noprint data=&data;
	%if %length(&where) >0 %then %do;
		where &where;
		%end;
   var &var;
   output out=_range_ min=mn1-mn&nv max=mx1-mx&nv;
run;

 /*----------------------------------------------------*
  |  Scale each variable to range from MINRAY to 1.0   |
  *----------------------------------------------------*/
%if %length(&scale) %then %do;
%let _scaled_=_scaled_;
data _scaled_;
   set &data;
	%if %length(&where) >0 %then %do;
		where &where;
		%end;
   if _n_=1 then set _range_;
	drop i keep mn1-mn&nv mx1-mx&nv;
   array vars[*] &var;
   array mn[*]  mn1-mn&nv ;
   array mx[*]  mx1-mx&nv ;
	keep = 1;
   do i = 1 to &nv;
		if vars[i] = .
			then do; vars[i] = &missing; keep=&missing; end;
			else vars[i] = &minray + (1-&minray)*(vars[i]-mn[i])/
								(mx[i]-mn[i]);
      end;
	if keep = . then delete;
	
  %put STARXY: &DATA dataset variables scaled to range &MINRAY to 1;
%end;
%else %let _scaled_=&data;
 /*---------------------------------------------------*
  |  Text POSITIONs corresponding to rays of varying  |
  |  angle around the star                            |
  *---------------------------------------------------*/
proc format;
   value posn     0-22.5 = '6'  /* left, centered  */
               22.6-67.5 = 'C'  /* left, above     */
               67.6-112.5= '2'  /* centered, above */
              112.6-157.5= 'A'  /* right, above    */
              157.6-202.5= '4'  /* right, centered */
              202.6-247.5= 'D'  /* right, below    */
              247.6-292.5= 'E'  /* centered, below */
              other='F';        /* left, below     */
run;
data &out;
     length function color $8 varname $12;
     array p[&nv] &var ;
 
     retain s1-s&nv c1-c&nv;
     array s[&nv]  s1-s&nv;              /* sines of angle      */
     array c[&nv]  c1-c&nv;              /* cosines of angle    */
     drop s1-s&nv c1-c&nv &var x0 y0  k save;
     drop varname showvar r ang a0 ;

     if _n_=1 then do;
        *--- precompute ray angles;
		  a0 = &a0 * (3.1415926/180);
        do k= 1 to &nv;
           ang = a0 + (2 * 3.1415926 * (k-1)/&nv);
           s[k] = sin( ang );
           c[k] = cos( ang );
           p[k] = 1.0;
        end;

	%if %length(&keyxy) %then %do;
		x0 = %scan(&keyxy,1,%str( ));
		y0 = %scan(&keyxy,2,%str( ));
		r = &keyradius;
     	showvar=1;
		link DrawStar;                 /* Do variable key  */
		%end;
	
     end;

     set &_scaled_ end=lastobs;
     showvar=0;
     r = &radius;
	 x0 = &x;
	 y0 = &y;
	 link origin;
     link drawstar;
	 %if %length(&circle) %then %do;
	 	circline=&circle;
		link circle;
	 	%end;
     return;
	 
Origin:
	xsys='2'; ysys='2';
	x = x0; y = y0; function='MOVE'; comment='Origin  ';
	output;
	return;
	
DrawStar:
     *-- Draw star outline;
	xsys='2'; ysys='2'; comment='Outline';
     do k = 1 to &nv;
        x = x0 + p[k] * r * c[k];
        y = y0 + p[k] * r * s[k];
        if k=1 then do;
          %if %length(&cfill)>0 %then %do;
				color = &cfill;
				if color ^=' ' then style = 'solid';
			 	%end;
		  	function = 'POLY';
			end;
        else do;
			color = &color;
			function = 'polycont';
			end;
        output;
     end;
 
     *-- draw rays from center to each point;
     *-- label with the variable name if showvar=1;
	 comment='Rays';
     do k = 1 to &nv;
        x=x0; y=y0;
        function='MOVE';   output;
        x = x0 + p[k] * r * c[k];
        y = y0 + p[k] * r * s[k];
*		  line = l{1+mod(k-1, &nl)};
		  line = &rayline;
		  save = size; size = &raythick;
        function = 'DRAW'; output;
		  size=save;

        if showvar = 1 then do;
           ang = a0 + (2 * 3.1415926 * (k-1)/&nv);
           varname= ' ';
           call vname(p[k],varname) ;
           text = trim(left(varname));
			  text = substr(text,1,1) || lowcase(substr(text,2));
           position = left(put(180*ang/3.14159,posn.));
           function = 'LABEL';  output;
        end;
     end;
  return;

Circle:
	comment='Circle';
	line=circline;
	r = &radius;
	style='empty';
	do k=1 to 40;
		x = x0 + r * cos(2 * 3.1415926 * (k-1)/40);
		y = y0 + r * sin(2 * 3.1415926 * (k-1)/40);
		if k=1 
			then function='poly';
			else function='polycont';
		output;
		end;
	return;
run;                         /* Force SAS to do it (DONT REMOVE)  */
%mend;

