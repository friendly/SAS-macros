 /*--------------------------------------------------------------*
  *    Name: cellipse.sas                                        *
  *   Title: Generate bivariate normal confidence ellipses       *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/nodoc.html      *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 16 Jul 98 10:06                                     *
  * Revised: 16 Jul 98 10:06                                     *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
	The CELLIPSE macro generates confidence ellipses for bivariate normal
	data. It can either create ellipses for the data or ellipses about the
	mean. 

=Usage:

 The cellipse macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
 %cellipse(x=height, y=weight);
 
==Parameters:

* DATA=		Specifies the name of the input data set to be analyzed.

* X=        Variable to be plotted on the horizontal axis.

* Y=        Variable to be plotted on the vertical axis.

* GROUP=
* BY=       Name of a group BY-variable.  One ellipse is produced for
            each group

* ABOUT=DATA|MEAN
            Specifies if you want the confidence ellipse on the DATA
			   or on the MEAN.

* OUT=		Specifies the name of the output data set

* CONF=     1-Alpha confidence level for the  ellipse to be computed.

* OUT=      Name of an output annotate data set containing the 
			   instructions to draw the ellipse.

* COLORS=    Color(s) used to plot the ellipse

 =*/

%macro cellipse(
	data=_LAST_,
	out=ellipse,
	x= ,
	y= ,
	group=,
	by=,
	about=data,
	conf=.95,
	line=1,
	colors=RED BLUE GREEN BLACK PURPLE YELLOW BROWN ORANGE,
	name=ellipse
	);

%if %length(&group)=0 %then %let by=&group;
%if %length(&by) %then %do;
proc sort data=&data;
	by &by;
	%end;

*-- Calculate covariance matrix;
proc corr data=&data cov outp=_corr_ noprint;
	var &x &y;
	%if %length(&by) %then %do;
		by &by;
		%end;
	run;

data _obs_(keep=&by n);
	set _corr_;
	where (_type_='N');
	n=&x;

data _mean_(keep=&by meanX meanY);
	set _corr_;
	where (_type_='MEAN');
	meanX=&x;
	meanY=&y;

*-- Extract variances and covariance;
data _choles_;
	set _corr_ end=eof;
	%if %length(&by) %then %do;
		by &by;
		%end;
	where (_type_='COV');
	retain varX cov varY;
	keep &by varX cov varY;
	if _name_ = upcase("&x") then do ;
		varX=&x;
		cov=&y;
		end;
	if _name_ = upcase("&y") then varY=&y;
	%if %length(&by) %then %do;
		if last.&by then output;
		%end;
	%else %do;
		if eof then output;
	%end;

data _choles_(keep=&by a b c meanX meanY n);
	merge _choles_ _mean_ _obs_;
	%if %length(&by) %then %do;
		by &by;
		%end;
	a=sqrt(varX);
	b=cov/a;
	c=sqrt(varY-(b**2));

*-- Compute points around the confidence ellipse;
data &out;
	set _choles_ end=eof;
	%if %length(&by) %then %do;
		by &by;
		if first.&by then _gp_+1;
		%end;
	%else %do;
		_gp_=1;
	%end;
	keep x y function color line size xsys ysys _gp_;
	retain xsys ysys '2';
	length function color $8;

	line = &line;
	size = 1;
	color = scan("&colors &colors &colors",_gp_);
	conf = &conf;
	if conf > 1 then conf = conf/100;

	%if %upcase(&about)=DATA %then %do;
		d = sqrt(cinv(conf,2));
		%end;
	%else %do;
		d = sqrt(((n-1)*2*finv(conf,2,n-2))/(n*(n-2)));
		%end;

	do t= 0 to 360 by 5;
		sin=sin(t*atan(1)/45);
		cos=cos(t*atan(1)/45);
		x = meanX + (a*d*cos);
		y = meanY + b*d*cos+(c*d*sin);
      If t=0 then function = 'MOVE';
             else function = 'DRAW';
		output;
		end;
	if eof then do;
		call symput('ngps', put(_gp_,3.));
		end;
	run;

*-- Find min, max points to make sure the ellipse is included in the
    plot range;
proc means noprint data=&out;
	var x y;
	output out=_minmax_ min=minx miny max=maxx maxy;

data _minmax_;
	set _minmax_;
	keep &x &y;
	&x = minx;  &y = miny; output;
	&x = maxx;  &y = maxy; output;

*-- Add a _GP_ variable to the data set;
data _plot_;
	set &data;
	keep &x &y _gp_;
	%if %length(&by) %then %do;
		by &by;
		if first.&by then _gp_+1;
		%end;
	%else %do;
		_gp_= 1;
	%end;

*-- Add min/max points to the dataset;
data _plot_;
	set _plot_ _minmax_(in=in2);
	if in2 then _gp_ = &ngps +1;
*proc print;

*-- Generate sumbol statements for the groups;
%gensym(n=&ngps, colors=&colors);
*-- symbol statement for phantom ellipse min/max points;
%let m = %eval(&ngps+1);
symbol&m i=none v=none;

proc gplot data=_plot_;
	plot &y * &x = _gp_ / 
		nolegend frame anno=&out vaxis=axis1
		name="&name"
		des="CELLIPSE plot of &y * &x";
	axis1 label=(a=90);
*	symbol1 i=none v=square color=black;
*	symbol2 i=none v=none   color=red;
	run; quit;
	
goptions reset=symbol;
%done:

%mend;

