 /*--------------------------------------------------------------*
  *    Name: polygons.sas                                        *
  *   Title: Create Annotate dataset to draw a polygon(s)        *
        Doc: http://www.datavis.ca/sasmac/polygons.html    
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 20 Jul 2006 12:24:20                                *
  * Revised: 08 Nov 2006 11:51:06                                *
  * Version: 1.0                                                 *
  *  - Included %repeat macro inline                             *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The POLYGONS macro reads a dataset with specified X= and Y= vaariables
 and constructs and Annotate dataset to draw polygon(s) in a plot.

=Usage:

 The POLYGONS macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%polygons();
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* X=          Name of horizontal variable

* Y=          Name of vertical variable

* XSYS=       Annotate coordinate system for X= variable [Default: XSYS=2]

* YSYS=       Annotate coordinate system for Y= variable [Default: YSYS=2]

* CLASS=      Grouping variable, each a separate polygon

* COLOR=      Line color(s) for the groups (repeated as necessary) [Default: COLOR=BLACK]

* LINE=       Line style(s) for polygon outlines [Default: LINE=1]

* WIDTH=      Width(s) of polygon outlines [Default: WIDTH=1]

* FILLCOLOR=  Fillcolor(s) for the polygon(s)

* WHEN=       When to draw? B or A [Default: WHEN=A]

* IN=         Name of input Annotate data set, concatenated to OUT=

* OUT=        The name of the output data set [Default: OUT=POLY]
                

 =*/

/*
Construct Annotate dataset to draw a possibly filled set of polygons
*/
%macro polygons(
	data=_last_,  /* name of the input data set                           */
	x=,           /* name of horizontal variable                          */
	y=,           /* name of vertical variable                            */
	xsys=2,       /* Annotate coordinate system for X= variable           */
	ysys=2,       /* Annotate coordinate system for Y= variable           */
	class=,       /* grouping variable, each a separate polygon           */
	color=black,  /* line color(s) for the groups (repeated as necessary) */
	line=1,       /* line style(s) for polygon outlines                   */
	width=1,      /* width(s) of polygon outlines                         */
	fillcolor=,   /* fillcolor(s) for the polygon(s)                      */
	when=a,       /* when to draw? B or A                                 */
	in=,          /* name of input Annotate data set, concatenated        */
	out=poly      /* name of output Annotate data set                     */
	);


%local groups;
*-- Make sure there is a CLASS variable surrogate;
data &out;
	set &data end=eof;
	retain _gp_ 0;
	%if %length(&class)=0 %then %do;
		_gp_=1;
		%end;
	%else %do;
		by &class;
		if first.&class then _gp_+1;
		%end;   
	if eof then call symput('groups', left(put(_gp_, 3.)));
	run;

%if &groups > 1 %then %do;
	%let color=%repeat(&color, &groups);
	%*let fillcolor=%repeat(&fillcolor, &groups);
	%end;

data &out;
	set &out;
	by _gp_;
    xsys="&xsys"; ysys="&ysys";
	x = &x;
	y = &y;
	length function color $8;
	keep xsys ysys _gp_ x y function color style when line size;
    if first._gp_ then do;
		function='POLY';
		%if %length(&fillcolor) %then %do;
			when = "&when";
			style = 'solid';
			color = scan("&fillcolor", _gp_);
			%end;
		end;
    else do;
  	 function='POLYCONT';
      color=scan("&color",_gp_);
	  %if %length(%scan(&line, 2)) %then %do;
		  line=input(scan("&line", _gp_), 3.);
		  %end;
	   %else %do;
		  line = &line;
		  %end;
	  %if %length(%scan(&width, 2)) %then %do;
		  size=input(scan("&width", _gp_), 3.);
		  %end;
	   %else %do;
		  size = &width;
		  %end;
	end;

%if %length(&in) %then %do;
	data &out;
		set &out &in;
	%end;
%mend;

/*
Fill the area between two lines or curves, defined on
the same set of x values
*/
%macro fillbetween(
	data=_last_,  /* name of the input data set                           */
	x=,           /* name of horizontal variable                          */
	y1=,          /* name of vertical variable for 1st series             */
	y2=,          /* name of vertical variable for 2nd series             */
	color=black,  /* line color(s) for the groups (repeated as necessary) */
	line=1,       /* line style for outline                               */
	width=1,      /* line width for outline                               */
	fillcolor=gray,  /* fill color                                        */
	in=,          /* name of input Annotate data set, concatenated        */
	out=fill      /* name of output Annotate data set                     */
	);

	proc sort data=&data out=_up_;
		by &x;
	proc sort data=&data out=_dn_;
		by descending &x;
	data _join_;
		keep &x _y_;
		set _up_(in=up)
		    _dn_(in=dn);
		if up then _y_ = &y1;
			  else _y_ = &y2;
		run;

	%polygons(data=_join_, x=&x, y=_y_,
		color=&color, line=&line, width=&width,
		fillcolor=&fillcolor, in=&in, out=&out);

%if %length(&in) %then %do;
	data &out;
		set &out &in;
	%end;

  *-- Clean up datasets no longer needed;
	proc datasets nofs nolist nowarn library=work memtype=(data);
    	delete _up_ _dn_ _join_;
	run; quit;

%mend;

%*---  Repeat a string of words, until there are at least
    a given number of words;

%macro repeat(parm, len, dlm=%str( ));
	%do k=1 %to &len;
		%if %length(%scan(&parm, &k, &dlm))=0
			%then %let parm = &parm &parm;
		%end;
	&parm
%mend;

