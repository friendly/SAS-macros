 /*--------------------------------------------------------------*
  *    Name: sparkline.sas                                       *
  *   Title: Draw sparklines, with control of aspect ratio       *
        Doc: http://www.datavis.ca/sasmac/sparkline.html   
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 14 Mar 2008 11:23:15                                *
  * Revised: 22 Apr 2008 10:01:25                                *
  * Version: 1.0-0                                               *
  * Original from:  Davis, K., Sparklines using SAS and JMP,     *
  *                 NESUG 2007                                   *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The SPARKLINE macro draws sparklines, described by Tufte as "small, high resolution 
 graphics embedded in a context of words, numbers, images."  The essential idea is
 to draw a minimal, small graph that can be used inline in text and tables.
 The typical usage is for time-series data.
 
 The SPARKLINE macro is designed so that, if desired, the aspect ratio (height/width)
 of the resulting small graphic can be controlled so that successive line segments
 have an average slope of 45 degrees in the plot ("banking to 45 degrees").

 A second macro, SPARKBAR, is designed similarly, but uses PROC GCHART rather than
 PROC GPLOT to produce the graph.

  
=Usage:

 The SPARKLINE macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%sparkline(data=rulers, y=years, x=order)
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* Y=          Vertical variable for the plot

* X=          Horizontal variable for the plot

* HEIGHT=     Height of the sparkline [Default: HEIGHT=12]

* WIDTH=      Desired width, or width=AUTO to bank to 45 degrees [Default: WIDTH=AUTO]

* UNIT=       Unit for height and width [Default: UNIT=PT]

* HAXIS=      Name of axis statement for horizontal axis

* COLOR=      Color of the line [Default: COLOR=GRAY]

* ANNO=       Mark first/last/min/max specially? ANNO=DOT adds graphic annotations
              showing the first, last, minimum and maximum points by special
			  symbols, as controlled by the parameters VMIN=, VMAX= and so forth
			  described below. [Default: ANNO=NONE]

* VMIN=       Symbol displayed at minimum data point [Default: VMIN=DOT]

* VMAX=       Symbol displayed at maximum data point [Default: VMAX=DOT]

* VLAST=      Symbol displayed at first/last data point [Default: VLAST=DOT]

* CMIN=       Colour of symbol displayed at minimum data point [Default: CMIN=RED]

* CMAX=       Colour of symbol displayed at maximum data point [Default: CMAX=RED]

* CLAST=      Colour of symbol displayed at first/last data point [Default: CLAST=GREEN]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=SPARK]

* GOUT=       Name of the graphic catalog in which the graph is written [Default: GOUT=GSEG]
                
=References:

* Davis, K., Sparklines using SAS and JMP, NESUG 2007
    http://www.nesug.org/Proceedings/nesug07/sa/sa06.pdf
* Tufte, E: "Beautiful Evidence". Graphics Press, 2006
  
=Examples:

  %include data(rulers);
  axis1 order=(0 to 45 by 5) length=95 pct;
  %sparkline(data=rulers, y=years, x=order, anno=DOTS, haxis=axis1);

  %sparkbar(data=rulers, y=years, x=order, color=greyB0);

 =*/


%macro sparkline (
	data=_last_,    /* name of input data set                              */
	y=,             /* vertical variable for the plot                      */
	x=,             /* horizontal variable for the plot                    */
	height=12,      /* height of the sparkline                             */
	width=AUTO,     /* desired width, or width=AUTO to bank to 45 degrees  */ 
	unit=pt,        /* unit for height and width                           */
	haxis=,         /* name of axis statement for horizontal axis          */
	color=gray,     /* color of the line                                   */
	anno=NONE,      /* mark first/last/min/max specially?                  */
    vmin=dot,       /* Symbol displayed at minimum data point              */
    vmax=dot,       /* Symbol displayed at maximum data point              */
    vlast=dot,      /* Symbol displayed at first/last data point           */
    cmin=red,       /* Colour of symbol displayed at minimum data point    */
    cmax=red,       /* Colour of symbol displayed at maximum data point    */
    clast=green,    /* Colour of symbol displayed at first/last data point */
	name=spark,
	gout=gseg
	);

%local me abort;
%let abort=0; %let me=&sysmacroname;
%if %length(&x)=0 or %length(&y)=0 %then %do;
	%put ERROR: &me: X= and Y= variables must be specified;
	%let abort=1;
	%goto DONE;
	%end;

%gbank(data=&data, y=&y, x=&x, out=__stats, height=&height, width=&width, gunit=&unit);
data _null_;
   set __stats;
   call symput('miny',miny);
   call symput('maxy',maxy); 
   call symput('width',round(width));
   run;
	%*end;

%*put &me: Setting height=&height width=&width;


%if %upcase(&anno)=DOTS %then %do;
   data __anno; *Create dots;
   set __sorted (keep=&x &y) end=last;
   by &x.;
   retain function "SYMBOL" when "A"  xsys ysys '2' hsys '1' ;
   size = 1.5*&height;
   if _n_=1 or last then do;
		text="&vlast";
      	x=&x.; y=&y.; 
		color="&clast"; 
		output; 
	  end;
   if &y = &maxy then do;
      x=&x.; y=&y.; text="&vmax"; color="&cmax"; output; end;
   if &y = &miny then do;
      x=&x.; y=&y.; text="&vmin"; color="&cmin"; output; end;
   run;
   proc print;
   %end;

%else %do;
*-- create a dummy anno data set;
data __anno; run;
	%end;

goptions noborder /*RESET=ALL vsize=&height &unit hsize=&width &unit */;
%if %length(&haxis)=0 %then %do;
	axis99 length=95 pct;
	%let haxis=axis99;
	%end;

%if &gout^=%str()  %then %let gout=GOUT=&gout;
symbol1 interpol=join value=none width=0.5 color=&color;
proc gplot data=__sorted &gout;
   plot &y *&x  / noaxis noframe  
    		annotate=__anno haxis=&haxis
			name="&name" description="sparkline of &y * &x"
			;
   run;
quit;

*-- Delete temporary data sets;
  proc datasets lib=work nolist;
  delete __stats __anno  __sorted;
  quit;

%done:

%mend sparkline;

%macro sparkbar(
	data=_last_,    /* name of input data set                              */
	y=,            /* vertical variable for the plot                      */
	x=,            /* horizontal variable for the plot                    */
	height=12,      /* height of the sparkline                             */
	width=AUTO,     /* desired width, or width=AUTO to bank to 45 degrees  */ 
	unit=pt,        /* unit for height and width                           */
	color=gray,
	name=sparkb,
	gout=gseg
	);

%let abort=0; %let me=&sysmacroname;
%if %length(&x)=0 or %length(&y)=0 %then %do;
	%put ERROR: &me: X= and Y= variables must be specified;
	%let abort=1;
	%goto DONE;
	%end;

%gbank(data=&data, y=&y, x=&x, out=__stats, height=&height, width=&width, gunit=&unit);
data _null_;
   set __stats;
   call symput('width',round(width));
   run;

%put &me: Setting height=&height width=&width;

goptions noborder /*RESET=ALL*/ vsize=&height &unit hsize=&width &unit;
pattern value=solid color=&color;

%if &gout^=%str()  %then %let gout=GOUT=&gout;
proc gchart data=&data.&gout;
   vbar &x / sumvar=&y discrete noaxis noframe name="&name";
   run;
quit;

*-- Delete temporary data sets;
  proc datasets lib=work nolist;
  delete __stats  __sorted;
  run; quit;

%done:
%mend sparkbar;

