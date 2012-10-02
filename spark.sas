/*
Originally from NOTE: Issue 18. October 2006

See also
http://bitworking.org/projects/sparklines/spark.cgi
*/

%macro spark(
    data=         /* Name of input data set */
   ,x=         /* Variable to plot on X-axis */
   ,y=         /* Variable to plot on Y-axis */
   ,cmin=         /* Colour of symbol displayed at minimum data point */
   ,cmax=         /* Colour of symbol displayed at maximum data point */
   ,clast=        /* Colour of symbol displayed at last data point */
   ,hmin=1        /* Height of symbol displayed at minimum data point */
   ,hmax=1        /* Height of symbol displayed at maximum data point */
   ,hlast=1       /* Height of symbol displayed at last data point */
   ,vmin=dot      /* Symbol displayed at minimum data point */
   ,vmax=dot      /* Symbol displayed at maximum data point */
   ,vlast=dot     /* Symbol displayed at last data point */
             /* Name and location of output graphics file */
   ,gdevice=gif   /* Output graphics device */
   ,hpixels=50    /* Length of spark line (pixels) */
   ,vpixels=11    /* Height of spark line (pixels) */
   ,outfile=spark.&gdevice
   ,tidy=y /* Delete temporary data sets prior to termination? */
   );

%* put **********************************************************************;
%* put Parameter values to be used by &sysmacroname are:;
%* put _local_;
%* put **********************************************************************;

/***********
/ Notes.
/ 1. The names of all temporary data sets are prefixed with the name of this
/ macro (&sysmacroname).
/ 2. All temporary data sets are deleted prior to termination of this macro
/ (conditional upon the value of the TIDY parameter).
/ 3. Expect the following messages:
/ NOTE: <nnn> observation(s) contained a MISSING value for the miny * <y> request.
/ NOTE: <nnn> observation(s) contained a MISSING value for the maxy * <y> request.
/ NOTE: <nnn> observation(s) contained a MISSING value for the lastair * <y> request.
/
************/

/************
/ STEP 1. Put minimum/maximum points into separate data
/ sets (one row in each). This is done regardless
/ of whether the information is actually required.
************/
data _&sysmacroname._min (keep=x4miny miny)
     _&sysmacroname._max (keep=x4maxy maxy)
     ;
  set &data end=finish;
 
  retain minx maxx;
  if _n_ eq 1 or &x lt minx then
  do;
    minx = &x;
  end;
  if _n_ eq 1 or &x gt maxx then
  do;
    maxx = &x;
  end;
 
  retain x4miny miny;
  if _n_ eq 1 or &y lt miny then
  do;
    x4miny = &x;
    miny = &y;
  end;
 
  retain x4maxy maxy;
  if _n_ eq 1 or &y gt maxy then
  do;
    x4maxy = &x;
    maxy = &y;
  end;
 
  if finish then
  do; /* Save and then write-out the values for informational purposes */
    OUTPUT;
    call symput('MINx',compress(minx,'BEST.'));
    call symput('MAXx',compress(maxx,'BEST.'));
    call symput('MINy',compress(miny,'BEST.'));
    call symput('MAXy',compress(maxy,'BEST.'));
  end;
run;
%put &sysmacroname: MINx=&minx, MAXx=&maxx;
%put &sysmacroname: MINy=&miny, MAXy=&maxy;

/************
/ STEP 2. Merge the min and max information with the actual
/ plot data.
/ Output data set will contain five columns: the
/ X and Y variables, plus variables for min, max,
/ and last.
/ This is all done regardless of whether the
/ information is actually required.
************/
data _&sysmacroname._data_minmaxlast;
  merge &data (keep=&x &y)
       _&sysmacroname._min(rename=(x4miny=&x))
       _&sysmacroname._max(rename=(x4maxy=&x))
        end=finish
        ;
  by &x;
  if finish then
    last&y=&y;
run;

/************
/ STEP 3. Are any specific points actually required? This
/ is interpreted from the fact that no colour
/ was specified.
/ For those that are not required, the plot
/ symbol is set to NONE.
************/
%if %length(&cmin) eq 0 %then %let vmin = NONE;
%if %length(&cmax) eq 0 %then %let vmax = NONE;
%if %length(&clast) eq 0 %then %let vlast = NONE;

/************
/ STEP 4. Produce the plot.
/ Need to set goptions, then, specify name and
/ location of output graphics file, then specify
/ invisible axes, then specify symbols for the
/ line (always visible) and the min/max/last points
/ (conditionally visible).
************/
goptions reset=all
         device=&gdevice
         hsize=&hpixels.pt vsize=&vpixels.pt
         gaccess=gsasfile
         ;
filename gsasfile "&outfile";
axis1 order=(&minx &maxx) label=none value=none major=none minor=none color=white;
axis2 order=(&miny &maxy) label=none value=none major=none minor=none color=white;
symbol1 c=black i=join v=none ;
symbol2 c=&cmin h=&hmin v=&vmin;
symbol3 c=&cmax h=&hmax v=&vmax;
symbol4 c=&clast h=&hlast v=&vlast;
proc gplot data=_&sysmacroname._data_minmaxlast;
  plot (&y miny maxy last&y) * &x / overlay
       noframe
       haxis=axis1
       vaxis=axis2
       ;
run; quit;
filename gsasfile clear;

/************
/ STEP 5. Conditionally delete temporary data sets.
************/
%if %upcase(%substr(&tidy,1,1)) eq Y %then
%do;
  proc datasets lib=work nolist;
  delete _&sysmacroname._:;
  quit;
%end;

%mend spark;

*options mprint;
*spark(data=sashelp.air
      ,x=date
      ,y=air
      ,cmin=red
      ,cmax=lime
      ,clast=orange
      ,outfile=c:\temp\spark.&gdevice
      ,tidy=n
      );
