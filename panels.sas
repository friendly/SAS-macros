 /*-------------------------------------------------------------------*
  *    Name: panels.sas                                               *
  *   Title: Macro to display a set of plots in rectangular panels    *
  *           using PROC GREPLAY.                                     *
        Doc: http://www.datavis.ca/sasmac/panels.html              
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *         
  * Created:  1 Mar 1994 13:16:36                                     *         
  * Revised: 18 Jan 2006 10:54:42                                     *         
  * Version:  1.8-2                                                   *
  *  - EQUATE default changed to Y                                    *
  *  - Added ORDER=BYROWS support                                     *
  * 1.7 Added %replay macro to construct replay lists                 *
  *  - Added SHAPE= parameter for LOWTRI, UPTRI displays              *
  *  - Fixed buglet with FIRST=                                       *
  * 1.8 Fixed bugs and problems with TOP=, but still not too general  *
  *  - Added TOPNAME= and TOPHEIGHT= to control the top panel         *
  *  - Added NAME= option to TREPLAY statement                        *
  *  - Fix small bug for many plots (thx: Rob Hall)                   *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/

/*=
=Description:

 The PANELS macro constructs a template in which to replay a series of
 graphs, assumed all the same size, in a rectangular array of R
 rows and C columns. By default, the panels are displayed
 left-to-right across rows,  starting either from the top
 (ORDER=DOWN) or bottom (ORDER=UP).  If the number of rows and
 columns are unequal, the aspect ratio of individual panels can
 be maintained by setting equate=Y.  It is assumed that all the
 plots have already been created, and stored in a graphics catalog
 (the default, WORK.GSEG is used automatically by SAS/GRAPH
 procedures).

 For interactive use within the SAS Session Manager you should be
 aware that all plots are stored cumulatively in the graphics
 catalog throughout your session, unless explicitly changed with
 the GOUT= option in graphics procedures or macros  To create multiple
 panelled plots you can use the FIRST= and LAST= parameters or a REPLAY=
 list to specify which plots are used in a given call.
   
=Usage:

 Call the PANELS macro after the steps which create the graphs in the
 graphics catalog.  The GDISPLA macro may be used to suppress the
 display of the original full-sized graphs.
 The ROWS= and COLS= parameters must be specified.
 
   goptions hsize=7in vsize=5in;
	%gdispla(OFF);
	proc gplot data=mydata;
		plot y * x = group;
		by sex;
	
	%gdispla(ON);
	%panels(rows=1, cols=2);
	
==Parameters:
   
* ROWS=

* COLS=   The ROWS= and COLS= arguments are required, and specify the
   size of the array of plots to be displayed. These are the only
   required arguments.
   
* PLOTS=   If there are fewer than &ROWS*&COLS plots, specify the number
   as the PLOTS= argument. Optionally, there can be an additional
   plot, which is displayed (as a GSLIDE title, for example) in
   the top nn% of the display, as specified by the TOP= argument.
   
* TOP=   If TOP=nn is specified, the top nn% of the display is reserved
   for one additional panel, to serve as the plot
   title or annotation. The other panels are replayed in the bottom
   100-&top percent of the display. The vertical size of the top panel is 
   determined by the TOPHEIGHT= argument.  

* TOPNAME=	Specifies the plot name or number of the graph to be used as the
	top panel.  If not specified, the macro uses the next graph after
	the last one from the GIN catalog used in filling the replay list,
	i.e., %eval(&last+&plots-1). 

* TOPHEIGHT=	Specifies the vertical size of the panel used to replay the top
	panel.  If you use PROC GSLIDE to create an overall title for the
	graph, use the default TOPHEIGHT=100.  If you want to use a graph
	rescaled to fit entirely within the top panel (i.e., with
	TOPHEIGHT=&TOP), that graph should be created with an aspect ratio
	to match the shape of the desired top panel; otherwise it will be
	deformed to fit.

* ORDER=   The ORDER= argument specifies the order of the panels in the
   REPLAY= list, when REPLAY= is not specified. 
	Typically, the panels are displayed across the columns, starting
	in the top row.
	ORDER=UP means that the panels in the bottom row are
	are drawn first, and numbered 1, 2, ..., &COLs. ORDER=DOWN means
	that the panels in the top row are drawn first, numbered 1, 2, ..., 
	&COLs.  If you add the keyword BYROWS to ORDER=, the panels are
	displayed up or down the rows.  For example, when ROWS=3, COLS=5,
	ORDER=DOWN BYROWS generates the REPLAY= list as,

       replay=1:1  2:4  3:7  4:10  5:13
              6:2  7:5  8:8  9:11 10:14
             11:3 12:6 13:9 14:12 15:15
   
* EQUATE=	The EQUATE= argument determines if the size of the panels is adjusted
	so that the aspect ratio of the plots is preserved.  If EQUATE=Y,
	the size of each plot is adjusted to the maximum of &ROWS and &COLS.
	This is usually desired, as long as the graphic options HSIZE and
	VSIZE are the same when the plots are replayed in the panels
	template as when they were originally generated.  The default is
	EQUATE=Y.
	
* REPLAY=   The REPLAY= argument specifies the list of plots to be replayed
   in the constructed template, in one of the forms used with the
   PROC GREPLAY REPLAY statement, for example, REPLAY=1:1 2:3 3:2
   4:4 or REPLAY=1:plot1 2:plot3 3:plot2 4:plot4.  If TOP= is used
   for a top panel, that graph should be replayed in panel 0.
   
* TEMPLATE=   The name of the template constructed to display the plots. The
	default is TEMPLATE=PANEL&ROWS.&COLS.

* TC=   The name of the template catalog used to store the template.
	You may use a two-part SAS data set name to save the template
	permanently.

* FIRST=

* LAST=   By default, the REPLAY= argument is constructed to replay plot
   i in panel i.  If the REPLAY= argument is not specified, you can
   override this default assignment by specifying FIRST= the sequential
   number of the first graph in the graphics catalog to plot (default:
   first=1), where:                       
		>0 means absolute number of first graph,          
		<1 means number of first graph relative to last (i.e. 0 means 
		 last graph only, -1 means first is one before last, etc.) 
		                            
   last=0          Number of last graph to plot                        
		>0 means absolute number of last graph,           
		<1 means number of last graph relative to number  
		of graphs in the catalog (i.e. 0 means last graph 
		in the catalog, -1 means one before last, etc.)

* SHAPE=   Ordinarily, the graphs are replayed in a rectangular template (SHAPE=RECT),
   but for some applications graphs are generated as the lower or upper triangle
   of a square matrix, with or without the diagonal.  Specify SHAPE=
   LOWTRI(D) or UPTRI(D) for these cases.

* GIN=  	GIN specifies the name of the input graphics catalog, from which the
	plots to be replayed are taken. The default is GIN=WORK.GSEG.

* GOUT= 	GOUT= specifies the name of the graphics catalog in which the panelled
	plot is stored. The default is GOUT=WORK.GSEG.

* NAME=       The name of the replayed graph in the GOUT= catalog
              [Default: NAME=PANELS]

=Notes:

 To use PANELS within another macro (such as SCATMAT), it is best to generate
 the individual graphs to a temporary graphics catalog rather than to the 
 same catalog used for the replayed graphs.


 =*/

%macro panels(
   rows=,                 /* number of rows of plots                    */
   cols=,                 /* number of columns of plots                 */ 
   plots=&rows * &cols,   /* total number of plots                      */
   top=0,                 /* percent of display for top panel           */
   topname=,              /* name or number of top panel                */
   topheight=100,         /* height of top panel                        */ 
   order=UP,              /* start at bottom (UP) or top (DOWN)?        */
   replay=,               /* List of plots to replay                    */
   equate=Y,              /* Adjust sizes to maintain aspect ratio?     */ 
   template=panel&rows.&cols,   /* name of template                     */
   tc=panels,             /* name of template catalog                   */
   first=1,               /* number of the first catalog entry used     */
   last=0,                /* number of the last catalog entry used      */
   shape=rect,            /* shape of the template.  See the REPLAY macro */
   gin=gseg,              /* name of input graphic catalog              */
   gout=gseg,             /* name of output graphic catalog             */
   name=panels            /* name of replayed graph in output catalog     */
   );

%local i j panl panl1 lx ly tx ty sx sy by;
%*put PANELS: plots= &plots;

%let order=%upcase(&order);
%let equate=%substr(%upcase(&equate),1,1);

%let abort=0;
%let showgout=0;
%if &rows=%str() | &cols=%str()
	%then %do;
		%put ERROR: The ROWS= and COLS= parameters must be specified;
      %let abort=1;
		%goto DONE;
	%end;

%if %length(&top)=0 %then %let top=0;
	
  /* determine how many graphs are in the catalog */
%let npics=0;
  proc catalog catalog=&gin et=grseg;
  	contents out=_cont_;
	run;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
  data _null_; 
   set _cont_ end=last; 
	if (last) then call symput('npics',trim(left(put(_n_,5.)))); 
	run;
%put PANELS: The graphics catalog &gin contains &npics graphic entries;
%if &npics=0 %then %goto DONE;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

  %if (&last<1) %then %do;
    %if (%eval(&npics+&last)>=1) %then %let last = %eval(&npics + &last);
    %else %do;
      %put NOTE (panels): last=&last too low - setting last=1.;
      %let last = 1;
    %end;
  %end;
  %else %if (&last>&npics) %then %do;
    %put WARNING: last=&last too high - setting last=&npics.;
    %let last = &npics;
  %end;

  %if (&first>&last) %then %do;
  	%put WARNING: first=&first too high - no plots done.;
	%goto done;
	%end;

  %if (&first<1) %then %do;
      %if (%eval(&last+&first)>=1) %then %let first = %eval(&last + &first);
      %else %do;
        %put WARNING: first=&first too low - setting first=1.;
        %let first = 1;
		  %let showgout=1;
      %end;
  %end;

    %put PANELS: plotting entries &first to &last .... to catalog &gout;
  

%*-- If the replay list is not given, construct it, ordering the
	plots across each row, but starting at either the bottom row
	(order=UP) or the top row (order=DOWN);
	
%if &replay=%str() %then %do;
	%replay(rows=&rows, cols=&cols, shape=&shape, first=&first, last=&last);

	%if &top>0 %then %do;
		%if %length(&topname)
			%then %let replay = 0:%eval(&first+&plots+1-1) &replay ;
			%else %let replay = 0:&topname  &replay ;
		%end;
/*
		%then %let replay = &replay %eval(&plots+1):%eval(&first+&plots+1-1);
*/
	%put PANELS: replay=&replay;
	%end;
	
%*-- Calculate panel size and starting location;
data _null_;
   %if &top>0
      %then %do;  ty=100-&top; %end;
      %else %do;  ty=100;      %end;
   tx=100;
	%if &equate=N %then %do;
		hsize = tx/&cols;
		vsize = ty/&rows;
		sx = 0; sy = 0;
	%end;
	%else %do;
		np = max(&rows,&cols);
		hsize = round(tx/np,.01);
		vsize = round(ty/np,.01);
		sx = round((np - &cols) * hsize/2,.01);
		sy = round((np - &rows) * vsize/2,.01);
	%end;
	lx = round(hsize + sx,.01);
	ly = round(vsize + sy,.01);
*	put hsize= vsize= tx= ty= sx= sy= lx= ly=;

	%if %index(&order,DOWN) %then %do;
		ly = round(ty - sy,.01);
		sy = ly - vsize;
		vsize = -vsize;
	%end;			

	put 'PANELS: ' hsize= vsize= tx= ty= sx= sy= lx= ly=;
   call symput('hsize', put(hsize,6.2));
   call symput('vsize', put(vsize,6.2));
   call symput('lx', put(lx,6.2));
   call symput('ly', put(ly,6.2));
   call symput('tx', put(tx,6.2));
   call symput('ty', put(ty,6.2));
   call symput('sx', put(sx,6.2));
   call symput('sy', put(sy,6.2));
run;

*-- calculate bottom y of top panel;
%if &top>0 %then %do;
	%if &topheight=100 
		%then %let by=0;
		%else %let by=%eval(100-&topheight);
	%end;

proc greplay igout=&gin
              gout=&gout  nofs
             template=&template
             tc=&tc ;

%* ---------------------------------------------------------------;
%* Generate a TDEF statement for a plot matrix                    ;
%* Start with (1,1) panel in lower left, and copy it across & up  ;
%* ---------------------------------------------------------------;
   TDEF &template DES="panels template &rows x &cols"
   %let panl=0;
   %do i = 1 %to &rows;
   %do j = 1 %to &cols;
		 %if &panl > %eval(&plots) %then %goto fini;
       %let panl  = %eval(&panl + 1);
       %if &j=1 %then
          %do;
             %if &i=1 %then %do;      %* (1,1) panel;
               &panl/
                ULX=&sx  ULY=&ly     URX=&lx  URY=&ly
                LLX=&sx  LLY=&sy     LRX=&lx  LRY=&sy
                %end;
             %else
                %do;                  %* (i,1) panel;
                   %let panl1 = %eval(&panl - &cols );
               &panl/ copy= &panl1 xlatey= &vsize
                %end;
          %end;
       %else
          %do;
               %let panl1 = %eval(&panl - 1);
               &panl/ copy= &panl1 xlatex= &hsize
          %end;
   %end;
   %end;
%fini:
   %if &top>0 %then %do;
			0 / 
          ULX=0  ULY=100     URX=&tx URY=100
          LLX=0  LLY=0       LRX=&tx LRY=&by
/*
       %let panl = %eval(&panl + 1);
         &panl/
          ULX=0  ULY=100     URX=&tx URY=100
          LLX=0  LLY=0       LRX=&tx LRY=0
 
          LLX=0  LLY=&ty     LRX=&tx LRY=&ty
*/
   %end;
     %str(;);      %* end the TDEF statement;

   %if &replay ^= %str() %then %do;
      TREPLAY &replay  name="&name" des="panels plot &rows x &cols";
		LIST template;
   %end;
	%if &showgout %then %str(LIST IGOUT;);
run;	quit;

%DONE:
%if &abort %then %put ERROR: The PANELS macro ended abnormally.;
options notes;
%mend;

/*
 *  Macro to generate replay statement for rectangular, lower triangular
 *  and upper triangular displays
 */
 
%macro replay(
	data=,         /* allow a dataset containing variables PLOT and PANEL */
	rows=,
	cols=,
	first=1,
	last=,
	plots=&rows*&cols,
	order=,
	shape=rect    /* RECT, LOWTRI, LOWTRID, UPTRI */
	);

%let shape=%upcase(&shape);
%let order=%upcase(&order);

%if %length(&data)=0 %then %do;
   %if &shape=RECT or &SHAPE=SQUARE %then %do;
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do plot = &first to &first+&plots-1;
			   panel +1;
			   output;
			   end; 
		   %end;
	   %else %do;
	   data _replay_;
		   array plots(&rows, &cols)  _temporary_;
		   drop i j;		
		   plot=&first;
		   do j=1 to &cols;
			   do i=1 to &rows;
			   plots[i,j] = plot;
			   plot+1;
			   end; end;
		   do i= 1 to &rows;
			   do j=1 to &cols;
			   panel+1;
			   plot = plots[i,j];
			   output;
			   end; end;
		   %end;

   %end;
   %else %if &shape=LOWTRI %then %do;
   /*
	  Assumes plots were generated in the lower triangle of a square n x n
	  matrix, so rows=n-1 and cols=n-1 in the macro call.
	  Assumes ORDER=DOWN is also specified.
   */
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do i=2 to &rows+1;
			   do j=1 to i-1;
			   plot+1;
			   panel = (j) + (&cols) *(i-2);
			   output;
			   end;
		   end;
		   %end;
	   %else %do;
	   data _replay_;
		   do j=1 to &cols-1;
			   do i=2 to &rows+1;
			   plot+1;
			   panel = (j) + (&cols) *(i-2);
			   output;
			   end;
		   end;
		   %end;
   %end;

   %else %if &shape=LOWTRID %then %do;
   /*
	  Assumes plots were generated in the lower triangle of a square n x n
	  matrix, including the diagnonal, so rows=n and cols=n in the macro call.
	  Assumes ORDER=DOWN is also specified.
   */
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do i=1 to &rows;
			   do j=1 to i;
			   plot+1;
			   panel = (j) + (&cols) *(i-1);
			   output;
			   end;
		   end;
		   %end;
   %end;

   %else %if &shape=UPTRI %then %do;
   /*
	  Assumes plots were generated in the lower triangle of a square n x n
	  matrix, so rows=n-1 and cols=n-1 in the macro call.
	  Assumes ORDER=DOWN is also specified.
   */
	   %if %index(&order,BYROWS)=0 %then %do;
	   data _replay_;
		   do i=1 to %eval(&rows);
			   do j=i to &cols;
			   plot+1;
			   panel = (j) + (&cols) *(i-1);
			   put i= j= plot= panel=;
			   output;
			   end;
		   end;
		   %end;
	   %else %do;   /* not tested */
	   data _replay_;
		   do j=1 to &cols-1;
			   do i=2 to &rows+1;
			   plot+1;
			   panel = (j) + (&cols) *(i-2);
			   output;
			   end;
		   end;
		   %end;
   %end;
%let data=_replay_;
%end;

data _replay_;
	set &data;
	replay = trim(left(put(panel, 2.))) || ':' || trim(left(put(plot, 5.)));
	run;
proc sort data=_replay_;
	by plot;
*proc print data=_replay_;
*	id panel plot;
run;

proc sql noprint;
	select replay into :replay separated by ' '
	from _replay_;
	quit;

%*put replay = &replay;
%mend;

