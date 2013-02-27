 /*-------------------------------------------------------------------*
  *    Name: corresp.sas                                              *
  *   Title: Correspondence analysis of contingency tables            *
        Doc: http://www.datavis.ca/sasmac/corresp.html             
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  19 Jan 1990 15:23:09                                    *
  * Revised:  27 Feb 2013 16:23:47                                    *
  * Version:  1.9-1                                                   *
  * 1.2  Added dim parameter and colors                               *
  * 1.3  Added graphics GPLOT plot, annotation controls               *
  * 1.5  Uses PROC CORRESP rather than IML , equate axes              *
  *      Now handles MCA, stacked analysis, and other options.        *
  * 1.6  Added 3D plotting (but cant equate axes or control rotation  *
  *      tilt, etc.)                                                  *
  * 1.7  Revised syntax to be more compatible with PROC CORRESP       *
  *      Added Version 8 MCA warning to use BENZECRI/GREENACRE        *
  * 1.8  Fixed validvarname for V7+                                   *
  * 1.9  Added BY= processing and an INANNO= data set                 *
  *      Fixed buglet with MCA when TABLES= includes char & numeric   *
  *      Replaced %label with %labels
  *                                                                   *
  * Requires: %labels, %equate                                        *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
/*=
=Description:
 
 The CORRESP macro carries out simple correspondence analysis of a
 two-way contingency table, and various extensions (stacked analysis,
 MCA) for a multiway table, as in the CORRESP procedures.  It also
 produces labeled plots of the category points in either 2 or 3
 dimensions, with a variety of graphic options, and the facility to
 equate the axes automatically. 

 The macro takes input in one of two forms:

(a) A data set in contingency table form, where the columns are
 separate variables and the rows are separate observations (identified
 by a row ID variable).  That is, the input data set contains R
 observations, and C variables (whose values are cell frequencies)
 for an R x C table.  For this form, specify:

       ID=ROWVAR, VAR=C1 C2 C3 C4 C5

(b) A contingency table in frequency form (e.g., the output from PROC FREQ),
 or raw data, where there is one variable for each factor.  In frequency
 form, there will be one observation for each cell.
 For this form, specify:

       TABLES=A B C

 Include the WEIGHT= parameter when the observations are in frequency
 form.

=Usage:

 The CORRESP macro is called with keyword parameters.  Either the
 VAR= parameter or the TABLES= parameter (but not both) must be
 specified, but other parameters or OPTIONS may be needed to carry
 out the analysis you want.  The arguments may be listed within
 parentheses in any order, separated by commas.  For example:
 
   %corresp(var=response, id=sex year);
 
 The plot may be re-drawn or customized using the output OUT=
 data set of coordinates and the ANNO= Annotate data set.

 The graphical representation of CA plots requires that the axes in the
 plot are equated, so that equal distances on the ordinate and abscissa
 represent equal data units (to perserve distances and angles in the plot).
 A '+', whose vertical and horizontal lengths should be equal,
 is drawn at the origin to indicate whether this has been achieved.

 If you do not specifiy the HAXIS= and YAXIS= parameters, the EQUATE
 macro is called to generate the AXIS statements to equate the
 axes.  In this case the INC=, XEXTRA=, and YEXTRA=, parameters
 may be used to control the details of the generated AXIS statements.

 By default, the macro produces and plots a two-dimensional solution.
 The BY= parameter may be used to produce separate solutions for subsets
 of the data set.
 
==Parameters:

* DATA=     Specifies the name of the input data set to be analyzed.
            [Default: DATA=_LAST_]

* VAR=      Specifies the names of the column variables for simple
            CA, when the data are in contingency table form.
            Not used for MCA (use the TABLES= parameter instead).

* ID=       Specifies the name(s) of the row variable(s) for simple
            CA.  Not used for MCA.

* TABLES=   Specifies the names of the factor variables used to create
            the rows and columns of the contingency table.  For a simple
            CA or stacked analysis, use a ',' or '/' to separate the
            the row and column variables.

* BY=       Specifies the names of one or more BY variables.  You may also
            use the NOTSORTED and DESCENDING keywords. A separate
            analysis and plot is produced for each level of the BY=
            variable(s).  [Default: BY=]

* WEIGHT=   Specifies the name of the frequency (WEIGHT) variable when
            the data set is in frequency form.  If WEIGHT= is omitted,
            the observations in the input data set are not weighted.

* SUP=      Specifies the name(s) of any variables treated as supplementary.
            The categories of these variables are included in the output,
            but not otherwise used in the computations.  Any supplementary
            variables must be included among the variables in the VAR= or
            TABLES= option, e.g., TABLES=Age Sex Row Col, SUP=Age Sex
            for an analysis of Row * Col with Age and Sex as supplementary.

* DIM=      Specifies the number of dimensions of the CA/MCA solution.
            Only two dimensions are plotted by the PPLOT option,
            however. [Default: DIM=2]

* OPTIONS=  Specifies options for PROC CORRESP.  Include MCA for an
            MCA analysis, CROSS=ROW|COL|BOTH for stacked analysis of
            multiway tables, PROFILE=BOTH|ROW|COLUMN for various
            coordinate scalings, etc.  [Default: OPTIONS=SHORT]

* OUT=      Specifies the name of the output data set of coordinates.
            [Default: OUT=COORD]

* ANNO=     Specifies the name of the output annotate data set of labels
            produced by the macro.  [Default: ANNO=LABEL]

* INANNO=   Specifies the name of an input annotate data set for adding
            additional annotations to the plot. [Default: INANNO=]

* PPLOT=    Produce a printer plot? [Default: PPLOT=NO]

* GPLOT=    Produce a graphics plot? [Default: GPLOT=YES]

* PLOTREQ=  The dimensions to be plotted [Default: PLOTREQ=DIM2*DIM1
            when DIM=2, PLOTREQ=DIM2*DIM1=DIM3 when DIM=3]

* HTEXT=    Height for row/col labels.  If not specified, the global
            HTEXT goption is used.  Otherwise, specify one or two numbers
            to be used as the height for row and column labels.
            The HTEXT= option overrides the separate ROWHT= and COLHT=
            parameters (maintained for backward compatibility).

* ROWHT=    Height for row labels

* COLHT=    Height for col labels

* COLORS=   Colors for row and column points, labels, and interpolations.
            In an MCA analysis, only one color is used.
            [Default: COLORS=BLUE RED]

* POS=      Positions for row/col labels relative to the points.
            In addition to the standard Annotate position values, the
            CORRESP macro also understands the special characters "/", 
            "|", or "-", (defined in the LABEL macro)  [Default: POS=5 5]

* SYMBOLS=  Symbols for row and column points, as in a SYMBOL statement.
            [Default: SYMBOLS=NONE NONE]

* INTERP=   Interpolation options for row/column points. In addition to the
            standard interpolation options provided by the SYMBOL statement,
            the CORRESP macro also understands the option VEC to mean
            a vector from the origin to the row or column point.
            The option JOIN may be useful for an ordered factor, and 
            the option NEEDLE may be useful to focus on the positions 
            of the row/column points on the horizontal variable.
            [Default: INTERP=NONE NONE, INTERP=VEC for MCA]

* HAXIS=    AXIS statement for horizontal axis.  If both HAXIS= and
            VAXIS= are omitted, the program calls the EQUATE macro to
				define suitable axis statements.  This creates the axis
				statements AXIS98 and AXIS99, whether or not a graph
				is produced.

* VAXIS=    The name of an AXIS statement for the vertical axis.

* VTOH=     The vertical to horizontal aspect ratio (height of one
            character divided by the width of one character) of the
            printer device, used to equate axes for a printer plot,
            when PPLOT=YES.  [Default: VTOH=2]

* INC=      The length of X and Y axis tick increments, in data units
            (for the EQUATE macro).  Ignored
            if HAXIS= and VAXIS= are specified. [Default: INC=0.1 0.1]

* XEXTRA=   # of extra X axis tick marks at the left and right.  Use to
            allow extra space for labels. [Default: XEXTRA=0 0]

* YEXTRA=   # of extra Y axis tick marks at the bottom and top.
            [Default: YEXTRA=0 0]

* M0=       Length of origin marker, in data units. [Default: M0=0.05]

* DIMLAB=   Prefix for dimension labels [Default: DIMLAB=Dimension]

* NAME=     Name of the graphics catalog entry [Default: NAME=Corresp]        

=Dependencies:
 
 The CORRESP macro calls several other macros not included here.
 It is assumed these are stored in an autocall library.  If not,
 you'll have to %include them in your SAS session or batch program.
 
 LABEL macro - label points 
 EQUATE macro - equate axes
 
 These are available from http://euclid.psych.yorku.ca/ftp/sas/vcd/macros/


=Bugs:

 Using SUP= variables messes up the assignment of symbols and colors
 to the row and column coordinates.

=*/

%macro CORRESP(
	data=_LAST_,        /* Name of input data set                */
	var=,               /* Column variable(s)                    */
	tables=,            /* TABLES statement variables            */
	id=,                /* Row variable or row labels            */
	by=,                /* BY variable                           */
	weight=,            /* Frequency variable (obs. weight)      */
	count=,             /* Frequency variable (obs. weight)      */
	sup=,               /* Supplementary variable(s)             */
	dim=2,              /* Number of CA dimensions               */
	options=short,      /* options for PROC CORRESP              */       
	out=COORD,          /* output data set for coordinates       */
	anno=LABEL,         /* name of annotate data set for labels  */
	inanno=,            /* name of an input annotate data set    */
	pplot=NO,           /* Produce printer plot?                 */
	gplot=YES,          /* Produce graphics plot?                */
	plotreq=,           /* dimensions to be plotted              */
	htext=,             /* height for row/col labels             */
	rowht=,             /* height for row labels                 */
	colht=,             /* height for col labels                 */
	colors=BLUE RED BLACK,    /* Colors for rows, cols, sup      */
	pos=5 5 5,          /* positions for row/col/sup labels      */
	symbols=none none,  /* symbols for row and column points     */
	interp=,            /* interpolations for row/column points  */
	haxis=,             /* AXIS statement for horizontal axis    */
	vaxis=,             /* and vertical axis- use to equate axes */
	vtoh=2,             /* PPLOT cell aspect ratio               */
	inc=0.1 0.1,        /* x, y axis tick increments             */
	xextra=0 0,         /* # of extra x axis tick marks          */
	yextra=0 0,         /* # of extra y axis tick marks          */
	m0=0.05,            /* Length of origin marker               */
	dimlab=,            /* Dimension label                       */
	name=corresp        /* Name for graphics catalog entry       */
     ); 

	%*-- Reset required global options;
	%if &sysver >= 7 %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;


%let abort=0;
%*-- Check for required parameters;
%if %length(&var)=0 and %length(&tables)=0
   %then %do;
      %put ERROR: Either the VAR= or TABLES= parameter must be specified;
      %let abort=1;
      %goto DONE;
   %end;

%*-- Use weight and count as synonyms;
%if %length(&count)=0 and %length(&weight)>0
	%then %let count=&weight;
	
%*-- Set defaults which depend on other options;
%if %length(&plotreq)=0 %then %do;
	%if &dim=2 %then %let plotreq =  dim2 * dim1;
	%if &dim=3 %then %let plotreq =  dim2 * dim1 = dim3;
	%else %let plotreq =  dim2 * dim1;
	%end;

%if %length(&dimlab)=0 %then %do;
	%if &dim=2 %then %let dimlab = Dimension;
	%if &dim=3 %then %let dimlab = Dim;
	%end;

%if %length(&interp)=0 %then %do;
	%if %index(&options, MCA) %then %let interp=vec;
	%else %let interp=none none;
	%end;

%if %index(&options, MCA) & &sysver>7 %then %do;
	%if %index(&options, BENZECRI)=0 & %index(&options, GREENACRE)=0 %then %do;
		%put WARNING: For MCA in Version &sasver, you should use the BENZECRI or GREENACRE options.;
		%end;
	%end;

%*-- Make character options case-insensitive;
%let pplot=%upcase(&pplot);
%let gplot=%upcase(&gplot);
%let interp=%upcase(&interp);
%let options=%upcase(&options);
options nonotes;

%*-- pre-process by variables to find lastby;
%if %length(&by) %then %do;
	%local byvars lastby nby token;
	%let byvars=;
	%let lastby=;
	%let nby=1;
	%let token=%upcase(%scan(&by,&nby,%str( )));
	%do %while(%bquote(&token)^=);
		%if %bquote(&token)=DESCENDING |
				%bquote(&token)=NOTSORTED %then %do;
		%end;
		%else %do;
			%let byvars=&byvars &token;
			%let lastby=&token;
		%end;
		%let nby=%eval(&nby+1);
		%let token=%upcase(%scan(&by,&nby,%str( )));
	%end;

	proc sort data=&data;
		by &by;
%end;
 

%if %length(&tables) %then %do;
	%*-- allow '/' rather than ',' in tables;
   data _null_;
      length tables $ 200;
      tables = "&tables";
      tables = translate(tables, ',', '/');
      call symput('tables', trim(tables));
		do nv=0 by 1 until (word=' ');
			word = scan(tables,nv+1);
			end;
		call symput('nv', left(put(nv,2.)));
   run;
		
	proc corresp data=&data outc=&out dimens=&dim &options;
		%if %length(&count) %then %str(weight &count;);
		tables &tables;
		%if %length(&by)  %then %str(by &by;);
		%if %length(&sup) %then %str(sup &sup;);
	%end;
%else %do;
	proc corresp data=&data outc=&out dimens=&dim &options;
		%if %length(&count) %then %str(weight &count;);
		var &var;
		id &id;
		%if %length(&by)  %then %str(by &by;);
		%if %length(&sup) %then %str(sup &sup;);
	%end;

%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

/* -- doesnt work for mixtures of char/num variables
%if %index(&options,MCA) %then %do;
	%*-- Find number of table variables;
	data _null_;
		set &data;
		array tab{*} &tables;
		if _n_=1 then do;
			nv = dim(tab);
			call symput('nv', left(put(nv,2.)));
			end;
	run; 
	%end;
*/

 /*-----------------------------------------------*
  |  Add % inertia to DIM labels (fix for MCA)    |
  *-----------------------------------------------*/
data &out;
   set &out nobs=nobs;
   drop i percent label;
   length label $30;
	array dimen{*} dim1-dim&dim;
	array contr{*} contr1-contr&dim;
	
	if nobs=0 then do;     *-- Check for empty output data set;
		call symput('abort', '1');
		stop;
		end;

	%if %length(&id)>0 %then %do;
		rename &id = _name_;
		%end;

	if _type_='INERTIA' then do;
		do i=1 to &dim;
			%if %index(&options,MCA) %then %do;
				%*-- Benzecri formula for %inertia in MCA;
				dimen{i} = (&nv/(&nv-1)) * (contr{i}-(1/&nv));
				%end;
			percent = ' (' ||compress(put((100*contr{i} / inertia), 5.1)) || '%)';
			label = "&dimlab" || ' ' || put(i,1.) 
			%if %index(&options,MCA) = 0 & %length(&by)=0
				%then %str(||  trim(percent));
				;
			call symput('p'||trim(left(put(i,2.))), label);
			end;
		end;
run;
%if &abort %then %goto DONE;

data &out;
	set &out;
   label                                                                        
   %do i=1 %to &dim;                                                            
       dim&i = "&&p&i"                                                   
       %end;                                                                    
   ;
                                                                       
proc print data=&out;
   id _type_ _name_;
	var dim1-dim&dim quality;
	%if %length(&by)  %then %str(by &by;);

%*-- Plot increments;
%let n1= %scan(&inc,1,%str( ));                                                      
%let n2= %scan(&inc,2,%str( ));
%if &n2=%str() %then %let n2=&n1;

%*-- Find dimensions to be ploted;
%let ya = %scan(&plotreq,1,%str(* ));
%let xa = %scan(&plotreq,2,%str(* ));
%let za = %scan(&plotreq,3,%str(=* ));
%*put Plotting &ya * &xa = &za;
	

%if &pplot = YES %then %do;
	%put WARNING: Printer plots may not equate axes (using VTOH=&vtoh);                                                     
   %if &sysver < 6.08
       %then %do;
          %put WARNING: CORRESP cannot label points adequately using
               PROC PLOT in SAS &sysver - use SAS 6.08 or later;
          %let symbol = %str( = _name_ );
          %let place =;
       %end;
       %else %do;
           %let symbol = $ _name_ = '*';
           %let place = placement=((h=2 -2 : s=right left)
                                   (v=1 -1 * h=0 -1 to -3 by alt)) ;
       %end;
 
	proc plot data=&out vtoh=&vtoh;
		where (_type_ ^= 'INERTIA');
		plot &ya * &xa &symbol
			/ haxis =by &n1 vaxis=by &n2 &place box;
		%if %length(&by)  %then %str(by &by;);
%end;

 /*--------------------------------------------------*
  | Annotate row and column labels                   |
  *--------------------------------------------------*/
	%*-- Assign colors / positions;
	%let c1= %scan(&colors,1);                                                      
	%let c2= %scan(&colors,2);
	%let c3= %scan(&colors,3);
	%if &c2=%str() %then %let c2=&c1;
	%if &c3=%str() %then %let c3=&c2;
	%let p1 = %scan(&pos,1,%str( ));
	%let p2 = %scan(&pos,2,%str( ));
	%let p3 = %scan(&pos,3,%str( ));
	%if "&p2"="" %then %let p2=&p1;
	%if "&p3"="" %then %let p3=&p2;

	%if %length(&htext)>0 %then %do;
		%let rowht = %scan(&htext,1,%str( ));
		%let colht = %scan(&htext,2,%str( ));
		%if &colht=%str() %then %let colht=&rowht;
		%end;

	%*-- Assign symbols and interpolations;
	%let s1= %scan(&symbols,1);                                                      
	%let s2= %scan(&symbols,2);
	%if &s2=%str() %then %let s2=&s1;
	%let i1= %upcase(%scan(&interp,1));                                                      
	%let i2= %upcase(%scan(&interp,2));
	%if &i2=%str() %then %let i2=&i1;

data _lab_;
	set &out(keep=_type_ _name_ &by dim1-dim&dim);
	where (_type_ ^= 'INERTIA');
	_name_ = trim(left(_name_));

	%labels(data=_lab_, x=&xa, y=&ya, z=&za, text=_name_, size=&rowht,
      color="&c1", subset=_type_='OBS', pos=&p1, out=_lab1_, len=16,
		copy=_type_, by=&by);
	%labels(data=_lab_, x=&xa, y=&ya, z=&za, text=_name_, size=&colht,
      color="&c2", subset=_type_='VAR', pos=&p2, out=_lab2_, len=16,
		copy=_type_, by=&by);
	%if %length(&sup) %then %do;
	%labels(data=_lab_, x=&xa, y=&ya, z=&za, text=_name_, size=&colht,
      color="&c3", subset=_type_=:'SUP', pos=&p3, out=_lab3_, len=16,
		copy=_type_, by=&by);
		%end;
	
	options nonotes;

 /*--------------------------------------------------*
  | Handle vector interpolation                      |
  *--------------------------------------------------*/
                                               
%if &i1=VEC or &i2=VEC %then %do;
data _vector_;
	set &out(keep=_type_ _name_ &by dim1-dim&dim);
	where (_type_ ^= 'INERTIA');
	drop dim1-dim&dim;
	retain xsys ysys '2';
	%if &dim=3 %then %do; retain zsys '2'; %end;             
	%if &i1=VEC %then %do;
      color="&c1";
		if _type_ = 'OBS' then link vec;
		%end;
	%if &i2=VEC %then %do;
      color="&c2";
		if _type_ = 'VAR' then link vec;
		%end;
		return;
vec:                    /* Draw line from the origin to point */
      x = 0; y = 0;
		%if &dim=3 %then %do; z=0; %end;             
      function='MOVE'    ; output;                                              
      x = &xa; y = &ya;                
		%if &dim=3 %then %do; z=&za; %end;             
      function='DRAW'    ; output;                                              
		return;
%end;

 /*--------------------------------------------------*
  | Mark the origin                                  |
  *--------------------------------------------------*/
%if &m0 > 0 %then %do;
data _zero_;
	%if %length(&by)  %then %do;
		set &out(keep=&byvars);
		by &by;
		if first.&lastby then do;
		%end;
	xsys='2';  ysys='2';
	%if &dim=3 %then %do; zsys='2'; z=0; %end;             
	x = -&m0;  y=0;   function='move'; output;
	x =  &m0;         function='draw'; output;
	x = 0;  y = -&m0; function='move'; output;
	        y =  &m0; function='draw'; output;
	%if %length(&by)  %then %do;
		end;
		%end;
%end;

 /*--------------------------------------------------*
  | Concatenate annotate data sets                   |
  *--------------------------------------------------*/
data &anno;
	set _lab1_ _lab2_ 
	%if %length(&sup) %then %str(_lab3_);
	%if &m0 > 0 %then _zero_ ; 
	%if &i1=VEC or &i2=VEC %then _vector_;
	%if %length(&inanno) %then &inanno;
	;
	%if %length(&by)  %then %str(by &by;);
%if &i1=VEC %then %let i1=none;
%if &i2=VEC %then %let i2=none;

%if %length(&vaxis)=0 and %length(&haxis)=0 %then %do;
	%let x1= %scan(&xextra,1);                                                      
	%let x2= %scan(&xextra,2);
	%if &x2=%str() %then %let x2=&x1;
	%let y1= %scan(&yextra,1);                                                      
	%let y2= %scan(&yextra,2);
	%if &y2=%str() %then %let y2=&y1;

	%equate(data=&out, x=&xa, y=&ya, plot=no,
		vaxis=axis98, haxis=axis99, xinc=&n1, yinc=&n2,
		xmextra=&x1, xpextra=&x2, ymextra=&y1, ypextra=&y2);
	%let vaxis=axis98;
	%let haxis=axis99;
	options nonotes;
	%end;
%else %do;
%if %length(&vaxis)=0 %then %do;
	%let vaxis=axis98;
	%put WARNING:  You should use an AXISn statement and specify VAXIS=AXISn to equate axis units and length;
   axis98 label=(a=90);
	%end;
%if %length(&haxis)=0 %then %do;
	%let haxis=axis99;
	%put WARNING:  You should use an AXISm statement and specify HAXIS=AXISm to equate axis units and length;
   axis99 offset=(2);
	%end;
%end;

%if %length(&sup)=0 %then %do;
	symbol1 v=&s1 i=&i1 l=33 c=&c1;
	symbol2 v=&s2 i=&i2 l=20 c=&c2;
	symbol3 v=none c=black;
	%end;
%else %do;
	symbol1 v=&s1 i=&i1 l=33 c=&c1;
	symbol2 v=none c=&c3;
	symbol3 v=&s2 i=&i2 l=20 c=&c2;
	%end;

%if &gplot = YES %then %do;
	%if &dim=2 or %length(&za)=0 %then %do;
	proc gplot data=&out  ;
		where (_type_ ^= 'INERTIA');
		plot &ya * &xa = _type_
				/ anno=&anno frame nolegend
					vaxis=&vaxis haxis=&haxis vminor=1 hminor=1
					name="&name"
					des="CORRESP plot of &data";
		%if %length(&by)  %then %str(by &by;);
	run;quit;
	%end;

	%else %if &dim=3 %then %do;
	%put WARNING: 3D plots do not equate axes.  Try GOPTIONS HSIZE and VSIZE.;                                                     
	proc g3d data=&out  ;
		where (_type_ ^= 'INERTIA');
		plot &ya * &xa = &za
				/ anno=&anno  
					xticknum=2 yticknum=2 zticknum=2 grid
					name="&name"
					des="3D CORRESP plot of &data";
	run;quit;
	%end;

%end;	/* %if &gplot = YES */

 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
proc datasets nofs nolist library=work memtype=(data);
    delete _lab1_ _lab2_
	%if %length(&sup) %then _lab3_;
	 %if &i1=VEC or &i2=VEC %then _vector_;
	 ;
	 run; quit;

%done:
%if &abort %then %put ERROR: The CORRESP macro ended abnormally.;
	%*-- Restore global options;
	%if &sysver >= 7 %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;
