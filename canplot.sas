/*-------------------------------------------------------------------*
  *    Name: canplot.sas                                             *
  *   Title: Canonical discriminant structure plot.                  *
 *         Plots class means on two canonical variables, confidence  *
 *         circles for those means, and variable vectors showing the *
 *         correlations of variables with the canonical variates.    *
      Doc: http://www.datavis.ca/sasmac/canplot.html           
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@YorkU.ca>         *
 * Created:  24 Nov 1991 13:17:10                                    *
 * Revised:  22 May 2007 12:45:46                                    *
 * Version:  1.6-5                                                   *
 *  - Added hsym, legend control                                     *
 *  - Added canx= cany= to plot other canonical dimensions           *
 *  - Warning message for <3 groups (only 1 dimension) - no plot     *
 * 1.3                                                               *
 *  - Added code to equate axes: requires %equate                    *
 *  - Added HTEXT= for height of variable labels                     *
 *  - When PLOT=NO generate AXIS statements anyway for use with other*
 *    plot annotations                                               *
 * 1.4                                                               *
 *  - Added ID=, HID= and IDCOLOR= to label observations in the plot *
 *    (This requires the %labels macro)                              *
 *  - Cleaned up annotate data set                                   *
 *  - Fixed error in calculating variance proportions                *
 *  - Added LINES= option to control line styles                     *
 * 1.5                                                               *
 *  - Fixed bug with colors not recycled when gps># colors           *
 *  - Added ability to include additional input annotate data sets   *
 * 1.6                                                               *
 *  - Implemented scale=AUTO                                         *
 *  - Added IDSUBSET= to subset the obs. labels plotted.             *
 *  - Added VARCOLOR= to set the color for variable vectors/labels   *
 *  - Updated inline GENSYM to latest version                        *
 *-------------------------------------------------------------------*/

 /*=
=Description:
 
 The CANPLOT macro constructs a canonical discriminant structure
 plot. The plot shows class means on the two largest canonical
 variables, confidence circles for those means, and variable vectors
 showing the correlations of variables with the canonical variates.

==Method:

 Discriminant scores and coefficients are extracted from PROC CANDISC
 and plotted.
 
 Other designs may be handled either by (a) coding factor combinations
 'interactively', so, e.g., the combinations of A*B are represented by
 a GROUP variable, or (b) by applying the method to adjusted response
 vectors (residuals) with some other predictor (class or continuous)
 partialled out.  The latter method is equivalent to analysis of
 the residuals from an initial PROC GLM step, with the effects to be
 controlled or adjusted for as predictors.
 
 e.g., to examine Treatment, controlling for Block and Sex,

    proc glm data=..;
     model Y1-Y5 = block sex;
     output out=resids
        r=E1-E5;
   %canplot(data=resids, var=E1-E5, class=Treat, ... );

 
=Usage:

 The CANPLOT macro is defined with keyword parameters.
 Values must be supplied for the CLASS= and VAR= parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%canplot(data=inputdataset, var=predictors, class=groupvariable...,);

 The interpretation of the angles betweeen variable vectors
 relies on the units for the horizontal and vertical axes being made
 equal (so that 1 data unit measures the same length on both axes.
 The axes should be equated either by using the GOPTIONS HSIZE= VSIZE=
 options, or using the macro HAXIS= and VAXIS= parameters
 and AXIS statements which specify the LENGTH= value for both
 axes. The current version now uses the EQUATE macro if the 
 HAXIS= and VAXIS= arguments are not supplied.
 
==Parameters:

* DATA=       Name of data set to analyze [Default: DATA=_LAST_]

* CLASS=      Name of one class variable, defining the groups to be
              discriminated.  

* VAR=        List of response/classification variables

* ID=         Observation ID variable, used to label observations in the plot.

* VARLAB=     How to label variables? _NAME_ or _LABEL_ [Default: VARLAB=_NAME_]

* DIM=        Number of canonical dimensions to be extracted. [Default: DIM=2]

* SCALE=      Scale factor for variable vectors in plot. The variable 
              vectors are multiplied by the SCALE= value, which should
              be specified (perhaps by trial and error) to make the vectors and
              observations fill the same plot region. If SCALE=AUTO or
			  SCALE=0, the program estimates a scale factor from the ratio
			  of the maximum distance to the origin in the observations
			  relative to the variables. [Default: SCALE=AUTO]

* CONF=       Confidence probability for canonical means, determining the radius
              of configence circles. [Default: CONF=.95]

* OUT=        Output data set containing discrim scores [Default: OUT=_DSCORE_]

* OUTVAR=     Output data set containing coefficients [Default: OUTVAR=_COEF_]

* ANNO=       Output data set containing annotations [Default: ANNO=_DANNO_]

* ANNOADD=    Additional annotations to add to the plot.  Can include
              'MEAN' and/or 'GPLABEL' and/or the name(s) of additional
			  input annotate data sets. [Default: ANNOADD=MEAN]

* PLOT=       YES (or NO to suppress plot) [Default: PLOT=YES]

* HAXIS=      The name of an optional AXIS statement for 
			  the horizontal axis. The HAXIS= and VAXIS= arguments may be used
			  to equate the axes in the plot so that the units are the same
			  on the horizontal and vertical axes.  If neither HAXIS= nor
			  VAXIS= are supplied, the EQUATE macro is called to generate
			  axis statements.

* VAXIS=      The name of an optional AXIS statement for 
              the  vertical axis.

* INC=        X, Y axis tick increments, in data units [Default: INC=1 1]

* XEXTRA=     # of extra X axis tick marks on the left and right.  Use this to
              extend the axis range. [Default: XEXTRA=0 0]

* YEXTRA=     # of extra Y axis tick marks on the bottom and top. [Default: YEXTRA=0 0]

* LEGEND=     Name of a LEGEND statement to specify legend for groups.  Use
              LEGEND=NONE to suppress the legend (perhaps with ANNOADD=GPLABEL
			  to plot group labels near the means).

* HSYM=       Height of plot symbols [Default: HSYM=1.2]

* HID=        Height of ID labels [Default: HID=1.4]

* IDCOLOR=    Color of ID labels

* IDSUBET=    Logical expression to subset the observation labels plotted

* HTEXT=      Height of variable and group labels [Default: HTEXT=1.5]

* CANX=       Horizontal axis of plot [Default: CANX=CAN1]

* CANY=       Vertical axis of plot [Default: CANY=CAN2]

* DIMLAB=     Dimension label prefix [Default: DIMLAB=Canonical Dimension]

* COLORS=     List of colors to be used for groups (levels of the CLASS= variable).
              The values listed are recycled as needed for the number of groups.
             [Default: COLORS=RED GREEN BLUE BLACK PURPLE BROWN ORANGE YELLOW]

* SYMBOLS=    List of symbols to be used for the observations within the groups,
              recycled as needed.
              [Default: SYMBOLS=dot circle triangle square star - : $  =]

* LINES=      List of line style numbers used for the confidence circles.
              [Default: LINES=20 20 20 20 20 20 20]

* VARCOLOR=   Name of a SAS color to be used to draw the variable vectors
              and variable names.

* NAME=       Name for graphic catalog entry [Default: NAME=CANPLOT]

* GOUT=       The name of the graphics catalog [Default: GOUT=GSEG]
                

==Dependencies:

 Requires: equate.sas


 =*/
 
%macro canplot(
	data=_last_,  /* name of data set to analyze               */
	class=,       /* name of class variable                    */
	var=,         /* list of classification variables          */
	id=,          /* observation ID variable                   */
	varlab=_NAME_, /* How to label variables? _NAME_ or _LABEL_ */
	dim=2,        /* number of canonical dimensions            */
	scale=AUTO,   /* scale factor for variable vectors in plot */
	conf=.95,     /* confidence probability for canonical means*/
	out=_dscore_, /* output data set containing discrim scores */
	outvar=_coef_,  /* output data set containing coefficients */
	anno=_danno_, /* output data set containing annotations    */
    annoadd=MEAN, /* additional annotations                    */
	plot=YES,     /* or NO to suppress plot                    */
	haxis=,       /* AXIS statement for horizontal axis        */
	vaxis=,       /* and for vertical axis- use to equate axes */
	inc=1 1,      /* x, y axis tick increments                 */
	xextra=0 0,   /* # of extra x axis tick marks              */
	yextra=0 0,   /* # of extra y axis tick marks              */
	legend=,      /* LEGEND statement                          */
	hsym=1.2,     /* height of plot symbols                    */
	hid=1.4,      /* height of ID labels                       */
	idcolor=,     /* color of ID labels                        */
	idsubset=1,   /* expresstion to subset ID labels           */
	htext=1.5,    /* height of variable and group labels       */
	canx=can1,    /* Horizontal axis of plot                   */
	cany=can2,    /* Vertical axis of plot                     */
	dimlab=Canonical Dimension,    /* Dimension label prefix   */
	colors=RED GREEN BLUE BLACK PURPLE BROWN ORANGE YELLOW,
	symbols=dot circle triangle square star -    :     $    =,
	lines=20 20 20 20 20 20 20 20, 
	varcolor=,    /* color for variable vectors and labels     */
	name=CANPLOT, /* name for graphic catalog entry            */
	gout=
	  );
 
%let plot = %substr(%upcase(&PLOT),1,1);
%let annoadd = %upcase(&annoadd);
%let legend = %upcase(&legend);
%if %upcase(&scale)=AUTO %then %let scale=0;

%local me; %let me=CANPLOT;

%if %length(&var)=0 %then %do;
    %put ERROR: (&me)  You must specify a VAR= variables list ;
    %goto DONE;
    %end;
%if %length(&class)=0 or %length(%scan(&class,2))>0 %then %do;
    %put ERROR: (&me)  You must specify one CLASS= variable ;
    %goto DONE;
    %end;
 
proc candisc data=&data /* ncan=&dim */
             out=&out short
             outstat=_dstat_;
   classes &class;
   var &var ;
	run;
options nonotes;
%if %sysevalf(&sysver  > 6.08) %then %do;
	*-- Extract canonical correlations --> % eig;
	data _dull_;
		set _dstat_ end=eof;
		drop &class &var;
		where (_type_='CANCORR');
		array v{*} &var;
		r = v(1);
		eig = r**2 / (1-r**2);
		cum+eig;
		if eof then call symput('cum', left(put(cum,best12.5)));
	run;
	*proc print;
	data _null_;
		set _dull_;
		p = eig / &cum;
		pc = '(' || trim(left(put(100*p, 4.1))) || '%)';
		call symput('p'||put(_n_,1.), pc);
	*proc print;
	%end;
%else %do i=1 %to &dim;
	%let p&i=;
	%end;
proc sort data=&out;
   by &class;
data &out;
	set &out;
	label 
	%do i=1 %to &dim;
		can&i = "&dimlab &i &&p&i"
		%end;
		;

proc means data=&out noprint;
   var &canx &cany;
   by &class;
   output out=_means_ mean=&canx &cany n=n;
proc print data=_means_;
 
data &anno;
   set _means_(drop=_type_ _freq_) end=eof nobs=gps;
   length text $20 color function comment $8;
   retain xsys '2' ysys '2';
   drop &canx &cany n a ang xp yp;
   x = &canx;
   y = &cany;
   color=scan(repeat("&colors ",gps),_n_);
   /* mark the class mean        */
	%if %index(&annoadd,MEAN) > 0 %then %do;
		comment='MEAN';
		text = 'PLUS';
		hsys = '4'; size = 4;
		function = 'SYMBOL';      output;
		%end;
 
 *-- Write group label (convert numeric &class to character);
   %if &class ^= %str() and %index(&annoadd,GPLABEL) > 0 %then %do;
	  comment='GPLABEL';
      text = trim(left(&class));
      position='3';
      size = &htext; hsys=' ';
      function='LABEL'; output;
   %end;

   /* draw confidence region */
   size = sqrt( cinv(&conf, 2, 0) / n ) ;   * radius ;
   line = input(scan("&lines", _n_), best6.);
   if line=. then line = 20;
   comment = 'CIRCLE';
   do a = 0 to 360 by 10;                   * draw a "circle" ;
      ang = a*arcos(-1)/180;                * convert to radians;
      xp=  size * cos(ang);
      yp=  size * sin(ang);
      x = xp+ &canx;                         * translate to means;
      y = yp+ &cany;
      if a=0 then FUNCTION = 'MOVE    ';
             else FUNCTION = 'DRAW    ';
      output;
   end;
	
   if eof then do;          * save number of groups ;
      call symput('NGP',put(_n_,best5.));
      end;
*proc print data=_dstat_;

data _coeff_;
   set _dstat_;             * get standardized coefficients ;
   drop _TYPE_ ;
   if _type_ = 'STRUCTUR' |  /* Version 6 */
      _type_ = 'TSTRUCT'  |  /* Version 5 */
	  _type_ = 'RSQUARED';
	if _type_ = 'RSQUARED' then _name_='RSQUARED';

proc transpose data=_coeff_ out=&outvar;
proc print data=&outvar;

%if &ngp < 3 %then %do;
	%put WARNING:  &me cannot produce a 2D plot for &NGP groups.;
	%goto done;
	%end;

*-- scale=0 or scale=AUTO: ratio of max dist from origin ;
data _obsmax_;
	set &out end=eof;
	retain maxo 0;  keep maxo;
	maxo = max(maxo, sqrt(uss(&canx, &cany)));
	if eof then output;
	*proc print;
	
data _varmax_;
	set &outvar end=eof;
	where (&canx^=.);
	retain maxv 0;  keep maxv;
	maxv = max(maxv, sqrt(uss(&canx, &cany)));
	if eof then output;
	*proc print;

data _null_;
	merge _obsmax_ _varmax_;
	ratio = round(maxo / maxv, .01);
	scale = &scale;
	put "&me: " scale= '-> suggested AUTO scale=' ratio;
	if scale=0 then do;
		call symput('scale', put(ratio, best5.1));
		put "&me: scale set to " ratio;
		end;
run;
	
data _vector_;
   set &outvar;
   where (&canx^=.);
   length function comment $8 text $20;
   retain xsys '2' ysys '2' position '6';
   drop _name_  can: RSQUARED _label_;
   x = 0 ; y = 0;
   comment = 'VARLABEL';
   %if %length(&varcolor) %then %do;
    	color = "&varcolor";
    	%end;
   function = 'MOVE ' ; output;
   x = &scale * &canx ;
   y = &scale * &cany ;
   function = 'DRAW ' ; output;
   size=&htext;
	%if %upcase(&varlab)=_NAME_ %then %do;
    	text = _NAME_;
		text = substr(text,1,1) || lowcase(substr(text,2));
		%end;
	%else %do;
		text = _LABEL_;
	%end;
  
	if &cany >=0                                                               
		then position='2';             /* up justify         */                
		else position='E';             /* down justify       */
   function = 'LABEL' ; output;
   run;
 
%if %length(&id) >0 %then %do;
%labels(data=&out,
	x=&canx, y=&cany,
	size=&hid,
    color=&idcolor,
	text=&id,
	subset=&idsubset,
	out=_canlab_
	);
	%end;

%let annoadd=%remword(&annoadd,MEAN);
%let annoadd=%remword(&annoadd,GPLABEL);
%*put ANNOADD= &annoadd;
data &anno;
   set &anno _vector_
	%if %length(&id) >0 %then _canlab_(in=inlab);
	%if %length(&annoadd) >0 %then &annoadd;
   ;
 %if %length(&id) >0 %then %do;
	if inlab then comment='OBSLABEL';
	%end;
 
%if %length(&vaxis)=0 and %length(&haxis)=0 %then %do;
	%local n1 n2 x1 x2 y1 y2;
	%*-- Plot increments;
	%let n1= %scan(&inc,1,%str( ));                                                      
	%let n2= %scan(&inc,2,%str( ));
	%if &n2=%str() %then %let n2=&n1;

	%let x1= %scan(&xextra,1);                                                      
	%let x2= %scan(&xextra,2);
	%if &x2=%str() %then %let x2=&x1;
	%let y1= %scan(&yextra,1);                                                      
	%let y2= %scan(&yextra,2);
	%if &y2=%str() %then %let y2=&y1;

	%put &me: Running EQUATE to equate the axes;
	%equate(data=&out, x=&canx, y=&cany, plot=no,
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

%if &plot = Y %then %do;
   %gensym(n=&ngp, h=&hsym, symbols=&symbols, colors=&colors);


	%if &legend=NONE %then %let legend=nolegend;
   %else %if %length(&legend)=0 %then %do;
      legend1 label=(h=&hsym) value=(h=&hsym);
      %let legend=legend=legend1;
      %end;
	%else %let legend=legend=&legend;


	%if %length(&gout) %then %let gout=gout=&gout;
   proc gplot data=&out &gout;
      plot &cany * &canx = &class
           / anno=&anno frame
             vaxis=&vaxis haxis=&haxis &legend
             hm=1 vm=1 name="&name"
				 des="canplot of &data" ;
      run; quit;
   %end;  /* end &plot=YES */

*-- Clean up;
proc datasets lib=work memtype=data nolist nowarn;
   delete _dull_ _coeff_ _dstat_ _means_ _vector_ _obsmax_ _varmax_
	%if %length(&id) >0 %then _canlab_;
	run; quit;	*-- quit necessary to avoid notes;
%done:
options notes;
%mend;
 
 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each GROUP |
  *----------------------------------------------------*/
%macro gensym(
   n=1,
   start=1, 
   h=1.5,
   interp=none,
   line=1,
   symbols=%str(square triangle : $ = X _ Y),
   colors=BLACK RED GREEN BLUE BROWN ORANGE PURPLE YELLOW,
   ci=,
   font=,
   width=1,
   repeat=1,
   label=
   );

*options mprint symbolgen;
    %*--  symbols, colors, line styles, and interp are recycled as needed;
  %local chr col int lin k cic;
  %do k=&start %to %eval(&n + &start -1) ;
     %if %length(%scan(&symbols, &k, %str( ))) = 0 
	      %then %let symbols = &symbols &symbols;
     %if %length(%scan(&colors, &k, %str( ))) = 0 
	      %then %let colors = &colors &colors;
     %if %length(%scan(&interp, &k, %str( ))) = 0 
	      %then %let interp = &interp &interp;
     %if %length(%scan(&line,   &k, %str( ))) = 0 
	      %then %let line = &line &line;
     %if %length(%scan(&width,   &k, %str( ))) = 0 
	      %then %let width = &width &width;
     %if %length(&ci) 	%then %do;
         %if %length(%scan(&ci,  &k, %str( ))) = 0 
	           %then %let ci = &ci &ci;
	     %end;

	 %let chr =%scan(&symbols,&k, %str( ));
     %let col =%scan(&colors, &k, %str( ));
     %let int =%scan(&interp, &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));
     %let wid =%scan(&width,  &k, %str( ));
     %let lin =%scan(&line,   &k, %str( ));

	  %if &k=99 %then %let repeat=999;
     symbol&k
      %if %length(&font) 	  %then font=&font;
	  %if %length(&label) 	%then pointlabel=%str( (h=1 "#&label") );
	  height=&h value=&chr color=&col i=&int l=&lin w=&wid r=&repeat
	  %if %length(&cic) 	%then %str(ci=&cic);
	  ;
	%if &k=99 %then %goto done;
  %end;
*options nomprint nosymbolgen;
%done:
%mend;
%macro remword(string,remove,dlm=%str( ));
%*--------------------------------------------------;
%* Remove word contained in Remove from String
%*--------------------------------------------------;
   %local count word result;
   %let count=1;
   %let word = %scan(&string,&count,&dlm);
   %let result=;
   %do %while(%quote(&word)^= );
       %if(%index(
	    	%upcase(&dlm.&remove.&dlm),
			%upcase(&dlm.&word.&dlm) )=0)
	    	%then %let result = &result &word;
	%*put REMWORD: word=&word result=&result;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,&dlm);
   %end;
   &result
%mend;

