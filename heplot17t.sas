  /*--------------------------------------------------------------*
  *    Name: heplot.sas                                           *
  *   Title: Plot Hypothesis and Error matrices for one MLM effect *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/heplot.html      *
  *---------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>     *
  * Created:  10 Jan 1997 13:29:31                                *
  * Revised:  16 Apr 2007 15:37:49                                *
  * Version:  1.7t                                               *
  *  1.3 Fixed some problems with variable case names for V7+     *
  *      Allow LEGEND=NONE                                        *
  *  1.4 Added CLASS= parameter and ability to handle MMRA data   *
  *      Added %cat macro to catenate level values for 2+ way     *
  *      designs.                                                 *
  *  1.5 Added SCALE= parameter to provide relative scaling of    *
  *      the H and E matrices.                                    *
  *      Added WIDTH=                                             *
  *  1.6 Added ADD=CANVEC to plot canonical vectors               *
  *      Added EFFLOC= and fixed positioning of effect labels     *
  *      Default for EFFLAB changed to EFFLAB=&EFFECT             *
  *  1.7 Added SIZE= to allow evidence-based scaling              *
  *                                                               *
  *--------------------------------------------------------------*/
 /*=

=Description:

 The HEPLOT macro plots the covariance ellipses for a hypothesized (H)
 effect and for error (E) for two variables from a multivariate linear
 model (MLM).  For a MANOVA effect, the plot helps to show how the
 means of the groups differ on the two variables jointly, in relation
 to the within-group variation.  The test statistics for any MANOVA are
 essentially saying how 'large' the variation in H is, relative to the
 variation in E, and in how many dimensions.  The HEPLOT macro shows a
 two-dimensional visualization of the answer to this question.
 
 An alternative two-dimensional view is provided by the
 CANPLOT macro, which shows the data, variables, and within-group ellipses
 projected into the space of the largest two canonical variables---
 linear combinations of the responses for which the group differences
 are largest. The HECAN macro displays the H and E ellipses in canonical
 space.
 
 Typically, you perform a MANOVA analysis with PROC GLM, and save the
 output statistics, including the H and E matrices, using the OUTSTAT=
 option.  This must be supplied to the macro as the value of the
 STAT= parameter.  If you also supply the raw data for the analysis
 via the DATA= parameter, the means for the levels of the EFFECT=
 parameter are also shown on the plot.

 Various kinds of plots are possible, determined by the M1= and M2=
 parameters.  The default is M1=H and M2=E.  If you specify M2=I
 (identity matrix), then the H and E matrices are transformed to H* = eHe
 (where e=E^-1/2), and E*=eEe=I, so the errors become uncorrelated, 
 and the size of H* can be judged more simply in relation to a 
 circular E*=I. For multi-factor designs, is it sometimes useful to 
 specify M1=H+E, so that each factor can be examined in relation to the
 within-cell variation.
	
=Usage:

 The HEPLOT macro is defined with keyword parameters.  The STATS=
 parameter and either the VAR= or the X= and Y= parameters are required.
 You must also specify the EFFECT= parameter, indicating the H matrix
 to be extracted from the STATS= data set.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	proc glm data=dataset outstat=HEstats;
		model y1 y2  = A B A*B / ss3;
		manova;
	%heplot(data=dataset, stat=HEstats, var=y1 y2, effect=A );
	%heplot(data=dataset, stat=HEstats, var=y1 y2, effect=A*B );
 
==Parameters:

* STAT=       Name of the OUTSTAT= dataset from proc glm containing the
              SSCP matrices for model effects and ERROR, as indicated by
			  the _SOURCE_ variable.

* DATA=       Name of the input, raw data dataset (for means)

* X=          Name of the horizontal variable for the plot

* Y=          Name of the vertical variable for the plot

* VAR=        2 response variable names: x y.  Instead of specifying X=
              and Y= separately, you can specify the names of two response
			  variables with the VAR= parameter.

* EFFECT=     Name of the MODEL effect to be displayed for the H matrix.
              This must be one of
              the terms on the right hand side of the MODEL statement used
			  in the PROC GLM or PROC REG step, in the same format that this
			  efffect is labeled in the STAT= dataset. This must be one of
			  the values of the _SOURCE_ variable contained in the STAT= 
			  dataset.

* CLASS=      Names of class variables(s), used to find the means for groups
              to be displayed in the plot.  The default value is the value
			  specified for EFFECT=, except that '*' characters are changed
			  to spaces. Set CLASS=, (null) for a quantitative regressor or to
              suppress plotting the means.

* EFFLAB=     Label (up to 16 characters) for the H effect, annotated near the max/min
              corner of the H ellipse. [Default: EFFLAB=&EFFECT]

* EFFLOC=     Location for the effect label: MAX (above) or MIN (below). [Default: EFFLOC=MAX]

* MPLOT=      Matrices to plot MPLOT=1 plots only the H ellipse. [Default: MPLOT=1 2]

* GPFMT=      The name of a SAS format for levels of the group/effect variable used in labeling
              group means.

* ALPHA=      Non-coverage proportion for the ellipses [Default: ALPHA=0.32]

* PVALUE=     Coverage proportion, 1-alpha [Default: PVALUE=0.68]

* SS=         Type of SS to extract from the STAT= dataset. The possibilities
              are SS1-SS4, or CONTRAST (but the SSn option on the MODEL statement in
			  PROC GLM will limit the types of SSCP matrices produced).
			  This is the value of the _TYPE_ variable in the STAT= dataset.
			  [Default: SS=SS3]

* WHERE=      To subset both the STAT= and DATA= datasets

* ANNO=       Name of an input annotate data set, used to add additional
              information to the plot of y * x.

* ADD=        Specify ADD=CANVEC to add canonical vectors to the plot. The
              PROC GLM step must have included the option CANONICAL on the
			  MANOVA statement.

* M1=         First matrix: either H or H+E [Default: M1=H]

* M2=         Second matrix either E or I [Default: M2=E]

* SCALE=      Scale factors for M1 and M2.  This can be a pair of numeric
              values or expressions using any of the scalar values calculated
			  in the PROC IML step.  The default scaling [SCALE=1 1]
              results in a plot of E/dfe and H/dfe, where the size 
			  and orientation of E shows error variation on the data scale,
			  and H is scaled conformably, allowing the group means to be
			  shown on the same scale. The _natural scaling_ of H and E
			  as generalized mean squares would be H/dfh and E/dfe, which is
			  obtained using SCALE=dfe/dfh 1, Equivalently, the E matrix can
			  be shrunk by the same factor by specifying SCALE=1 dfh/dfe.  

* VAXIS=      Name of an axis statement for the y variable

* HAXIS=      Name of an axis statement for the x variable

* LEGEND=     Name of a LEGEND statement.  If not specified, a legend for
              the M1 annd M2 matrices is drawn beneath the plot.

* COLORS=     Colors for the H and E ellipses [Default: COLORS=BLACK RED]

* LINES=      Line styles for the H and E ellipses [Default: LINES=1 21]

* WIDTH=      Line widths for the H and E ellipses [Default: WIDTH=3 2]

* HTEXT=      Height of text in the plot.  If not specified, the global
              graphics option HTEXT controls this. 

* OUT=        Name of the output dataset containing the points on the
              H and E ellipses. [Default: OUT=OUT]

* NAME=       Name of the graphic catalog entry [Default: NAME=HEPLOT]

* GOUT=       Name of the graphic catalog [Default: GOUT=GSEG]
                
=References:

 Friendly, M. (2006).
   Data Ellipses, HE Plots and Reduced-Rank Displays for Multivariate Linear 
   Models: SAS Software and Examples. 
   Journal of Statistical Software, 17(6), 1-42.
   http://www.jstatsoft.org/v17/i06/

 Friendly, M. (2007).
   HE plots for Multivariate General Linear Models.
   Journal of Computational and Graphical Statistics, 16, in press.
   http://www.math.yorku.ca/SCS/Papers/heplots.pdf 


 =*/

%macro heplot(
	stat=,          /* name of OUTSTAT= dataset from proc glm       */
	data=,          /* name of the input dataset (for means)        */
	x=,             /* name of horizontal variable for the plot     */
	y=,             /* name of vertical variable for the plot       */
	var=,           /* 2 response variable names: x y               */
	effect=,        /* name of MODEL effect to be displayed         */
	class=&effect,  /* names of class variables(s)                  */
	efflab=&effect, /* label for the H matrix                       */
	effloc=max,     /* location for the label (min/max)             */
	mplot=1 2,      /* matrices to plot                             */
	gpfmt=,         /* format for levels of the group/effect var    */
	size=effect,    /* how to scale H ellipse(s) relative to E      */
	alpha=0.05,     /* signif level for Roy test, if size=evidence  */
	pvalue=0.68,    /* coverage proportion, 1-alpha                 */
	ss=ss3,         /* type of SS to extract from the STAT= dataset */
	where=,         /* to subset both the STAT= and DATA= datasets  */
	anno=,          /* name of an input annotate data set           */
	add=,           /* add=CANVEC adds canonical vectors for H      */
	m1=H,           /* first matrix: either H or H+E                */
	m2=E,           /* second matrix either E or I                  */
	scale=1 1,      /* scale factors for M1 and M2                  */
	vaxis=,         /* name of an axis statement for the y variable */
	haxis=,         /* name of an axis statement for the x variable */
	legend=,        /* name of a LEGEND statement                   */
	colors=BLACK RED,     /* colors for the H and E ellipses        */
	lines=1 21,     /* line styles for the H and E ellipses         */
	width=3 2,      /* line widths for the H and E ellipses         */
	htext=,         /* height of text labels                        */
	hsym=,          /* height of point symbols (means)              */
	print=,
	out=out,        /* name of the output dataset                   */
	name=heplot,    /* name of the graphic catalog entry            */
	gout=gseg       /* name of the graphic catalog                  */
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

%local me; %let me = HEPLOT;
%if %length(&x)=0 %then %let x = %scan(&var,1);
%if %length(&y)=0 %then %let y = %scan(&var,2);

%*-- Check for required parameters;
%local abort;
%let abort=0;
%if %length(&x)=0 or %length(&y)=0 %then %do;
	%put ERROR: (&me) The X= and Y= variables (or VAR=) must be specified;
	%let abort=1;
	%goto done;
	%end;

%if %length(&stat)=0 %then %do;
	%put ERROR: (&me) The STAT= data set (OUTSTAT= from GLM) is required;
	%let abort=1;
	%goto done;
	%end;
%else %do;
	data &stat;
		set &stat;
		_SOURCE_ = upcase(_SOURCE_);
	%end;
	
%if %length(&effect)=0 %then %do;
	%put ERROR: (&me) The EFFECT= parameter is required;
	%let abort=1;
	%goto done;
	%end;
%let effect=%upcase(&effect);
%let effloc=%upcase(&effloc);

%let m1 = %upcase(&m1);
%let m2 = %upcase(&m2);
%let ss = %upcase(&ss);
%let legend = %upcase(&legend);

options symbolgen;
%local sf;
%let size=%substr(%upcase(&size),1,3);
%if &size=EFF
	%then %let sf=1;
	%else %let sf=2;
options nosymbolgen;

%local c1 c2;
%let c1 = %scan(&colors, 1);
%let c2 = %scan(&colors &colors, 2);

%local l1 l2;
%let l1 = %scan(&lines, 1);
%let l2 = %scan(&lines &lines, 2);

%local w1 w2;
%let w1 = %scan(&width, 1);
%let w2 = %scan(&width &width, 2);

%if %length(&scale)=0 %then %let scale=1 1;
%if "%upcase(&scale)"="H" %then %let scale=sqrt(n) 1;
%if "%upcase(&scale)"="E" %then %let scale=1 1/sqrt(n) ;

%local s1 s2;
%let s1 = %scan(&scale, 1, %str( ));
%let s2 = %scan(&scale &scale, 2, %str( ));

%if %length(&where) %then %do;
	data _subset_;
		set &stat;
		where (&where);
	%put &me: Subsetting the stat=&stat data set, where=(&where);
	%let stat=_subset_;
	%end;

%if %length(&data) %then %do;
	proc summary data=&data nway;
		var &x &y;
		%if %length(&where) %then %do;
			where (&where);
			%end;
		output out=_gmean_ mean=&x &y;
	%end;

proc iml;
start ellipse(c, df1, df2, mean, xx, npoints, pvalue);
  /*----------------------------------------------------------------*
   |  Computes elliptical contours for a scatterplot                |
   |   C       returns the contours as consecutive pairs of columns |
   |   MEAN    2x1 mean vector                                      |
   |   XX      2x2 covariance matrix for the data                   |
   |   NPOINTS scalar giving number of points around a contour      |
   |   PVALUE  confidence coefficient                               |
   *----------------------------------------------------------------*/
 
   call eigen(v, e, xx);
 
   *-- Set contour levels --;
   c =  2*finv(pvalue,df1,df2,0);  * / df; *print c;
   a = sqrt(c* max(v[ 1 ],0) );
   b = sqrt(c* max(v[ 2 ],0) );
 
   *-- Parameterize the ellipse by angles around unit circle --;
   t = ( (1:npoints) - {1}) # atan(1)#8/(npoints-1);
   s = sin(t);
   t = cos(t);
   s = s` * a;
   t = t` * b;
 
   *-- Form contour points --;
   s = ( ( e*(shape(s,1)//shape(t,1) )) );
	s = s +  mean @ j(1,npoints*ncol(c),1)  ;
	s = t(s);
   c = shape( s, npoints);
   *-- C returned as NCOL pairs of columns for contours--;
	finish;

*-- Critical value for lambda_max = Roy test;
start lambda_crit( alpha, p, dfh, dfe );
	df1 = max(p, dfh);
	df2 = dfe - df1 + dfh;
	lambda = (df1/df2) * finv(1-alpha, df1, df2);
	return(lambda);
	
finish;

*--- main routine;

   use &stat;
   read all var{&x &y} where(_type_='ERROR') into e[r=_name_ c=var];
   read all var{df}    where(_type_='ERROR') into dfe[r=_name_];
   dfe = dfe[1];
   p = nrow(e);
   read all var{&x &y} 
    	where(_TYPE_=%upcase("&ss") & _SOURCE_=%upcase("&effect")) 
		into h[r=_name_ c=var];
   read all var{DF}    
    	where(_TYPE_=%upcase("&ss") & _SOURCE_=%upcase("&effect")) 
		into dfh[r=_source_];
	dfh = dfh[1];
*   read all var{_source_ _type_}
        where (_type_^='ERROR') into id;
	*print dfh;
*	reset noprint;
   close &stat;

	if nrow(e)>2 then do;
		w = loc( upcase("&x")=upcase(_name_) ) || 
		    loc( upcase("&y")=upcase(_name_) );
		e = e[w,];
		h = h[w,];
		end;

	mean = {0, 0};
%if %length(&data) %then %do;
	use _gmean_;
	read all var{&x &y} into mean;
	read all var{_freq_} into n;
	mean = t(mean);
	%end;

    I = I(2);
    m1 = &M1;
    m2 = &M2;
   alpha=&alpha;

	if &sf = 1 
		then factor = 1;
		else factor = lambda_crit( alpha, p, dfh, dfe );
	print factor;

    %if "&M1"="H" %then %do;
        df = dfh;
        m1 = &s1 # (h / dfe) / factor;  *-- ?? ;
        %end;
    %else %if "&M1"="H+E" %then %do;
        df = dfh + dfe;
        m1 = &s1 # ((h+e) / (df)) / factor ;  *-- ?? ;
        %end;


    %if "&M2"="E" %then %do;
        df = dfe;
        m2 = &s2 # e / dfe;
        %end;
	%if "&M2"="I" %then %do;
		eh = inv(half(e));
		m1 = &s1 # eh * h * t(eh);
		m2 = &s2 # I(2);
		%end;
		
*   print 'H and E matrices', dfh dfe,
       h[r=var c=var], e[r=var c=var];
	%if %length(&print) %then %do;
   print "M1 (&M1) and M2 (&M2) matrices [s1=&s1, s2=&s2]", 
       m1[r=var c=var], m2[r=var c=var];
	%end;

   np = 60;
   run ellipse(c1, 2, dfe, mean, M1, np, &pvalue);
   run ellipse(c2, 2, dfe, mean, M2, np, &pvalue);
   c = (j(np,1,1) || c1)
     //(j(np,1,2) || c2);
   vars = {mat}||var;
   create &out from c[c=vars];
   append from c;
quit;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
*options notes;

%if %length(&efflab) %then %do;
/*
   proc summary data=&out nway;
	   var &x &y;
	   where mat=1;
	   output out=minmax min=minx miny max=maxx maxy;
   proc corr noprint data=&out outp=_corr_;
	   var &x &y;
	   where mat=1;
   data _corr_;
	   set _corr_;
	   where _name_= upcase("&x");
	   r = &y;
	   keep r;

   data _efflab_;
	   set minmax (drop=_type_ _freq_);
	   set _corr_;
   *	if mat=1;
	   xsys='2'; ysys='2';
	   length function color $8;
	   y = maxy;
	   if r>0 
		   then x = maxx;
		   else x = minx;
	   function = 'label   ';
	   color = "&c1";
	   text = "&efflab";
	   if x < 0 then position = '1';
		   else position = '3';
	   position='2';
*   proc print;
*/
	*-- find min/max y and the corresponding x values;
   proc summary data=&out nway;
	   var &y;
	   where mat=1;
       output out=minmax(drop=_type_ _freq_)
           min=miny max=maxy 
           idgroup (min(&y) out[1] (&x)=minx)
           idgroup (max(&y) out[1] (&x)=maxx)
           ;
   data _efflab_;
	   set minmax;
	   xsys='2'; ysys='2';
	   length function color $8;
	   %if &effloc=MAX %then %do;
	      y = maxy;
	      x = maxx;
		  position='2'; *-- or B;
	      %end;
       %else %do;
	      y = miny;
	      x = minx;
		  position='8'; *-- or E;
	      %end;
		
	   function = 'label   ';
	   color = "&c1";
	   text = "&efflab";
   %end; 

%if %length(&data) %then %do;
	%*-- Transfer variable labels to &out data set, trickily;
	data &out;
		set &data(obs=0 keep=&x &y)
			&out;
	%end;

proc format;
	value mat 
	%if "&M2"="I" %then %quote(1='eHe (e=E^-1/2)' 2='Identity');
	%else %if "&M1"="H"   %then %quote(1='Hypothesis' 2='Error');
	%else %if "&M1"="H+E" %then %quote(1='Hyp+Error' 2='Error');
		;
	run;	

%if %length(&data) %then %do;
	data _center_;
	set _gmean_;
	xsys='2'; ysys='2';
	length function color $8 text $16;
	text = 'PLUS';
	x=&x; y=&y;
	hsys = '4'; size = 4; color='green';
	function = 'SYMBOL';      output;

	%if %length(&efflab) %then %do;
	   data _center_;
	    set _center_ _efflab_;
	   %end;

	*-- Plot the canonical vectors;
	%if %index(%upcase(&add), CANVEC) %then %do;
		%canvec(data=&data, stat=&stat, x=&x, y=&y, effect=&effect, out=_canvec_);
		%if &syserr > 4 %then %do;
			%put WARNING: (&me) ADD=CANVEC requested, but this step failed.;
			%end;
		%else %do;
		   data _center_;
	    		set _center_ _canvec_;
			%end;
		%end;

	%if %length(&class) %then %do;
		%if  %index(&class,*)>0 %then %do;
			%let class = %sysfunc(translate(&class,  %str( ),%str(*)));
			%end;
			
	%*put class=&class effect=&effect;
	   %hemeans(data=&data, x=&x, y=&y, effect=&effect, class=&class, where=&where,
		   gpfmt=&gpfmt, htext=&htext, hsym=&hsym, out=_means_);
	   data _center_;
		   set _means_ _center_ 
		   ;
	   %end;
	%end;

%if %length(&anno) %then %do;
	data _center_;
		set &anno _center_;
	%end;

%if %length(&vaxis)=0 %then %do;
	axis99 label=(a=90 r=0);
	%let vaxis=axis99;
	%end;

*goptions reset=symbol;
proc gplot data=&out gout=&gout;
   where (mat in (&mplot));
   plot &y * &x = mat / frame vm=1 hm=1 anno=_center_
	%if %length(&haxis)>0 %then %str(haxis=&haxis);
	%if %length(&vaxis)>0 %then %str(vaxis=&vaxis);
	%if &legend=NONE %then nolegend;
	%else %if %length(&legend)>0 %then %str(legend=&legend);
	name = "&name"
	des = "HEplot of &effect for &y and &x, M1=&M1 M2=&M2"
	;
   symbol1 v=none i=join l=&l1  c=&c1 w=&w1;
   symbol2 v=none i=join l=&l2  c=&c2 w=&w2;
	label mat='Matrix';
	format mat mat.;
run; quit;

proc datasets lib=work memtype=data nolist nowarn;
   delete _center_ _means_ _gmean_;
   run; quit;

%done:

	%*-- Restore global options;
	%if &sysver >= 7 %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%if &abort %then %put ERROR: The &me macro ended abnormally.;

%mend;

%macro hemeans(
	data=&data,
	x=,
	y=,
	htext=,
	hsym=,
	class=,
	effect=,
	gpfmt=,
	where=,
	out=_means_
	);

%***--- this will only work for a main effect.  Interactions handled outside;
%if %index(&effect,*) %then %do;
	%end;
proc summary data=&data nway;
	class &class;
	var &x &y;
	%if %length(&where) %then %do;
		where (&where);
		%end;
	output out=_means_ mean=&x &y;

%if %length(%scan(&class, 2, %str( ))) %then %do;
	%cat(data=_means_, var=&class, out=_means_, catvar=text);
	*proc print;
	%end;
	
/*
proc standard data=_means_ m=0 out=_means_;
	var &x &y;
	freq _freq_;
*/
data &out;
	length function $8 text $16;  %*-- allow 16-char labels;
	if _n_=1 then do;     %*-- this construction retains gx, gy;
		set _gmean_;
		gx = &x;
		gy = &y;
		end;
	set _means_;
	xsys='2'; ysys='2';
	drop &x &y gx gy _type_;
	x = &x;
	y = &y;
	if x > gx
		then position = '6';
		else position = '4';
	function = 'label   ';
	%if %length(%scan(&class, 2, %str( )))=0 %then %do;
	   %if %length(&gpfmt)  
		   %then %str(text=' ' || left(put(&effect,&gpfmt)) || ' ';);
		   %else %str(text=' ' || left(&effect) || ' ';);
	   %if %length(&htext)
		   %then %str(size=&htext;);
		%end;
	%else %str(text= ' ' || trim(left(text)) || '';);  
	output;
	%if %length(&hsym) %then %do;
		size = &hsym;
		%end;
	function = 'symbol'; text='dot'; output;
%mend;

/*
 Catenate a set of variable values, with a specified separator.

 Useful for situations were you need to combine a number of factor
 variables into a single variable.  V9 has the function 
 catx(sep, str1, str2, ...), that does something similar, and
 other functions, cat(str1, ...), cats(str1, ...), catt(str1, ...)
 that don't provide an explicit separator argument.
*/
%macro cat(
	data=_last_,   /* input dataset */
	var=,          /* list of variables to be concatenated */
	catvar=,       /* output result variable */
	sep=:,         /* separator string */
	length=,       /* length of result variable */
	out=&data      /* name of output dataset */
	);

%local catstr i v;
data &out;
	set &data;
	%if %length(&length) %then %do;
		length &catvar $ &length;
		%end;
	%let catstr = trim(left(%scan(&var,1,%str( ))));
	%let i=2;
	%let v=%scan(&var, &i, %str( ));
	%do %while (%length(&v) > 0 );
		%let catstr = &catstr || "&sep" || trim(left(%scan(&var,&i,%str( ))));
		%let i = %eval(&i+1);
		%let v=%scan(&var, &i, %str( ));
		%end;
	%put CAT: &catvar = &catstr;
	&catvar = &catstr;
	
%mend;

/*
Add canonical vectors to the plot
*/

%macro canvec(
	data=,
	stat=,
	var=,
	x=,
	y=,
	effect=,
	text=lowcase(_name_),
	color=green,
	out=_canvec_
	);


%if %length(&x)=0 %then %let x = %scan(&var,1);
%if %length(&y)=0 %then %let y = %scan(&var,2);

proc summary data=&data;
	var &x &y;
	output out=_means_ mean=m&x m&y
				std=s&x s&y;
	
* TODO: test whether the &stat dataset contains _type_='STRUCTUR';

data &out;
	if _n_=1 then
		set _means_(keep=m&x m&y s&x s&y);
	set &stat(keep=_name_ _type_ _source_ &x &y);
	where _type_='STRUCTUR' and _source_="&effect";

	retain xsys ysys '2';
	length function color $8;
	drop _type_ &x &y m&x m&y s&x s&y;

	x = m&x; y = m&y; 
	function = 'move'; output;
	x = m&x + s&x * &x; 
	y = m&y + s&y * &y; 
	color="&color";
	function = 'draw'; output;

	text = &text;
	function = 'label'; output;
	run;
%mend;
	
