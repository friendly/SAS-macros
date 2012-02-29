 /*--------------------------------------------------------------*
  *    Name: heplots.sas                                         *
  *   Title: Plot Hypothesis and Error matrices for an MLM       *
        Doc: http://www.datavis.ca/sasmac/heplots.html     
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 21 Oct 2006 10:43:20                                *
  * Revised: 17 Dec 2009 10:41:40                                *
  * Version: 1.2-1                                               *
  *--------------------------------------------------------------*/
 /**
=ChangeLog:
    1.1  Added EFFLOC= and fixed positioning of effect labels     
         Allow labeling the E ellipse                             
         Apply HTEXT to effect labels                             
         Fixed small bugs when subset of terms is plotted         
         Fixed small bugs with contrasts                          
         Fixed buglets with ANNO=  and Intercept                  
    1.2  Added PLOTOPT= for additional plot options               
         Implemented PRINT= to control printed output                       
 **/

 /*=
=Description:
 
 The HEPLOTS macro plots the covariance ellipses for one or more
 hypothesized (H) effects and for error (E) for two variables from a
 multivariate linear model (MLM).  For a MANOVA effect, the plot helps to
 show how the means of the groups differ on the two variables jointly,
 in relation to the within-group variation. 
 
 The original HEPLOT macro does this for a single H effect. The HEPLOTS
 macro also provides a scaling of the H ellipses (SIZE=EVIDENCE) so that
 an H ellipse will protrude beyond the E ellipse, somewhere in the full
 p-variable space (not necessarily for the chosen X= and Y= variables),
 if and only if the Roy greatest-root test is significant at the specified
 ALPHA= level.
 
 One alternative two-dimensional view is provided by the
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



=Usage:

 The HEPLOT macro is defined with keyword parameters.  The STATS=
 parameter and either the VAR= or the X= and Y= parameters are required.
 You must also specify the EFFECTS= parameter, indicating the H matrices
 to be extracted from the STATS= data set.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	proc glm data=dataset outstat=HEstats;
		model y1 y2  = A B A*B / ss2;
		manova;
	%heplots(data=dataset, stat=HEstats, var=y1 y2, effects=A B A*B );
 
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

* EFFECT=     Name(s) of the MODEL effect(s) to be displayed as H matrices.
              This must be a blank-separated list of one or more
              terms on the right hand side of the MODEL statement used
			  in the PROC GLM or PROC REG step, in the same format that this
			  efffect is labeled in the STAT= dataset. These must be among
			  the values of the _SOURCE_ variable contained in the STAT= 
			  dataset.
			  One limitation is that to plot the effects for CONTRAST statements,
			  the contrast labels cannot contain spaces.


* EFFECTS=    Synonym for EFFECT=

* CLASS=      Names of class variables(s) [Default: CLASS=&EFFECT]

* EFFLAB=     Label(s) (up to 16 characters, no embedded spaces) for the H effects, 
              annotated near the upper corner of the ellipses.
			  Use EFFLAB=&EFFECT Error to also label the E ellipse. 
			  [Default: EFFLAB=&EFFECT]

* EFFLOC=     Location for the effect label: MAX (above) or MIN (below). Can
              be a list corresponding to the effects. [Default: EFFLOC=MAX]

* MPLOT=      Matrices to plot, a list of integers from 1 to the number of
              effects

* GPFMT=      The name of a SAS format for levels of the group/effect variable used
              in labeling group means.

* SIZE=       How to scale H ellipse(s) relative to E. If SIZE=EVIDENCE or SIZE=SIGNIF,
              the H ellipses are scaled so a significant hypothesis ellipse extends outside the 
              E ellipse. If SIZE=EFFECT the H ellipses are plotted on the same scale as the
			  E ellipse. [Default: SIZE=EVIDENCE]

* ALPHA=      Significance level for Roy greatest-root test, if SIZE=EVIDENCE. [Default: ALPHA=0.05]

* LEVEL=      Coverage proportion for the E ellipse for normally distributed errors. [Default: LEVEL=0.68]

* SS=         Type of SS to extract from the STAT= dataset. This SS type must have been specified
              on the MODEL statement in PROC GLM. [Default: SS=SS3]

* WHERE=      To subset the DATA= dataset

* ANNO=       Name of an input annotate data set, used to add additional
              information to the plot of y * x.

* NP=         Number of points on each ellipse [Default: NP=40]

* VAXIS=      Name of an axis statement for the y variable

* HAXIS=      Name of an axis statement for the x variable

* LEGEND=     Name of a LEGEND statement to identify the ellipses.  Rarely needed,
              because the ellipses are normally labeled via the EFFLAB= parameter.
			  [Default: LEGEND=NONE]

* COLORS=     List of colors for the H and E ellipses.  The last value is used for the
              E ellipse; all but the last value are repeated as necessary for the
			  hypothesized EFFECT= ellipses. [Default: COLORS=BLACK RED]

* LINES=      Line styles for the H and E ellipses.  The last value is used for the
              E ellipse; all but the last value are repeated as necessary for the
			  hypothesized EFFECT= ellipses. [Default: LINES=1 21]

* WIDTH=      Line widths for the H and E ellipses.  The last value is used for the
              E ellipse; all but the last value are repeated as necessary for the
			  hypothesized EFFECT= ellipses. [Default: WIDTH=3 2]

* HTEXT=      Height of text labels

* HSYM=       Height of point symbols (means)

* PLOTOPT=    Additional options for PLOT statement

* PRINT=      Control details of printed output [Default: PRINT=0] 

* OUT=        The name of the output data set [Default: OUT=HEOUT]

* OUTEFF=     Name of output dataset describing effects [Default: OUTEFF=OUTEFF]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=HEPLOT]

* GOUT=       The name of the graphics catalog [Default: GOUT=GSEG]
                
=References:

 Friendly, M. (2006).
   Data Ellipses, HE Plots and Reduced-Rank Displays for Multivariate Linear 
   Models: SAS Software and Examples. 
   I<Journal of Statistical Software>, 17(6), 1-42.
   L<http://www.jstatsoft.org/v17/i06/>

 Friendly, M. (2007).
   HE plots for Multivariate General Linear Models.
   I<Journal of Computational and Graphical Statistics>, 16(2) 421--444.
   L<http://www.datavis.ca/papers/heplots.pdf>

 J. Fox and M. Friendly and G. Monette (2009).
   Visualizing hypothesis tests in multivariate linear models: The heplots
   package for R. I<Computational Statistics>, 24(2), 233-246.
 =*/

%macro heplots(
	stat=,          /* name of OUTSTAT= dataset from proc glm       */
	data=,          /* name of the input dataset (for means)        */
	x=,             /* name of horizontal variable for the plot     */
	y=,             /* name of vertical variable for the plot       */
	var=,           /* 2 response variable names: x y               */
	effect=,        /* name(s) of MODEL effect to be displayed      */
	effects=,       /* synonym for EFFECT=                          */
	class=&effect,  /* names of class variables(s)                  */
	efflab=&effect, /* optional label for each H matrix              */
	effloc=max,     /* location for the label (min/max)             */
	mplot=,         /* matrices to plot                             */
	gpfmt=,         /* format for levels of the group/effect var    */
	size=evidence,  /* how to scale H ellipse(s) relative to E      */
	alpha=0.05,     /* signif level for Roy test, if size=evidence  */
	level=0.68,     /* coverage proportion for E ellipse            */
	ss=ss3,         /* type of SS to extract from the STAT= dataset */
	where=,         /* to subset the DATA= dataset                  */
	anno=,          /* name of an input annotate data set           */
	np=40,          /* Number of points on each ellipse             */
	vaxis=,         /* name of an axis statement for the y variable */
	haxis=,         /* name of an axis statement for the x variable */
	legend=none,    /* name of a LEGEND statement                   */
	colors=BLACK RED,     /* colors for the H and E ellipses        */
	lines=1 21,     /* line styles for the H and E ellipses         */
	width=3 2,      /* line widths for the H and E ellipses         */
	htext=,         /* height of text labels                        */
	hsym=,          /* height of point symbols (means)              */
	plotopt=,       /* additional options for PLOT statement        */
	print=0,        /* control details of printing                  */
	out=heout,      /* name of the output dataset                   */
	outeff=outeff,  /* name of output dataset describing effects    */
	name=heplot,    /* name of the graphic catalog entry            */
	gout=gseg       /* name of the graphic catalog                  */
	);

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;

%local me; %let me = HEPLOTS;
%if %length(&x)=0 %then %let x = %scan(&var,1);
%if %length(&y)=0 %then %let y = %scan(&var,2);

%*-- Check for required parameters;
%local abort;
%let abort=0;
%if %length(&x)=0 or %length(&y)=0 %then %do;
	%put ERROR: (&me) The X= and Y= variables (or VAR=x y) must be specified;
	%let abort=1;
	%goto done;
	%end;

%if %length(&stat)=0 %then %do;
	%put ERROR: (&me) The STAT= data set (OUTSTAT= from GLM) is required;
	%let abort=1;
	%goto done;
	%end;
%else %do;
/*
	data &stat;
		set &stat;
		_SOURCE_ = upcase(_SOURCE_);
*/
	%end;

%if %length(&effect)=0 %then %do;
	%let effect = &effects;
	%end;
%let effloc=%upcase(&effloc);
	
%let ss = %upcase(&ss);
%let legend = %upcase(&legend);


%local sf;
%let size=%substr(%upcase(&size),1,3);
%if &size=EVI or &size=SIG
	%then %let sf=2;
	%else %let sf=1;


%if %length(&data) %then %do;
	proc summary data=&data nway;
		var &x &y;
		%if %length(&where) %then %do;
			where (&where);
			%end;
		output out=_gmean_ mean=&x &y;
	%end;

proc iml;

start ellipse2(ell, mean, cov, radius, npoints);

reset print;
   call eigen(roots, vectors, cov);
   angles = ( t(1:npoints) - 1) # atan(1)#8/(npoints-1);
   circle = radius # ( sin(angles) || cos(angles) );
*   ell = circle * diag (max(roots, 0)) * vectors;
   ell = circle  * t(vectors) * diag (max(roots, 0));
   ell = ell + J(npoints, 1) * shape( mean, 1, 2);
reset noprint;
   
finish;

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
   s =  ( e*(shape(s,1)//shape(t,1) )) ;
	s = s +  mean @ j(1,npoints*ncol(c),1)  ;
	s = t(s);
   c = shape( s, npoints);
   *-- C returned as NCOL pairs of columns for contours--;
	finish;

start symput(name, val);
   if type(val) ='N'
      then value = trim(char(val));
      else value = val;
   call execute('%let ', name, '=', value, ';');
   finish;

/*
 this module is not used; retained temporarily for definitions of
 test statistics used to scale to evidence
*/
start mstats(h, e, dfh, dfe, stats, df, alpha, tests)
      global(verbose);
   call geneig(roots, vectors, h, e);
   theta = roots / (1+roots);
   p = nrow(h);
   s = min(p,dfh);                   * non-zero roots;
   m = .5#(abs(p-dfh)-1) ;
   n = (dfe - p - 1)/2;
   reset name fuzz;
   if verbose>1 then print roots theta s m n;
   free stats df pow;
 
      *-- Wilks lambda --;
   if any(tests=1) then do;
      lambda  = (1/(1+roots[1:s]))[#];
      pdf = p##2 + dfh##2 -5;
      if pdf <=0
         then t = 1;
         else t = sqrt((p##2 # dfh##2 - 4) / (p##2 + dfh##2 - 5));
      a = dfe +dfh - (p+dfh+1)/2;
      df1= p#dfh;
      df2= (t#a)- (df1/2) + 1;
      fw = ((1 - lambda##(1/t)) / df1)
         / ((    lambda##(1/t)) / df2);
      eta= 1 - lambda##(1/s);
      stats = (lambda || fw);
      df = df1 || df2;
      end;
 
      *-- Pillai trace --;
   if any(tests=2) then do;
      pillai = sum(theta[1:s]);
      df1 = s # (2#m + s + 1);
      df2 = s # (2#n + s + 1);
      fp  = (pillai/(s-pillai)) # (df2/df1);
      eta = pillai/s;
      stats = stats // (pillai || fp);
      df = df // (df1 || df2);
      end;
 
      *-- Lawley-Hotelling trace --;
   if any(tests=3) then do;
      lawley = sum(roots[1:s]);
	  df1 = 2#m + s + 1;
      df2 = 2#(s#n + 1);
      fl = (lawley/s) # (df2/df1);
      eta= lawley / sum( 1 / (1 - (theta[1:s])) );
      stats = stats // (lawley || fl);
      df = df // (df1 || df2);
      end;
 
      *-- Roy maximum root --;
   if any(tests=4) then do;
      roy = roots[1];
      df1 = max(p,dfh);
      df2 = dfe - df1 + dfh;
      fr  = roy # (df2/df1);
      eta = theta[1];
      stats = stats // (roy || fr); 
	  df = df // (df1 || df2);
      end;
 
      if verbose>0 then do;
      sname = {"Wilks' Lambda" "Pillai's Trace" "Lawley Trace"
               "Roy's max. Root"}[union(tests)];
      reset noname;
	  prob = 1-probf(stats[,2], df[,1], df[,2]);
      print stats[r=sname c={"Value" "F"} format=8.3] 
            df [c={"df1" "df2"} format=5.0]
	  		prob[c={"ProbF"} format=6.4]
			;
      end;
	  reset name;
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
   *reset print name;
   p = nrow(e);
   *-- select rows corresponding to chosen variables;
   if nrow(e)>2 then
	   w = loc( upcase("&x")=upcase(_name_) ) || 
		   loc( upcase("&y")=upcase(_name_) );
   else w=1:2;
   e = e[w,];

	*-- TODO: select SSH according to _TYPE_;
 *  reset print;
 *  reset name;
   read all var{&x &y}   where(_type_^='ERROR') into ssh[r=_name_ c=var];
   read all var{df}      where(_type_^='ERROR') into dfs[r=_source_];
   read all var{_source_ _type_}
        where (_type_^='ERROR') into id;
	*print "Read effects as", ssh dfs id;

/*
   read all var{&x &y} 
    	where(_TYPE_=%upcase("&ss") & _SOURCE_=%upcase("&effect")) 
		into h[r=_name_ c=var];
   read all var{DF}    
    	where(_TYPE_=%upcase("&ss") & _SOURCE_=%upcase("&effect")) 
		into dfh[r=_source_];
	*dfh = dfh[1];
*/
	reset noprint;
   close &stat;

	*-- Create mean vector;
	mean = {0, 0};
%if %length(&data) %then %do;
	use _gmean_;
	read all var{&x &y} into mean;
	read all var{_freq_} into n;
	mean = t(mean);
	close _gmean_;
	%end;

   np=&np;
   vars = {mat}||var;
      
   neff = nrow(ssh) / p;
   nmat = neff+1;
   
   verbose = &print;
   alpha=&alpha;
   scale = 1 / dfe;
   size_factor=&sf;
   radius = sqrt(2 # finv(&level, 2, dfe));
   mat=0;
   do i = 1 to neff;
      r1 = 1 + p#(i-1);    *-- range of rows for this effect;
      r2 =     p#i;
      dfh= dfs[r1];
      h = ssh[r1:r2,];     *-- extract all rows;
	  h = h[w,];           *-- subset for chosen x,y;
      effect = trim(id[r1,1]);
	  type = trim(id[r1,2]);
	  s = min(dfh, p);     *-- # non-zero roots;
	*-- Allow for &effect=_ALL_;
	*-- TODO: select type=&ss;  
	  if ((%upcase("&ss") = type) | type='CONTRAST') &
	     (%upcase("&effect")='_ALL_' | 
	     index(%upcase("&effect"), upcase(effect))) then do;
		mat = mat+1;

	  	%*-- Select forms for M1 and M2;
*        m1 = &s1 # (h / dfe);  *-- ?? ;
		if &sf = 1 
			then factor = 1;
			else factor = lambda_crit( alpha, p, dfh, dfe );
        m1 = h # scale / factor; 
		if verbose> 0 then print effect type h[f=8.3] m1[f=8.3];
		if s>1
			then pts=np;
			else pts=8;
    	run ellipse(c1, 2, dfe, mean, M1, pts, &level);
*		run ellipse2(c1, mean, M1, radius, pts);
		_source_ = j(pts,1,effect);
        c = (j(pts,1,mat) || c1);
		if mat=1 then create &out from c[c=vars r=_source_];
        append from c[r=_source_];
		test=1; 
        *run mstats(h, e, dfh, dfe, stats, df, alpha, test);

		eff = eff // effect;
		typ = typ // type;
		dff = dff // dfh;
	  	end;
		
      end;
   run symput('neff', mat);
   nmat=mat+1;
   run symput('nmat', nmat);

	*-- E matrix as mat=nmat;
*   m2 = &s2 # e /dfe;
   m2 =  e # scale;
   run ellipse(c2, 2, dfe, mean, M2, np, &level);
*   run ellipse2(c2, mean, M2, radius, np);
   c = (j(np,1,nmat) || c2);
   effect = 'Error'; type=effect;
   _source_ = j(np,1,effect);
   append from c[r=_source_];
	eff = eff // effect;
	typ = typ // type;
	dff = dff // dfe;
	create &outeff var{eff typ dff};
	append;

quit;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%put NOTE: (&me): &neff effects selected: &effect;
%let colors = %repeatw( %butlast(&colors), &neff) %scan(&colors, -1);
%put NOTE: (&me): colors expanded to: &colors;
%let lines = %repeatw( %butlast(&lines), &neff) %scan(&lines, -1);
%put NOTE: (&me): lines expanded to: &lines;
%let width = %repeatw( %butlast(&width), &neff) %scan(&width, -1);
%put NOTE: (&me): width expanded to: &width;
%let effloc = %repeatw( &effloc, %eval(&neff+1));
%put NOTE: (&me): effloc expanded to: &effloc;

data &outeff;
	set &outeff;
	rename eff=effect typ = _type_ dff=df;
%if &print > 0 %then %do;
proc print data=&outeff;
%end;
data _fmt_;
	set &outeff(rename=(effect=label));
	retain fmtname 'mat';
	start = _n_;
	keep fmtname start label;
proc format cntlin=_fmt_;

%if %length(&data) %then %do;
	data _center_;
	set _gmean_;
	xsys='2'; ysys='2';
	length function color $8 text $32 ;
	text = 'PLUS';
	x=&x; y=&y;
	hsys = '4'; size = 4; color='green';
	function = 'SYMBOL';      output;

	%end;

%if %length(&data) %then %do;
	%*-- Transfer variable labels to &out data set, trickily;
	data &out;
		informat _source_ $16.;
		length mat 4;
		set &data(obs=0 keep=&x &y)
			&out;
		run;

	%if %length(&class) %then %do;
		%local nc;
		%let nc=1;
		%let eff = %scan(&class,&nc,%str( ));
		%let fmt = %scan(&gpfmt,&nc,%str( ));
			 %do %while(%quote(&eff)^= );
			 	%put &me: Finding means for &eff effect;
				%if  %index(&eff,*)>0 %then %do;
					%let cls = %sysfunc(translate(&eff,  %str( ),%str(*)));
					%end;
				%else %let cls = &eff;
				%let color=%scan(&colors, &nc, %str( ));
				*options mprint;
				%hemeans(data=&data, x=&x, y=&y, effect=&eff, class=&cls, 
					where=&where, gpfmt=&fmt, 
					htext=&htext, hsym=&hsym, color=&color,
					out=_means_);
				*options nomprint;
				*proc print data=_means_;
				data _center_;
		    		set _means_ _center_ ;
					run;

			 	%let nc = %eval(&nc+1);
				%let eff = %scan(&class,&nc,%str( ));
				%let fmt = %scan(&gpfmt,&nc,%str( ));
			 	%end;
		%end;

	%end; /* if %length (&data) */

%if %length(&efflab) %then %do;
	*-- find min/max y and the corresponding x values;
   proc summary data=&out nway;
	   var &y;
	   *where mat < &nmat;
	   by mat;
       output out=minmax(drop=_type_ _freq_)
           min=miny max=maxy 
           idgroup (min(&y) out[1] (&x)=minx)
           idgroup (max(&y) out[1] (&x)=maxx)
           ;

   data _efflab_;
	   set minmax;
	   by mat;
	   xsys='2'; ysys='2';
	   length function color $8;
	   loc = scan("&effloc", mat, ' ');
	   drop loc;
	   if loc='MAX' then do;
	      y = maxy;
	      x = maxx;
		  position='2'; *-- or B;
	      end;
       else do;
	      y = miny;
	      x = minx;
		  position='8'; *-- or E;
	      end;
		
	   function = 'label   ';
	   text = scan("&efflab", mat, ' ');
	   color = scan("&colors", mat);
	   %if %length(&htext)
		   %then %str(size=&htext;);
   data _center_;
	set _center_ _efflab_;
   %end; 

%if %length(&class) %then %do;
	%end;

%if %length(&anno) %then %do;
	data _center_;
		set &anno _center_;
	%end;


%if %length(&vaxis)=0 %then %do;
	axis99 label=(a=90 r=0);
	%let vaxis=axis99;
	%end;

proc gplot data=&out gout=&gout;
	%if %length(&mplot) %then %do;
		where (mat in (&mplot));
		%end; 

   plot &y * &x = mat / frame vm=1 hm=1 
    anno=_center_ &plotopt
	%if %length(&haxis)>0 %then %str(haxis=&haxis);
	%if %length(&vaxis)>0 %then %str(vaxis=&vaxis);
	%if &legend=NONE %then nolegend;
	%else %if %length(&legend)>0 %then %str(legend=&legend);
	name = "&name"
	des = "HEplot for &y and &x"
	;
   *symbol1 v=none i=join l=&l1  c=&c1 w=&w1;
   *symbol2 v=none i=join l=&l2  c=&c2 w=&w2;
   %gensym(n=&nmat, interp=join, symbols=none, line=&lines, width=&width,
   colors=&colors);
	label mat='Matrix';
	format mat mat.;
run; quit;

proc datasets lib=work memtype=data nolist nowarn;
   delete _center_ _means_ _gmean_;
   run; quit;

%done:

	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;

%if &abort %then %put ERROR: The &me macro ended abnormally.;
%exit:
%mend;

%macro hemeans(
	data=&data,
	x=,
	y=,
	htext=,
	hsym=,
	color=,
	class=,
	effect=,
	gpfmt=,
	where=,
	out=_means_
	);

%***--- this will only work for a main effect;
%if %index(&effect,*) %then %do;
	%end;
proc summary data=&data nway;
	%if %upcase(&class) ^= INTERCEPT %then %do;
		class &class;
		%end;
	var &x &y;
	%if %length(&where) %then %do;
		where (&where);
		%end;
	output out=_means_ mean=&x &y;

%if %length(%scan(&class, 2, %str( ))) %then %do;
	%cat(data=_means_, var=&class, out=_means_, catvar=text);
	proc print;
	%end;
	
data &out;
	length function $8 color $8 text $16;  %*-- allow 16-char labels;
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
	%if %length(&color) %then %do;
		color = "&color";
		%end;
	%if %length(&hsym) %then %do;
		size = &hsym;
		%end;
	output;
	function = 'symbol'; text='dot'; output;
	run;
%mend;

 /*------------------------------------------------------------*
  *    Name: repeatw                                           *
  *    Desc: Repeat words in a string to a length of n         *
  *------------------------------------------------------------*/

%macro repeatw(string, n, dlm=%str( ));

	%local k index;
	%let result=;
	%let nw = %words(&string);
	%do k=1 %to &n;
		%let index =%sysevalf(1+%sysfunc(mod(%eval(&k-1), &nw)));
		%* put index = &index;
		%let word = %scan(&string, &index, &dlm);
		%let result = &result &word;
		%end;
	&result
%mend;

 /*------------------------------------------------------------*
  *    Name: butlast                                           *
  *    Desc: Return all but the last word in a string          *
  *------------------------------------------------------------*/

%macro butlast(string, dlm=%str( ));
   %local count word result;
   %let count=1;
   %let word = %qscan(&string,&count,&dlm);
   %let result=;
   %do %while(&word^= );
       %let word = %qscan(&string,&count,&dlm);
	   %if %length(%qscan(&string,%eval(&count+1),&dlm))
	    %then %let result = &result &word ;
       %let count = %eval(&count+1);
   %end;
   %unquote(&result)
%mend;

