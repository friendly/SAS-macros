 /*--------------------------------------------------------------*
  *    Name: inflplot.sas                                        *
  *   Title: Influence plots for regression models               *
        Doc: http://www.datavis.ca/sasmac/inflplot.html    
  *                                                              *
  *  This SAS macro makes a plot of studentized residuals vs.    *
  *  leverage (hat-value), using COOK's D or DFFITS as the size  *
  *  of a bubble symbol.                                         *
  *--------------------------------------------------------------*
     Author:  Michael Friendly            <friendly@yorku.ca>     
    Created:  14 Nov 1993 10:42:11                                
    Revised:  17 Jan 2012 15:51:35                                
    Version:  1.3-1                                                 
     1.1  Added INFL= to control what's influential               
          Added HREF= to control ref lines for hat values         
          Fixed NAME=                                             
          Added VREF= to control ref lines for residuals          
          Added OUT=, OUTANNO= for output data sets               
          Added plot for COVRATIO                                 
     1.2 Added contour plots for CookD, DFFITS and CovRatio       
      Fixed some problems with labels                             
      Added internal documtentation
	 1.3 
	  Added BFILL= option for filled bubbles.  BFILL=gradient is useful
	  Added LFONT= option for font of bubble labels                            
                                                                  
  *--------------------------------------------------------------*/

 /*=
=Description:
 
 The INFLPLOT macro produces a variety of influence plots for a regression
 model -- plots of studentized residuals vs.  leverage (hat-value),
 using an influence measure (COOK's D, DFFITS, COVRATIO) as the size of
 a bubble symbol.  The plot show the components of influence (residual
 and leverage) as well as their combined effect.

 Plots can be produced either as bubble plots with PROC GPLOT or GCONTOUR
 plots of any of the influence measures overlaid with bubble symbols.
 The contour plots show how the influence measures vary with residual
 and leverage.  Horizontal reference lines in the plots delimit
 observations whose studentized residuals are individually or jointly
 (with a Bonferonni correction) significant. Vertical reference lines in
 the plot shows observations which are of "high leverage".

=Usage:

 The INFLPLOT macro is defined with keyword parameters. The Y= and
 X= parameters are required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%inflplot(Y=response, X=X1 X2 X3 X4, ...);
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* Y=          Name of the criterion variable.

* X=          Names of the predictors in the model.  Must be a blank-separated
              list of variable names.

* ID=         The name of an observation ID variable.  If not specified, observations
              are labeled sequentially, 1, 2, ...

* BUBBLE=     Influence measure shown by the bubble size.
              Specify one of COOKD, DFFITS, or COVRATIO [Default: BUBBLE=COOKD]

* CONTOUR=    Specifies influence measures shown as contours in the plot(s).
              One or more of COOKD, DFFITS, or COVRATIO.

* LABEL=      Points to label with the value of the ID variable in the plot: 
              One of ALL, NONE or INFL.  The choice INFL causes only influential
              points to be labelled. [Default: LABEL=INFL] 

* INFL=       Criterion for declaring an influential observation,  
              a logical expression using any of the variables in the
              output OUT= data set of regression diagnostics.
              The default is 
              
              INFL=%STR(ABS(RSTUDENT) > TCRIT
                OR HATVALUE > HCRIT
                OR ABS(&BUBBLE)  > BCRIT)

* LSIZE=      Observation label size.  The height of other text is controlled by
              the HTEXT= goption. [Default: LSIZE=1.5]

* LCOLOR=     Observation label color [Default: LCOLOR=BLACK]

* LPOS=       Observation label position, using a position value
              understood by the Annotate facility. [Default: LPOS=5]

* LFONT=      Font used for observation labels.
 
* BSIZE=      Bubble size scale factor [Default: BSIZE=10]

* BSCALE=     Scale for the bubble size. BSCALE=AREA makes the bubble area 
              proportional to the influence measure;  BSCALE=RADIUS makes the bubble
              radius proportional to influence. [Default: BSCALE=AREA]

* BCOLOR=     Bubble color [Default: BCOLOR=RED]

* BFILL=      Bubble fill? Options are BFILL=SOLID | GRADIENT, where the
              latter uses a gradient version of BCOLOR

* HREF=       Locations of horizontal reference lines. The macro variables
              HCRIT and HCRIT1 are internally calculated as 2 and 3 times the
			  average HAT value. [Default: HREF=&HCRIT &HCRIT1]

* VREF=       Locations of vertical reference lines. The program computes
              critical values of the t-statistic for an individual residual
			  (TCRIT) or for all residuals using a Bonferroni correction
			  (TCRIT1)  [Default: VREF=-&TCRIT1 -&TCRIT 0 &TCRIT &TCRIT1]

* REFCOL=     Color of reference lines [Default: REFCOL=BLACK]

* REFLIN=     Line style for reference lines. Use 0 to suppress. [Default: REFLIN=33]

* GPLOT=      Whether to draw the plot using PROC GPLOT, Y or N.  This may be useful
              if you use the CONTOUR= option and want to suppress the GPLOT version.

* OUT=        The name of the output data set containing regression diagnostics
              [Default: OUT=_DIAG_]

* OUTANNO=    Output data set containing point labels [Default: OUTANNO=_ANNO_]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=INFLPLOT]

* GOUT=       The name of the graphics catalog
                
==Dependencies:

  %gskip

==Example:

  %include macros(inflplot);
  %include data(duncan) ;
  %inflplot(data=duncan,
	 y=Prestige,       
	 x=Income Educ,  
	 id=job,
	 bubble=cookd,
	 bsize=14, lsize=2.5, bcolor=red,
	 out=infl, outanno=labels,
	 contour=cookd covratio);

 =*/

%macro inflplot(
     data=_last_,    /* Name of input data set                     */
     y=,             /* Name of criterion variable                 */
     x=,             /* Names of predictors                        */
     id=,            /* Name of observation ID variable (char)     */
     bubble=COOKD,   /* Bubble proportional to: COOKD or DFFITS    */
     contour=,       /* Show as contour plot(s)?                   */
     label=INFL,     /* Points to label: ALL, NONE, or INFL        */
     infl=%str(abs(rstudent) > tcrit
         or hatvalue > hcrit
         or abs(&bubble)  > bcrit),
     lsize=1.5,      /* obs label size.  The height of other       */
                     /* text is controlled by the HTEXT= goption   */
     lcolor=BLACK,   /* obs label color                            */
     lpos=5,         /* obs label position                         */
	 lfont=,
     bsize=10,       /* bubble size scale factor                   */
     bscale=AREA,    /* bubble size proportional to AREA or RADIUS */
     bcolor=RED,     /* bubble color                               */
	 bfill=,         /* fill bubbles?  SOLID|GRADIENT              */
     href=&hcrit &hcrit1,
     vref=-&tcrit1 -&tcrit 0 &tcrit &tcrit1,
     refcol=BLACK,   /* color of reference lines                   */
     reflin=33,      /* line style for reference lines; 0->NONE    */
     gplot=Y,        /* Produce the GPLOT?                         */
     out=_diag_,     /* output data set containing reg diagnostics */
     outanno=_anno_, /* output data set containing point labels    */
     name=INFLPLOT,
     gout=
     );
 

%local me; %let me=&sysmacroname;
%let abort=0;
%let nv = %numwords(&x);        /* number of predictors */
%if &nv = 0 %then %do;
    %put ERROR: List of predictors (X=) is empty;
	%let abort=1;
    %goto done;
    %end;
%if %length(&y) = 0 %then %do;
    %put ERROR: You must specify a Y= response variable;
	%let abort=1;
    %goto done;
    %end;


%if %length(&gout) %then %let gout= GOUT=&gout;
 
%let label=%upcase(&label);
%let bubble=%upcase(&bubble);
%if not ((%bquote(&bubble) = COOKD)
    or   (%bquote(&bubble) = DFFITS)
	or   (%bquote(&bubble) = COVRATIO)) %then %do;
    %put &me: BUBBLE=%bquote(&bubble) is not valid. BUBBLE=COOKD will be used;
    %let bubble=cookd;
    %end;
 
	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1;
		%let o1 = %sysfunc(getoption(notes));
		options nonotes;
		%end;
	%else %do;
	   options nonotes;
		%end;

proc reg data=&data noprint outest=_outest_;
   model &y = &x / influence;
   %if %length(&id) %then %do;
      id &id;
      %end;
   output out=&out h=hatvalue
           residual=residual
           student=student
           rstudent=rstudent
		   cookd=cookd
		   dffits=dffits
		   covratio=covratio
                   ;
	%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
 

   data &outanno;
      set &out nobs=n;
      length xsys $1 ysys $1 function $8 position $1 text $12 color $8 style $8;
      retain xsys '2' ysys '2' function 'LABEL' color "&lcolor" when 'A';
      retain tcrit hcrit bcrit tcrit1 hcrit1;
      drop tcrit hcrit bcrit  tcrit1 hcrit1 bcrit p;

      if _n_=1 then do;
	  	 p = &nv+1;
         tcrit  = tinv(.975, n-3);              *-- individual;
		 tcrit1 = tinv(1 - (0.05/(2*n)), n-3);  *-- Bonferroni;
         hcrit = 2 * (p)/n;
		 hcrit1= 3 * (p)/n;
         %if &bubble = COOKD
            %then %do; bcrit = 4/(n-p); %end;
         %else %if &bubble = DFFITS 
		 	%then %do; bcrit = 2 * sqrt(p/n); %end;
		 %else %do;  bcrit = 1 + 3*p/n; %end;         *-- s/b  abs(covratio-1) > 3p/n;

         put "INFLPLOT: Influential values for RSTUDENT, HAT and &bubble, using:";
		 put 'INFLPLOT:    abs(RSTUDENT)> ' tcrit 5.2 ', HAT> ' hcrit 5.3 " &bubble > " bcrit 5.3;
         call symput('tcrit',put(tcrit,5.2));
         call symput('hcrit',put(hcrit,5.3));
         call symput('tcrit1',put(tcrit1,5.2));
         call symput('hcrit1',put(hcrit1,5.3));
         call symput('n',put(n,5.));
         call symput('p',put(p,5.));
		  size = &lsize * .67;
		  xsys='2'; ysys='1'; x=hcrit;  y=1; text='2h'; position='B'; output;
		  xsys='2'; ysys='1'; x=hcrit1; y=1; text='3h'; position='B'; output;
		  xsys='1'; ysys='2'; x=99;  y=tcrit; text='t(.975)'; position='4'; output;
		  xsys='1'; ysys='2'; x=99;  y=-tcrit; text='t(.025)'; position='4'; output;
		  xsys='1'; ysys='2'; x=99;  y=tcrit1; text='B(.975)'; position='4'; output;
		  xsys='1'; ysys='2'; x=99;  y=-tcrit1; text='B(.025)'; position='4'; output;
         end;

   %if &label ^= NONE %then %do;
      xsys='2'; ysys='2';
      x=hatvalue;
      y=rstudent;
      %if &id ^= %str() %then %do;
         text = trim(left(&id));
         %end;
      %else %do;
         text = trim(left(put(_n_,3.0)));
         %end;
      size=&lsize;
      position="&lpos";
	  %if %length(&lfont) %then %do; 
		style="&lfont";
		%end;
      %if %upcase(&label) = INFL %then %do;
			if &infl then output;
         %end;
   %end;  /* &label ^= NONE */
   run;
 
proc summary data=&out;
	var rstudent hatvalue;
	output out=_minmax_ min=minr minh max=maxr maxh;

*-- Expand scales slightly to give nice values;
data _null_;
	set _minmax_;
	minr =     floor(minr);      call symput('minr',put(minr,5.0));
	maxr =     ceil(maxr);       call symput('maxr',put(maxr,5.0));
	minh = .1 *floor(10*minh);   call symput('minh',put(minh,5.1));
	maxh = .1 *ceil(10*maxh);    call symput('maxh',put(maxh,5.1));
	*put minr= maxr= minh= maxh=;
	run;

axis1 order=(&minr to &maxr) label=(a=90 r=0);
axis2 order=(&minh to &maxh by .05);
%let gplot = %substr(%upcase(&gplot),1,1);
%if &gplot=Y %then %do;
proc gplot data=&out &GOUT ;
  bubble rstudent * hatvalue = &bubble /
        annotate=&outanno
        frame
        vaxis=axis1 haxis=axis2 vminor=1 hminor=1
        %if &reflin ^= 0 %then %do;
        vref= &vref   lvref=&reflin  cvref=&refcol
        href= &href   lhref=&reflin  chref=&refcol
        %end;
        bsize=&bsize  bcolor=&bcolor  bscale=&bscale
		%if %length(&bfill) %then %do; 
			bfill=&bfill
			%end;
        des="Regression influence plot for &y"
		name="&name";
  label rstudent='Studentized Residual'
        hatvalue='Leverage (Hat Value)';
  format hatvalue 3.2;
  run; quit;
%end;

%let contour=%upcase(&contour);
%if %length(&contour) %then %do;


*-- calculate CookD, DFFITS and covratio over a grid;
proc summary data=&out;
	var residual hatvalue;
	output out=_minmax_ min=minr minh max=maxr maxh;
data _grid_;
	set _minmax_;
	set _outest_(keep=_rmse_);
	minr =     floor(minr);
	maxr =     ceil(maxr);
	minh = .1 *floor(10*minh);
	maxh = .1 *ceil(10*maxh);
	do residual=minr to maxr by (maxr-minr)/100;
		student = residual / ( _rmse_ * sqrt(1-hatvalue) );
		rstudent = student * sqrt( (&n-&p-1) / (&n-&p - student**2 ) );
		do hatvalue = minh to maxh by (maxh-minh)/100;
			cookd = ((residual / (_rmse_ * (1-hatvalue) ))**2 * hatvalue)/ &p;
			covratio = 1/((1-hatvalue)*((&n-&p-1 + rstudent**2)/(&n-&p))**&p);
			dffits = rstudent * sqrt(hatvalue / (1-hatvalue));
			output;
			end;
		end;
	label rstudent='Studentized residual'
		hatvalue = 'Leverage (Hatvalue)';

%points(data=&out, x=hatvalue, y=rstudent, 
	in=&outanno, out=_anno2_, symbol='circle', size=max(1,40*cookd));

proc summary data=_grid_;
	var cookd covratio dffits;
	output out=_minmax2_ min=mincd mincr mindf max=maxcd maxcr maxdf;

	%if %index(&contour, COOKD) %then %do;
	%gskip;
	*-- find cutoffs for CookD;
	data _null_;
		set _minmax2_;
		cookcut = 4/(&n-&p);  
		*put cookcut= maxcd=;
		length cdlevels $200;
		cdlevels = '0, ' || put(cookcut,best8.) ||' to ' || put(maxcd,best8.) || ' by '
			|| put( ((maxcd-cookcut)/6), best8.);
		put 'INFLPLOT: Shading levels for CookD:' cdlevels=;
		call symput('cdlevels', cdlevels);
		run;

/* Pattern statements generated via:
	%brewerpal(n=7, palette=YlOrRd, out=colors);
	pattern1 c=white    value=solid;
	%genpat(data=colors, n=nobs, colors=color, start=2, show=1);
*/
	pattern1 c=white    value=solid;
	pattern2 value=solid color=CXFFFFB2 repeat=1;
	pattern3 value=solid color=CXFED976 repeat=1;
	pattern4 value=solid color=CXFEB24C repeat=1;
	pattern5 value=solid color=CXFD8D3C repeat=1;
	pattern6 value=solid color=CXFC4E2A repeat=1;
	pattern7 value=solid color=CXE31A1C repeat=1;
	pattern8 value=solid color=CXB10026 repeat=1;

	proc gcontour data=_grid_ &GOUT ;
		plot rstudent * hatvalue = cookd /
			anno=_anno2_
			vaxis=axis1 haxis=axis2 hm=1 vm=1
			vref=-&tcrit1 -&tcrit 0 &tcrit &tcrit1  lvref=33 cvref=gray
			href=&hcrit &hcrit1    lhref=33 chref=gray
			nolegend 
			caxis=black
			pattern join   levels=&cdlevels
			name="&name" 
            des="Influence CookD contour plot for &y"
			;
		run; quit;
	%end;

	%if %index(&contour, COVRATIO) %then %do;
		%gskip;

	*-- find cutoffs for covratio;
	data _null_;
		set _minmax2_;
		cutlo = 1 - 3*(&p/&n);
		cuthi = 1 + 3*(&p/&n);
		*put cutlo= cuthi= mincr= maxcr=;
		length crlevels $200;
		crlevels = put(mincr, best8.) || ',' || put((mincr+cutlo)/2, best8.) || ', ' ||
		    	   put(cutlo, best8.) || ', 1, ' || put(cuthi, best8.) || ', ' ||
            	   put((maxcr+cuthi)/2, best8.) || ',' || put(maxcr, best8.);
		put 'INFLPLOT: Shading levels for covratio:' crlevels=;
		call symput('crlevels', crlevels);
		run;

/* Pattern statements generated via:
	%brewerpal(n=7, palette=RdYlBu, out=colors);
	%genpat(data=colors, n=nobs, colors=color, show=1);
*/
	pattern1 value=solid color=CXD73027 repeat=1;
	pattern2 value=solid color=CXFC8D59 repeat=1;
	pattern3 value=solid color=CXFEE090 repeat=1;
	pattern4 value=solid color=CXFFFFBF repeat=1;
	pattern5 value=solid color=CXE0F3F8 repeat=1;
	pattern6 value=solid color=CX91BFDB repeat=1;
	pattern7 value=solid color=CX4575B4 repeat=1;

	proc gcontour data=_grid_ &GOUT;
		plot rstudent * hatvalue = covratio /
			anno=_anno2_
			vaxis=axis1 haxis=axis2 hm=1 vm=1
			vref=-&tcrit1 -&tcrit 0 &tcrit &tcrit1  lvref=33 cvref=gray
			href=&hcrit &hcrit1    lhref=33 chref=gray
			nolegend 
			caxis=black
			pattern join  levels=&crlevels 
			name="&name" 
            des="Influence covratio contour plot for &y"
			;
		run; quit;
	%end;

	%if %index(&contour, DFFITS) %then %do;
		%gskip;

		*-- find cutoffs for dffits;
		data _null_;
			set _minmax2_;
			cutlo = -2*sqrt(&p/&n);
			cuthi =  2*sqrt(&p/&n);
			*put cutlo= cuthi= mindf= maxdf=;
			length dflevels $200;
			dflevels = put(mindf, best8.) || ',' || put((mindf+cutlo)/2, best8.) || ', ' ||
		    		   put(cutlo, best8.) || ', 0, ' || put(cuthi, best8.) || ', ' ||
            		   put((maxdf+cuthi)/2, best8.) || ',' || put(maxdf, best8.);
		    put 'INFLPLOT: Shading levels for dffits:' dflevels=;
			call symput('dflevels', dflevels);
			run;

	pattern1 value=solid color=CXD73027 repeat=1;
	pattern2 value=solid color=CXFC8D59 repeat=1;
	pattern3 value=solid color=CXFEE090 repeat=1;
	pattern4 value=solid color=CXFFFFBF repeat=1;
	pattern5 value=solid color=CXE0F3F8 repeat=1;
	pattern6 value=solid color=CX91BFDB repeat=1;
	pattern7 value=solid color=CX4575B4 repeat=1;

		proc gcontour data=_grid_ &GOUT;
			plot rstudent * hatvalue = dffits /
				anno=_anno2_
				vaxis=axis1 haxis=axis2 hm=1 vm=1
				vref=-&tcrit1 -&tcrit 0 &tcrit &tcrit1  lvref=33 cvref=gray
				href=&hcrit &hcrit1    lhref=33 chref=gray
				nolegend 
				caxis=black
				pattern join  levels=&dflevels 
				name="&name" 
            	des="Influence DFFITS contour plot for &y"
				;
	%end;
%end; /* %if %length(&contour) */

%done:
	%if &abort %then %put ERROR: The INFLPLOT macro ended abnormally.;
   proc datasets nolist nowarn;
      delete _outest_ _minmax_;
      %if %length(&contour) %then %do;
      delete _grid_ _minmax2_;
      %end;
   quit;
   %if %length(&contour) %then %do;
   pattern;
   %end;
 
%exit:
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1;
		%end;
	%else %do;
	   options notes;
		%end;

%mend;
 
%macro numwords(lst);
   %let i = 1;
   %let v = %scan(&lst,&i);
   %do %while (%length(&v) > 0);
      %let i = %eval(&i + 1);
      %let v = %scan(&lst,&i);
      %end;
   %eval(&i - 1)
%mend;

