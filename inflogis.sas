 /*-------------------------------------------------------------------*
  *    Name: inflogis.sas                                             *
  *   Title: Influence plot for logistic regression models            *
        Doc: http://www.datavis.ca/sasmac/inflogis.html            
  *                                                                   *
  *-------------------------------------------------------------------*
     Author:  Michael Friendly            <friendly@yorku.ca>          
    Created:  14 Nov 1993 10:42:11                                     
    Revised:  08 Jan 2012 15:44:00                                     
    Version:  1.4-1                                                   
     - Added TRIALS= parameter(for event/trials syntax)                
     - Added OUT= parameter                                            
     - Added INFL= parameter (what's influential?)                     
     - Refinements to labeling influential observations                
	 1.4 
	  Added BFILL= option for filled bubbles.  BFILL=gradient is useful
	  Added LFONT= option for font of bubble labels 
	  Sort descending by &bubble when BFILL specified                           
                                                                       
    Dependencies:  %gskip (needed for eps/gif only)                    
                                                                       
    From ``Visualizing Categorical Data'', Michael Friendly (2000)              
  *-------------------------------------------------------------------*/
 /*=
=Description:

This SAS macro produces influence plots for a logistic regression
model.  The plot shows a measure of badness of fit for a given
case (DIFDEV or DIFCHISQ) vs.  the fitted probability (PRED) or
leverage (HAT), using an influence measure (C or CBAR) as the
size of a bubble symbol.

=Usage:

 The inflogis macro is called with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example:

	%include data(arthrit);
	%inflogis(data=arthrit,
			y=better,
			x=_sex_ _treat_ age,
			id=case,
			);

==Parameters:

* DATA=       Specifies the name of the input data set to be analyzed.
              [Default: DATA=_LAST_]

* Y=          Name of the response variable

* TRIALS=     Name of trials variable (for event/trials syntax)

* X=          Names of predictors

* CLASS=      Names of class variables among predictors (V8)

* ID=         Name of observation ID variable (char)

* OUT=        Name of the output data set [Default: OUT=_DIAG_]

* GY=         Ordinate for plot: DIFDEV or DIFCHISQ [Default: GY=DIFDEV]

* GX=         Abscissa for plot: PRED or HAT [Default: GX=PRED]

* BUBBLE=     Bubble proportional to: C or CBAR [Default: BUBBLE=C]

* LABEL=      Points to label: ALL, NONE, or INFL [Default: LABEL=INFL]

* DEV=        DIFDEV/DIFCHISQ criterion for infl pts [Default: DEV=4]

* INFL=       Specifies the criterion used to determine which observations
              are influential (when used with LABEL=INFL).
              [Default: INFL=%STR(DIFCHISQ > &DEV OR &BUBBLE > 1)]

* LSIZE=      Observation label size.  The height of other text is controlled
              by the HTEXT= goption. [Default: LSIZE=1.5]

* LCOLOR=     Observation label color [Default: LCOLOR=BLACK]

* LPOS=       Observation label position [Default: LPOS=5]

* LFONT=      Font used for observation labels.
 
* BSIZE=      Bubble size scale factor [Default: BSIZE=10]

* BSCALE=     Bubble size proportional to AREA or RADIUS [Default: BSCALE=AREA]

* BCOLOR=     Bubble color [Default: BCOLOR=BLACK]

* BFILL=      Bubble fill? Options are BFILL=SOLID | GRADIENT, where the
              latter uses a gradient version of BCOLOR

* REFCOL=     Color of reference lines [Default: REFCOL=BLACK]

* REFLIN=     Line style for reference lines; 0->NONE [Default: REFLIN=33]

* LOPTIONS=   Options for PROC LOGISTIC [Default: LOPTIONS=NOPRINT]

* NAME=       Name of the graph in the graphic catalog [Default: NAME=INFLOGIS]

* GOUT=       Name of the graphics catalog

 =*/

%macro inflogis(
   data=_last_,    /* Name of input data set                  */
   y=,             /* Name of criterion variable              */
   trials=,        /* Name of trials variable                 */
   x=,             /* Names of predictors                     */
	class=,         /* Names of class variables (V8+)          */
   id=,            /* Name of observation ID variable (char)  */
   out=_diag_,     /* Name of the output data set             */
   gy=DIFDEV,      /* Ordinate for plot: DIFDEV or DIFCHISQ   */
   gx=PRED,        /* Abscissa for plot: PRED or HAT          */
   bubble=C,       /* Bubble proportional to: C or CBAR       */
   label=INFL,     /* Points to label: ALL, NONE, or INFL     */
   infl=%str(difchisq > &dev or &bubble > 1 or hat>hcrit1),
   dev=4,          /* DIFDEV/DIFCHISQ criterion for infl pts  */
   lsize=1.5,      /* obs label size.  The height of other    */
                   /* text is controlled by the HTEXT= goption*/
   lcolor=BLACK,   /* obs label color                         */
   lpos=5,         /* obs label position                      */
   lfont=,         /* obs label font                          */
   bsize=10,       /* bubble size scale factor                */
   bscale=AREA,    /* bubble size proportional to AREA or RADIUS */
   bcolor=RED,     /* bubble color                            */
   bfill=,         /* fill bubbles?  SOLID|GRADIENT              */
   refcol=BLACK,   /* color of reference lines                */
   reflin=33,      /* line style for reference lines; 0->NONE */
   loptions=noprint,/* options for PROC LOGISTIC              */
   name=INFLOGIS,
   gout=
   );

%let me=INFLOGIS;
%let nv = %numwords(&x);        /* number of predictors */
%let nx = %numwords(&gx);       /* number of abscissa vars */
%let ny = %numwords(&gy);       /* number of ordinate vars */
%if &nv = 0 %then %do;
    %put ERROR: List of predictors (X=) is empty;
    %goto done;
    %end;

%let gx=%upcase(&gx);
%let gy=%upcase(&gy);
%let label=%upcase(&label);
%let bubble=%upcase(&bubble);
%if not ((%bquote(&bubble) = C)
    or   (%bquote(&bubble) = CBAR)) %then %do;
    %put BUBBLE=%bquote(&bubble) is not valid. BUBBLE=C will be used;
    %let bubble=C;
    %end;

%if %length(&class) > 0 and &sysver < 8 %then %do;
	%let class=;
	%put INFLOGIS:  The CLASS= parameter is not supported in SAS &sysver;
	%end;

proc logistic nosimple data=&data &loptions ;
   %if %length(&class)>0 %then %do;
		class &class;
		%end;
   %if %length(&trials)=0 %then %do;
      model &y         = &x / influence;
      %end;
   %else %do;
      model &y/&trials = &x / influence;
      %end;
   output out=&out h=hat pred=pred
                     difdev=difdev
                     difchisq=difchisq
                     c=c  cbar=cbar
                     resdev=resdev;

data &out;
   set &out;
   label difdev='Change in Deviance'
         difchisq='Change in Pearson Chi Square'
         hat = 'Leverage (Hat value)'
         studres = 'Studentized deviance residual';
   studres = resdev / sqrt(1-hat);
   run;

%if %length(&bfill) %then %do; 
proc sort data=&out;
	by  descending &bubble;
	run;
%end;

%do i=1 %to &ny;
   %let gyi = %scan(&gy, &i);
   %do j=1 %to &nx;
   %let gxj = %scan(&gx, &j);
   %put &me: Plotting &gyi vs &gxj ;

   %if &label ^= NONE %then %do;
   data _label_;
      set &out nobs=n;
      length xsys $1 ysys $1 function $8 position $1 text $16 color $8;
      retain xsys '2' ysys '2' function 'LABEL' color "&lcolor" when 'A';
      retain hcrit hcrit1;
      drop hcrit;
      *keep &id x y xsys ysys function position text color size position
      hat difchisq difdev &bubble;
      x = &gxj;
      y = &gyi;
      %if &id ^= %str() %then %do;
         text = left( &id );
         %end;
      %else %do;
         text = put(_n_,3.0);
         %end;
      if _n_=1 then do;
         hcrit = 2 * (&nv+1)/n;
         hcrit1 = 3 * (&nv+1)/n;
         put "&me: Hatvalue criteria: 2p/n=" hcrit 4.3 ', 3p/n=' hcrit1 4.3;
         call symput('hcrit',put(hcrit,4.3));
         call symput('hcrit1',put(hcrit1,4.3));
         end;
      size=&lsize;
      position="&lpos";
	  %if %length(&lfont) %then %do; 
		style="&lfont";
		%end;
      %if &label = INFL %then %do;
/*         if %scan(&gy,1) > &dev
		   or difchisq > &dev
         or hat > hcrit  
         or &bubble > 1
            then output;   */
         if &infl then output;
         %end;
   run;
   %if &i=1 and &j=1 %then %do;
      proc print data=_label_;
         var &y &x pred studres hat difchisq difdev &bubble;
		   format hat 3.2 pred &bubble 4.3 studres 6.3 difdev difchisq 6.3;
      %if &id ^= %str() %then %do;
         id &id;
         %end;
      %else %do;
         id text;
         %put WARNING:  Observations are identified by sequential number (TEXT) because no ID= variable was specified.;
         %end;
      %end;
   %end;  /* &label ^= NONE */

   proc gplot data=&out &GOUT ;
     bubble &gyi * &gxj = &bubble /
           %if &label ^= NONE %then %do;
           annotate=_label_
           %end;
           frame
           vaxis=axis1 vminor=1 hminor=1
           %if &reflin ^= 0 %then %do;
           %if (&gyi = DIFDEV) or (&gyi = DIFCHISQ) %then %do;
              vref=&dev       lvref=&reflin  cvref=&refcol
              %end;
           %if (&gxj = HAT) %then %do;
              href= &hcrit &hcrit1 lhref=&reflin  chref=&refcol
              %end;
           %end;
           bsize=&bsize  bcolor=&bcolor  bscale=&bscale
		   %if %length(&bfill) %then %do; 
			bfill=&bfill
			%end;
           name="&name"
           Des="Logistic influence plot for &y";
     axis1 label=(a=90 r=0);
   run; quit;
   %gskip;
   %end;   /* gx loop */
%end;   /* gy loop */
%done:
quit;
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
