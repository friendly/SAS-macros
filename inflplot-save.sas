 /*--------------------------------------------------------------*
  *    Name: inflplot.sas                                        *
  *   Title: Influence plot for regression models                *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/inflplot.html   *
  *                                                              *
  *  This SAS macro makes a plot of studentized residuals vs.    *
  *  leverage (hat-value), using COOK's D or DFFITS as the size  *
  *  of a bubble symbol.                                         *
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created:  14 Nov 1993 10:42:11                               *
  * Revised:  25 Mar 2005 14:46:08                               *
  * Version:  1.1                                                *
  *  1.1  Added INFL= to control what's influential              *
  *       Added HREF= to control ref lines for hat values        *
  *       Fixed NAME=                                            *
  *                                                              *
  *--------------------------------------------------------------*/
%macro inflplot(
     data=_last_,    /* Name of input data set                  */
     y=,             /* Name of criterion variable              */
     x=,             /* Names of predictors                     */
     id=,            /* Name of observation ID variable (char)  */
     bubble=COOKD,   /* Bubble proportional to: COOKD or DFFITS */
     label=INFL,     /* Points to label: ALL, NONE, or INFL     */
     infl=%str(abs(rstudent) > tcrit
         or hatvalue > hcrit
         or abs(&bubble)  > bcrit),
     lsize=1.5,      /* obs label size.  The height of other    */
                     /* text is controlled by the HTEXT= goption*/
     lcolor=BLACK,   /* obs label color                         */
     lpos=5,         /* obs label position                      */
     bsize=10,       /* bubble size scale factor                */
     bscale=AREA,    /* bubble size proportional to AREA or RADIUS */
     bcolor=RED,     /* bubble color                            */
	 href=&hcrit &hcrit1,
     refcol=BLACK,   /* color of reference lines                */
     reflin=33,      /* line style for reference lines; 0->NONE */
     name=INFLPLOT,
     gout=
     );
 
%let nv = %numwords(&x);        /* number of predictors */
%if &nv = 0 %then %do;
    %put ERROR: List of predictors (X=) is empty;
    %goto done;
    %end;
 
%let label=%upcase(&label);
%let bubble=%upcase(&bubble);
%if not ((%bquote(&bubble) = COOKD)
    or   (%bquote(&bubble) = DFFITS)) %then %do;
    %put BUBBLE=%bquote(&bubble) is not valid. BUBBLE=COOKD will be used;
    %let bubble=cookd;
    %end;
 
proc reg data=&data noprint;
   model &y = &x / influence;
   %if &id ^= %str() %then %do;
      id &id;
      %end;
   output out=_diag_ h=hatvalue
                     rstudent=rstudent
                     &bubble=&bubble;
 
   %if &label ^= NONE %then %do;
   data _label_;
      set _diag_ nobs=n;
      length xsys $1 ysys $1 function $8 position $4 text $12 color $8;
      retain xsys '2' ysys '2' function 'LABEL' color "&lcolor";
      retain tcrit hcrit bcrit tcrit1 hcrit1;
      drop tcrit hcrit bcrit  tcrit1 hcrit1;
      x=hatvalue;
      y=rstudent;
      %if &id ^= %str() %then %do;
         text = &id;
         %end;
      %else %do;
         text = put(_n_,3.0);
         %end;
      if _n_=1 then do;
         tcrit  = tinv(.975, n-3);              *-- individual;
		 tcrit1 = tinv(1 - (0.05/(2*n)), n-3);  *-- Bonferroni;
         hcrit = 2 * (&nv+1)/n;
		 hcrit1= 3 * (&nv+1)/n;
         %if &bubble = COOKD
            %then %do; bcrit = 4/n; %end;
            %else %do; bcrit = 2 * sqrt((&nv+1)/n); %end;
         put "NOTE: Influential values for RSTUDENT, HAT and &bubble, using:";
		 put 'NOTE:    abs(RSTUDENT)> ' tcrit 5.2 ', HAT> ' hcrit 5.3 " &bubble > " bcrit 5.3;
         call symput('tcrit',put(tcrit,5.2));
         call symput('hcrit',put(hcrit,5.3));
         call symput('tcrit1',put(tcrit1,5.2));
         call symput('hcrit1',put(hcrit1,5.3));
         end;
      size=&lsize;
      position="&lpos";
      %if %upcase(&label) = INFL %then %do;
/*       if abs(rstudent) > tcrit
         or hatvalue > hcrit
         or abs(&bubble)  > bcrit  then output;
*/
			if &infl then output;
         %end;
   run;
   %end;  /* &label ^= NONE */
 
proc gplot data=_diag_ &GOUT ;
  bubble rstudent * hatvalue = &bubble /
        %if &label ^= NONE %then %do;
        annotate=_label_
        %end;
        frame
        vaxis=axis1 vminor=1 hminor=1
        %if &reflin ^= 0 %then %do;
        vref=-&tcrit 0 &tcrit lvref=&reflin  cvref=&refcol
        href= &href           lhref=&reflin  chref=&refcol
        %end;
        bsize=&bsize  bcolor=&bcolor  bscale=&bscale
        des="Regression influence plot for &y"
		name="&name";
  axis1 label=(a=90 r=0);
  label rstudent='Studentized Residual'
        hatvalue='Leverage (Hat Value)';
  format hatvalue 3.2;
  run; quit;
%done:
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
