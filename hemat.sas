 /*--------------------------------------------------------------*
  *    Name: hemat.sas                                           *
  *   Title: HE plots for all pairs of response variables        *
        Doc: http://www.datavis.ca/sasmac/nodoc.html       
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 03 Jan 2004 10:13:16                                *
  * Revised: 25 Jan 2008 13:56:57                                *
  * Version: 1.2-1                                               *
  * - Added GPFMT=                                               *
  * - Added EFFLAB=, ADD=CANVEC, CLASS=                          *
  * 1.2 Added PANELMAC= to allow heplot/heplots                  *
    - Made HEPLOTS the default                                   *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The HEMAT macro plots the covariance ellipses for a hypothesized (H)
 effect and for error (E) for all pairs of variables from a MANOVA
 or multivariate multiple regression.

==Method:

 The macro calls either the HEPLOT macro or the HEPLOTS macro
 within nested %do ... %end loops to plot all
 pairs of responses.  This is wrapped with calls to the GDISPLA macro 
 to suppress display of the individual plots.  The final display is
 produced by PROC GREPLAY.  In order to make this macro reusable with
 a single SAS session, the separate plots are saved in a temporary
 graphics catalog (GTEMP=work.gtemp), which is normally deleted at the end
 (GKILL=Y).

=Usage:

 The HEMAT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%hemat(data=iris, stat=stats, 
	   var=SepalLen SepalWid PetalLen PetalWid,
	   effect=species);

 
==Parameters:

* DATA=       Name of the raw data set to be plotted [Default: DATA=_LAST_]

* STAT=       Name of OUTSTAT= dataset from PROC GLM

* VAR=        Names of response variables to be plotted - can be
              a list or X1-X4 or VARA--VARB [Default: VAR=_NUMERIC_]

* EFFECT=     Name of MODEL effect to be displayed for the H matrix.
              This must be one (or more, with HEPLOTS) of
              the terms on the right hand side of the MODEL statement used
			  in the PROC GLM or PROC REG step, in the same format that this
			  efffect is labeled in the STAT= dataset. This must be one of
			  the values of the _SOURCE_ variable contained in the STAT= 
			  dataset.

* EFFECTS=    Synonym for EFFECT=

* EFFLAB=     Optional label for the H effect, annotated near the upper
              corner of the H ellipse

* NAMES=      Alternative variable names (used to label the diagonal
              cells.)

* M1=         First matrix: either H or H+E [Default: M1=H]

* M2=         Second matrix either E or I [Default: M2=E]

* SCALE=      Scale factors for M1 and M2.  See description in HEPLOT

* HTITLE=     Height of variable name in diagonal cells

* COLORS=     Colors for the H and E ellipses [Default: COLORS=BLACK RED]

* LINES=      Line styles for the H and E ellipses [Default: LINES=1 21]

* WIDTH=      Line widths for the H and E ellipses [Default: WIDTH=3 2]

* ANNO=       Annotate diag or off-diag plot (not implemented). [Default: ANNO=NONE]

* GTEMP=      Temporary graphics catalog [Default: GTEMP=GTEMP]

* KILL=       Delete grtemp when done [Default: KILL=Y]

* GOUT=       Name of the graphic catalog [Default: GOUT=GSEG]

* PANELMAC=   Either HEPLOT or HEPLOTS

* SIZE=       For HEPLOTS: Either SIZE=EVIDENCE (significance scaling)
              or SIZE=EFFECT (effect size scaling) of H relative to E

* LEVEL=      For HEPLOTS: Coverage for the E ellipse


 =*/

%macro hemat(
	data=_LAST_,       /* data set to be plotted                       */
	stat=,             /* name of OUTSTAT= dataset from proc glm       */
	effect=,           /* name of MODEL effect(s) to be displayed      */
	effects=,           /* synonym for EFFECT=                         */
	var=_NUMERIC_,     /* variables to be plotted - can be             */
                       /* a list or X1-X4 or VARA--VARB                */
	class=&effect,
	ss=ss3,            /* type of SS to extract from the STAT= dataset */
	names=,            /* Alternative variable names                   */
	gpfmt=,            /* format for levels of the group/effect var    */
	efflab=&effect,    /* optional label for the H matrix              */
	htitle=,           /* height of variable name titles               */
	symbols=,          /* not used */
	colors=BLACK RED,  /* colors for the H and E ellipses              */
	lines=1 21,        /* line styles for the H and E ellipses         */
	width=3 2,         /* line widths for the H and E ellipses         */
	anno=NONE,         /* annotate diag or off-diag plot [not used]    */

	panelmac=heplots,   /* macro to plot off-diag panels                */

	/* heplot-specific */
	M1=H,              /* first matrix: either H or H+E                */
	M2=E,              /* second matrix either E or I                  */
	scale=,            /* scale factors for M1 and M2                  */
	add=,
	/* heplots-specific */
	size=evidence,
	level=0.68,        /* coverage proportion for E ellipse            */

	gtemp=gtemp,       /* temporary graphics catalog                   */
	kill=Y,            /* delete grtemp when done                      */
	gout=GSEG);        /* graphic catalog for plot matrix              */
 
 options nonotes;
%let anno=%upcase(&anno);
%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%local me; %let me=HEMAT;

%*-- For Session Manager, need to start a new graphic catalog;
%*-- Use a temporary graphics catalog to create the individual plots;
%*let gtemp = gtemp;
 
*-- Parse variables list;
 data _null_;
 set &data (obs=1) nobs=nobs;
 	call symput('nobs', trim(left(put(nobs,8.))) );
    %if %index(&var,-) > 0 or "&var"="_NUMERIC_" %then %do;
 
       %* find the number of variables in the list and
         convert shorthand variable list to long form;
     length _vname_ $ 8 _vlist_ $ 200;
     array _xx_ &var;
     _vname_ = ' ';
     do over _xx_;
        call vname(_xx_,_vname_);
        if _vname_ ne "&group" then do;
           nvar + 1;
	        _vlist_ = trim(_vlist_)|| ' ' || trim(_vname_);
        end;
     end;
     call symput( 'VAR', trim(_vlist_) );
   %end;
   %else %do;
     * find the number of variables in the list;
     nvar = n(of &var) + nmiss(of &var);
   %end;
   call symput('NVAR',trim(left(put(nvar,2.))));
     * default symbol height, if hsym not specified;
   ht = scan('1.4 2.3 3 3.7 4.2 4.5 5 5.3 5.4 5.5 5.6 5.7',nvar,' ');
   call symput('HT',trim(left(put(ht,3.1))));
 RUN;
%if &nvar < 2 or &nvar > 12 %then %do;
   %put Cannot do a HE plot matrix for &nvar variables ;
   %goto DONE;
   %end;
 
%if %length(&effect)=0 %then %do;
	%let effect = &effects;
	%end;

%gdispla(OFF);
 
title h=0.01 ' ';
%let annoopt=;
%let plotnum=0;    * number of plots made;
%let replay = ;    * replay list;

%if %length(&htitle)=0 %then %let htitle=2*&nvar;
 
%do i = 1 %to &nvar;                   /* rows */
   %let vi = %scan(&var , &i );
	%let ni=;
	%if %length(&names)>0
		%then %let ni = %scan(&names , &i, %str( ) );
/*
   proc means noprint data=&data;
      var &vi;
      output out=minmax min=min max=max;
*/ 
   %do j = 1 %to &nvar;                /* cols */
      %let vj = %scan(&var , &j );
      %let plotnum = %eval(&plotnum+1);
      %let replay  = &replay &plotnum:&plotnum ;
      %put &me: plot &plotnum: &vi vs. &vj ;
 
      %if &i = &j %then %do;           /*---- diagonal panel ----*/
         data title;
            length text $12 function $8;
*            set minmax;
            xsys = '1'; ysys = '1';
            x = 50; y = 50;
            text = "&vi";
				%if %length(&ni)>0 %then %str(text = "&ni";);
            size = &htitle;
            function = 'LABEL';  output;
            x = 1; y = 1;
            function = 'MOVE ';  output;
            x = 99; y = 99; line=0; size=1;
            function = 'BAR ';  output;
			
/*  

         proc gplot data = &data gout=&gtemp;
            plot &vi * &vi = &plotnam
            / frame anno=title vaxis=axis1 haxis=axis1
				  name="scat&i" des="SCATMAT title &vi";
         axis1 label=none value=none major=none
               minor=none offset=(2);
         run; quit;
*/
		proc ganno anno=title gout=&gtemp;
			run;
      %end;
 
      %else %do;                     /*---- off-diagonal panel ----*/

         axis1  label=none value=none major=none
               minor=none offset=(2);
		%if %upcase(&panelmac)=HEPLOT %then %do;
		   %heplot(data=&data, stat=&stat, var=&vj &vi, effect=&effect,
			   class=&class, ss=&ss,
			   M1=&M1, M2=&M2,
			   gpfmt=&gpfmt,
			   efflab=&efflab,
			   scale=&scale, colors=&colors,
			   lines=&lines, width=&width,
			   add=&add,
			   legend=none, gout=&gtemp, vaxis=axis1, haxis=axis1,
			   htext=&ht);
		   %end;
		%else %if %upcase(&panelmac)=HEPLOTS %then %do;
		   %heplots(data=&data, stat=&stat, var=&vj &vi, effect=&effect,
			   class=&class, ss=&ss,
			   gpfmt=&gpfmt,
			   efflab=&efflab,
			   size=&size,
			   colors=&colors,
			   lines=&lines, width=&width,
			/*   add=&add, */
			   legend=none, gout=&gtemp, vaxis=axis1, haxis=axis1,
			   htext=&ht);
		   %end;
      %end;
 
   %end; /* cols */
%end;    /* rows */
 
%gdispla(ON);
 
%macro tdef(nv);
%* ---------------------------------------------------------------;
%* Generate a TDEF statement for a scatterplot matrix             ;
%* Start with (1,1) panel in upper left, and copy it across & down;
%* This version uses %sysfunc and %sysevalf to obtain non-integer
   %s, so it will no longer work with SAS < V7;
%* ---------------------------------------------------------------;
%local i j panl panl1 lx ly;
 
   TDEF scat&nv DES="HE mat matrix &nv x &nv"
   %let panl=0;
   %let size = %sysfunc(round(100/&nv,0.01));
   %let shift = &size;
   %let lx = &size;
   %let ly = %sysevalf(100-&size);
   %do i = 1 %to &nv;
   %do j = 1 %to &nv;
       %let panl  = %eval(&panl + 1);
       %if &j=1 %then
          %do;
             %if &i=1 %then %do;      %* (1,1) panel;
               &panl/
                ULX=0  ULY=100   URX=&lx URY=100
                LLX=0  LLY=&ly   LRX=&lx LRY=&ly
                %end;
             %else
                %do;                  %* (i,1) panel;
                   %let panl1 = %eval(&panl - &nv );
               &panl/ copy= &panl1 xlatey= -&shift
                %end;
          %end;
       %else
          %do;
               %let panl1 = %eval(&panl - 1);
               &panl/ copy= &panl1 xlatex= &shift
          %end;
   %end;
   %end;
     %str(;);      %* end the TDEF statement;
%mend;
 
%put &me: Generating a &nvar x &nvar template to replay the plots.;
 
proc greplay igout=&gtemp
              gout=&gout  nofs
             template=scat&nvar
             tc=templt ;
   %tdef(&nvar); 
   TREPLAY &replay;
run;
%if &kill=Y %then %do;
  proc catalog kill catalog=&gtemp et=grseg;
run; quit;
%end;
%DONE:
  options notes;
  goptions reset=symbol;
%mend;
 
