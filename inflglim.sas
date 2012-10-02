 /*-------------------------------------------------------------------*
  *    Name: inflglim.sas                                             *
  *   Title: Influence plots for generalized linear models            *
        Doc: http://www.datavis.ca/sasmac/inflglim.html            
  *                                                                   *
  *-------------------------------------------------------------------*
     Author:  Michael Friendly            <friendly@yorku.ca>          
    Created:  24 Nov 1997 10:36:05                                     
    Revised:  30 Sep 2012 15:14:13                                     
    Version:  1.6-2                                                   
     - Fixed error if DIST= not specified. Added FREQ= parm            
     - Added MOPT= parm, INFL= parm (what's influential?)              
     1.4   Fixed make ... noprint for V7+                              
       Fixed numerous problems with GENMOD for V7+ (sigh)              
       Added PRINT= to control printing of OUT= data set               
     1.5 Added OFFSET=                                                 
       Fixed problem with PRINT=N                                      
       Fixed problem with long variable names                          
	 1.6 
	  Added BFILL= option for filled bubbles.  BFILL=gradient is useful
	  Added LFONT= option for font of bubble labels
	  Fixed warning for lfont style                            
                                                                       
    Dependencies:  %gskip (needed for eps/gif only)                    
                                                                       
    From ``Visualizing Categorical Data'', Michael Friendly (2000)              
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The INFLGLIM macro produces various influence plots for a generalized
 linear model fit by PROC GENMOD.  Each of these is a bubble plot of one
 diagnostic measure (specified by the GY= parameter) against another
 (GX=), with the bubble size proportional to a measure of influence
 (usually, BUBBLE=COOKD).  One plot is produced for each combination
 of the GY= and GX= parameters.
 
=Usage:

 The macro normally takes an input data set of raw data and fits the
 GLM specified by the RESP=, and MODEL= parameters, using an error
 distribution given by the DIST= parameter.  It fits the model,
 obtains the OBSTATS and PARMEST data sets, and uses these to compute
 some additional influence diagnostics (HAT, COOKD, DIFCHI, DIFDEV,
 SERES), any of which may be used as the GY= and GX= variables.

 Alternatively, if you have fit a model with PROC GENMOD and saved
 the OBSTATS and PARMEST data sets, you may specify these with the
 OBSTATS= and PARMEST= parameters.  The same additional diagnostics
 are calculated and plotted.

 The INFLGLIM macro is called with keyword parameters.  The MODEL=
 and RESP= parameters are required, and you must supply the DIST=
 parameter for any model with non-normal errors.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
   %inflglim(data=berkeley,
      class=dept gender admit,
      resp=freq, model=dept|gender dept|admit,
      dist=poisson,
      id=cell,
      gx=hat, gy=streschi);

 
==Parameters:

* DATA=       Name of input (raw data) data set. [Default: DATA=_LAST_]

* RESP=       The name of response variable.  For a loglin model, this
              is usually the frequency or cell count variable when the
              data are in grouped form (specify DIST=POISSON in this
              case).

* MODEL=      Gives the model specification.  You may use the '|' and
              '@' symbols to specify the model.

* CLASS=      Specifies the names of any class variables used in the model.

* DIST=       The name of the PROC GENMOD error distribution.  If you
              don't specify the error distribution, PROC GENMOD uses
              DIST=NORMAL.

* LINK=       The name of the link function.  The default is the canonical
              link function for the error distribution given by the
				  DIST= parameter.

* OFFSET=     The name(s) of any offset variables in the model.

* MOPT=       Other options on the MODEL statement (e.g., MOPT=NOINT
              to fit a model without an intercept).

* FREQ=       The name of a frequency variable, when the data are in
              grouped form.

* WEIGHT=     The name of an observation weight (SCWGT) variable, used, for
              example, to specify structural zeros in a loglin model.

* ID=         Gives the name of a character observation ID variable
              which is used to label influential observations in the
              plots. Usually you will want to construct a character
              variable which combines the CLASS= variables into a
              compact cell identifier.

* GY=         The names of variables in the OBSTATS data set used as
              ordinates for in the plot(s).  One plot is produced for
				  each combination of the words in GY by the words in GX.
				  [Default: GY=DIFCHI STRESCHI]

* GX=         Abscissa(s) for plot, usually  PRED or HAT. [Default: GX=HAT]

* OUT=        Name of output data set, containing the observation
              statistics. [Default: OUT=COOKD]

* OBSTATS=    Specifies the name of the OBSTATS data set (containing
              residuala and other observation statistics) for a model 
              already fitted.

* PARMEST=    Specifies the name of the PARMEST data set (containing
              parameter estimates) for a model already fitted.

* BUBBLE=     Gives the name of the variable to which the bubble size is
              proportional. [Default: BUBBLE=COOKD]

* LABEL=      Determines which observations, if any, are labeled in the
              plots.  If LABEL=NONE, no observations are labeled; if
              LABEL=ALL, all are labeled; if LABEL=INFL, only possibly
              influential points are labeled, as determined by the
              INFL= parameter. [Default: LABEL=INFL]

* INFL=       Specifies the criterion used to determine which observations
              are influential (when used with LABEL=INFL).
              [Default: INFL=%STR(DIFCHI > 4 OR HAT > &HCRIT OR &BUBBLE > 1)]

* LSIZE=      Observation label size. [Default: LSIZE=1.5]. The height of
              other text (e.g., axis labels) is controlled by the HTEXT=
              goption.

* LCOLOR=     Observation label color. [Default: LCOLOR=BLACK]

* LPOS=       Observation label position, relative to the point.
              [Default: LPOS=5]

* LFONT=      Font used for observation labels.
 
* BSIZE=      Bubble size scale factor. [Default: BSIZE=10]

* BSCALE=     Specifies whether the bubble size is proportional to AREA 
              or RADIUS. [Default: BSCALE=AREA]

* BCOLOR=     The color of the bubble symbol. [Default: BCOLOR=RED]

* BFILL=      Bubble fill? Options are BFILL=SOLID | GRADIENT, where the
              latter uses a gradient version of BCOLOR

* REFCOL=     Color of reference lines.  Reference
              lines are drawn at nominally 'large' values for HAT values,
              standardized residuals, and change in chi square values.
				  [Default: REFCOL=BLACK]

* REFLIN=     Line style for reference lines. Use REFLIN=0 to suppress
              these reference lines. [Default: REFLIN=33]

* NAME=       Name of the graph in the graphic catalog [Default:
              NAME=INFLGLIM]

* GOUT=       Name of the graphics catalog.

 =*/
 

%macro inflglim(
     data=_last_,    /* Name of input data set                  */
     resp=,          /* Name of criterion variable              */
     model=,         /* Model specification                     */
     class=,         /* Names of class variables                */
     dist=,          /* Error distribution                      */
     link=,          /* Link function                           */
     offset=,        /* Offset variable(s)                      */
     mopt=,          /* other model options (e.g., NOINT)       */
     freq=,          /* Freq variable                           */
     weight=,        /* Observation weight variable (zeros)     */
     id=,            /* Name of observation ID variable (char)  */
     gy=DIFCHI STRESCHI,      /* Ordinate(s) for plot(s)        */
     gx=HAT,         /* Abscissa(s_ for plot: PRED or HAT       */
     out=cookd,      /* Name of output data set                 */
     obstats=,       /* For a model already fitted              */
     parmest=,       /*  "      "      "      "                 */
	  print=Y,        /* Print the OUT= data set?                */
     bubble=COOKD,   /* Bubble proportional to: COOKD           */
     label=INFL,     /* Points to label: ALL, NONE, or INFL     */
     infl=%str(difchi > 4 or hat > &hcrit or &bubble > 1),
     lsize=1.5,      /* obs label size.  The height of other    */
                     /* text is controlled by the HTEXT= goption*/
     lcolor=BLACK,   /* obs label color                         */
     lpos=5,         /* obs label position                      */
	 lfont=,         /* obs label font                          */
     bsize=10,       /* bubble size scale factor                */
     bscale=AREA,    /* bubble size proportional to AREA or RADIUS */
     bcolor=RED,     /* bubble color                            */
	 bfill=,         /* bubble fill? SOLID or GRADIENT          */
     refcol=BLACK,   /* color of reference lines                */
     reflin=33,      /* line style for reference lines; 0->NONE */
     name=INFLGLIM,  /* Name of the graph in the graphic catalog */
     gout=
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

%let abort=0;
%let gx=%upcase(&gx);
%let gy=%upcase(&gy);
%let dist=%upcase(&dist);
%let label=%upcase(&label);
%let bubble=%upcase(&bubble);
%let print=%upcase(%substr(&print,1,1));
%let me=INFLGLIM;

%if %length(&model) = 0 %then %do;
    %put ERROR: List of model terms (MODEL=) is empty.;
    %let abort=1;
    %goto done;
    %end;

%if %length(&resp) = 0 %then %do;
    %put ERROR: No response (RESP=) has been specified.;
    %let abort=1;
    %goto done;
    %end;

%if %length(&dist) = 0 %then %do;
    %put WARNING: No distribution (DIST=) has been specified.;
    %put WARNING: GENMOD will use DIST=NORMAL.;
    %end;

%let nx = %numwords(&gx);       /* number of abscissa vars */
%let ny = %numwords(&gy);       /* number of ordinate vars */

%if %sysevalf(&sysver  < 6.12) %then %do;
   %if %upcase(&dist)=BINOMIAL %then %do;
      %if %length(%scan(&resp,2,/))=0 %then %do;
         %put ERROR: Response must be specified as RESP=events/trials for DIST=BINOMIAL;
         %let abort=1;
         %goto done;
         %end;
      %if %length(&link)=0 %then %let link=logit;
      %end;
   %end;

%if %length(&obstats)=0 or %length(&parmest)=0 %then %do;
proc genmod data=&data;
  class &class;
        %if %length(&freq)>0 %then %do;  freq &freq; %end;
        %if %length(&weight)>0 %then %do;  scwgt &weight; %end;
  model &resp = &model /
        %if %length(&dist)>0 %then %do;  dist=&dist %end;
        %if %length(&link)>0 %then %do;  link=&link %end;
		%if %length(&offset)>0 %then %do; offset=&offset  %end;
        %if %length(&mopt)>0 %then %do;  %str(&mopt) %end;
        obstats residuals;
	%if %sysevalf(&sysver <7) %then %do;
  	make 'obstats'  out=_obstat_ noprint;
  	make 'parmest'  out=_parms_ noprint;
		%end;
	%else %do;
		%if &print=N %then %do;
		ods listing exclude ObStats;
		%end;
		ods output ObStats=_obstat_;
		ods output ParameterEstimates=_parms_;
		*proc print data=_parms_;
		%end;
  run;

%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;
%let obstats=_obstat_;
%let parmest=_parms_;
%end;

options nonotes;
%let parms=0;
data _null_;
   set &parmest end=eof;
   parms + df;
   if eof then do;
      call symput('parms', left(put(parms,5.)));
      end;
run;
%*put parms=&parms;
%if &parms=0 %then %let abort=1; %if &abort %then %goto DONE;

data &out;
  /* GENMOD seems to make all class variables character */
  /* keep only the GENMOD computed variables */
  merge &data 
  		&obstats(keep=pred--reslik)
		end=eof;
  drop hcrit obs;
  obs=_N_;
  label hat = 'Leverage (H value)'
        cookd = "Cook's Distance"
        difchi = 'Change in Pearson ChiSquare'
        difdev = 'Change in Deviance'
        reschi = 'Pearson residual'
        resdev = 'Deviance residual'
        streschi = 'Adjusted Pearson residual'
        stresdev = 'Adjusted Deviance residual'
        seres = 'Residual Std. Error'
        pred = 'Fitted value';

  /* hat is the leverage */
  hat = Std*Hesswgt*Std;
  if hat<1 then do;
     cookd = hat*Streschi**2/((&parms)*(1-hat));
     seres = sqrt(1-hat);
     end;

  difchi = streschi**2;
  difdev = stresdev**2;

  if eof then do;
     hcrit =  &parms / obs;
     call symput('hcrit', put(hcrit,4.3));
     end;
  run;

%if &print=Y %then %do;
proc print data=&out noobs label;
   id &id ;
   format pred hesswgt lower upper 6.2 
          xbeta std resraw--reslik hat cookd difchi difdev seres 7.3 ;
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
      length xsys $1 ysys $1 function $8 position $1 text $12 color $8 style $32;
      retain xsys '2' ysys '2' function 'LABEL' color "&lcolor" when 'A' style '';
      keep &id &class x y xsys ysys function position text color size
           position hat difchi &bubble when style;
      x = &gxj;
      y = &gyi;
      %if &id ^= %str() %then %do;
         text = left( &id );
         %end;
      %else %if %length(&class) %then %do;
         %let c = 1;
         %let v = %scan(&class,&c);
         text = '';
         %do %while (%length(&v) > 0);
            text = trim(text) || trim(&v);
            %let c = %eval(&c + 1);
            %let v = %scan(&class,&c);
            %end;
            %end;
      %else %do;
         text = put(_n_,3.0);
         %end;
      size=&lsize;
      position="&lpos";
	  %if %length(&lfont) %then %do; 
		style="&lfont";
		%end;
      %if &label = INFL %then %do;
         if &infl then output;
         %end;
   run;
   %end;  /* &label ^= NONE */

   proc gplot data=&out &GOUT ;
     bubble &gyi * &gxj = &bubble /
      %if &label ^= NONE %then %do;
      annotate=_label_
      %end;
      frame
      vaxis=axis1 vminor=1 hminor=1
      %if &reflin ^= 0 %then %do;
      %if (&gyi=DIFCHI) or (&gyi=DIFDEV) %then %do;
         vref=4       lvref=&reflin  cvref=&refcol
         %end;
      %else %if (&gyi=STRESCHI) or (&gyi=STRESDEV) %then %do;
         vref=0 -2 2       lvref=&reflin  cvref=&refcol
         %end;
      %if (&gxj = HAT) %then %do;
         href= &hcrit lhref=&reflin  chref=&refcol
         %end;
      %end;
      bsize=&bsize  bcolor=&bcolor  bscale=&bscale
      %if %length(&bfill) %then %do; 
			bfill=&bfill
      %end;
      name="&name"
      Des="Influence plot for &resp (&gyi vs. &gxj)";
     axis1 label=(a=90 r=0);
     format hat 3.2 streschi stresdev best3.1;
   run; quit;
   %gskip;
   %end;   /* gx loop */
%end;   /* gy loop */


%done:
%if &abort %then %put ERROR: The INFLGLIM macro ended abnormally.;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
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
