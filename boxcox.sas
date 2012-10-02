 /*------------------------------------------------------------------*
  *    Name: boxcox.sas                                              *
  *   Title: Power transformations by Box-Cox method                 *
  *          with graphic display of maximum likelihood solution,    *
  *               t-values for model effects, and influence of       *
  *               observations on choice of power.                   *
        Doc: http://www.datavis.ca/sasmac/boxcox.html          
  *------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>        *
  * Created:  23 Oct 1991 10:20:14                                   *
  * Revised:  05 Sep 2006 16:44:11                                   *
  * Version:  1.4                                                    *
  *  1.2  Added ADD= parameter (additive constant); Allow no MODEL=  *
  *  -handle special missing values (.A-.Z). Fixed V8 lc varname bug *
  *  1.3  Added folded-power transformations for variables bounded   *
  *   on [0, &FOLD], specified by FOLD=                              *
  *  1.4  Added plot of logL vs. lambda                              *
  *       Changed default to PPLOT=NONE. Fixed %do-%end buglet       *
  *       Changed validvarname=V6 to validvarname=upcase             *
  *       Added code to best and simple powers in printed output     *
  *       Added code from v1.4a for better check on valid data       *
  *                                                                  *
  *------------------------------------------------------------------*/

 /*=
=Description:
 
 The BOXCOX macro computes the maximum likelihood power transformation
 (or folded-power transformation) of the response variable in a linear
 regression with zero or more predictors.  It produces a variety of plots,
 including a display of the maximum likelihood solution, t-values for
 effects in the model, and an influence plot of the observations in
 determining the power.  The optimal transformation of the response
 variable is returned in an output dataset.

 If the response variable is bounded on a closed interval, [0, b],
 the FOLD= parameter may be used to obtain analogous folded-power
 transformations.  For example, use FOLD=100 when the response
 variable is a percentage on the interval [0, 100].

 In SAS V7+ Box-Cox transformations can be found more efficiently
 using PROC TRANSREG, but this does not provide the convenience of
 plotting with the BOXCOX macro.

==Method:

 The program transforms the response to all powers from
 the LOPOWER= value to the HIPOWER= value, and fits a regression
 model for each, extracting values to an output dataset from
 which the plots are drawn.
 
=Usage:

 The BOXCOX macro is defined with keyword parameters.  The RESP=
 parameter is required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%boxcox(data=duncan, resp=prestige, model=income educ, id=job);
 
==Parameters:

* RESP=       Name of the response variable for analysis

* MODEL=      The independent variables in the regression, i.e., the 
              terms on the right side of the = sign in the MODEL statement 
				  for PROC REG.  May be empty, in which case the response
				  is transformed toward a normal distribution.

* ID=         Name of an ID variable for observations used as labels in
              the INFL plot.

* DATA=       The name of the data set holding the response and predictor
              variables.  [Default: DATA=_LAST_]

* OUT=        Output dataset with transformed resp. Contains all original
              variables, with the transformed response replacing the 
				  original variable. [Default: OUT=_DATA_]

* OUTPLOT=    Output dataset for plot of powers Contains _RMSE_, and 
              t-values for each effect in the model, with one observation
				  for each power value tried. [Default: OUTPLOT=_PLOT_]

* PPLOT=      Printer plots: One or more of RMSE, LOGL, EFFECT, and INFL
              [Default: PPLOT=NONE]

* GPLOT=      Graphic plots: One or more of RMSE, LOGL, EFFECT, and INFL 
              [Default: GPLOT=RMSE EFFECT INFL]

* ADD=        Additive constant for response, used to avoid problems when
              the response can be negative or zero.

* FOLD=       Upper bound on the response when the response
              is bounded above and below.. 
			  If FOLD>0 is specified, folded power transformations 
			  are computed. E.g., for a response which is a proportion, 
			  specify FOLD=1; for a percentage, specify FOLD=100.
			  [Default: FOLD=0]

* LOPOWER=    Low value for power [Default: LOPOWER=-2]

* HIPOWER=    High value for power [Default: HIPOWER=2]

* NPOWER=     Number of power values in interval [Default: NPOWER=21]

* CONF=       Confidence coefficient of CI on power [Default: CONF=0.95]

 
==Credits:

 This program incorporates portions of program code from the
 macro ADXTRANS in the ADX system for experimental design
 distributed as part of the SAS/QC product.

=References:

  Box, G.E.P. and Cox, D.R. (1964), "An Analysis of Transformations,"
  Journal of the Royal Statistics Society, B-26, 211 -252.

 =*/


%macro boxcox(
    resp=,                   /* name of response variable            */
    model=,                  /* independent variables in regression  */
    id=,                     /* ID variable for observations         */
    data=_last_,             /* input dataset                        */
    out=_data_,              /* output dataset with transformed resp */
    outplot=_plot_,          /* output dataset for plot of powers    */
    pplot=NONE,              /* printer plots: RMSE, LOGL, EFFECT, INFL*/
    gplot=RMSE EFFECT INFL,  /* graphic plots: RMSE, LOGL, EFFECT, INFL*/
	 add=0,                   /* additive constant for response       */
	 fold=0,                  /* upper bound on response (FOLD>0)     */
    lopower=-2,              /* low value for power                  */
    hipower=2,               /* high value for power                 */
    npower=21,               /* number of power values in interval   */
    conf=.95);               /* confidence coefficient of CI on power*/

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

   %if (%length(&resp) = 0) %then %do;
       %put ERROR: BOXCOX: RESP= must be specified;
       %goto exit;
       %end;
 
   %let nmod = %numwords(&model);
	%if %upcase(&data)=_LAST_ %then %let data = &syslast; 

 /*
 /  Get the number of non-missing observations; quick exit if none.
 /  Check for valid data (positive).
 /-------------------------------------------------------------------*/
   %let FLAG = 0;
   data _nomiss_; 
		set &data end=eof;
		drop nwarn;
		%if (&add>0) %then %do;
			&resp = &resp + &add;
			%end;
      if (.Z < &resp <= 0) then do;
			nwarn+1;
         if nwarn<11 then 
			put "WARNING: Non-positive value of dependent variable &resp"
             "(obs=" _n_ ") " &resp ;
	      call symput('FLAG','1');
         end;
		if eof then do;
         if nwarn then
				put 'WARNING:' nwarn " non-positive values of dependent variable &resp";
			end;
      if &resp > .Z;
   run;

   proc contents noprint data=_nomiss_ out=_NOBS_;
   data _null_; set _NOBS_;
      call symput('NOBS',left(put(nobs,best20.)));
   run;
   %if (&nobs <= 0) %then %do;
		%put ERROR: There are no non-missing values of the response &resp;
		%goto done;
		%end;
 
   %if (&FLAG) %then %do;
        %put BOXCOX: Cannot compute Box-Cox transformation for non-positive &resp. Use ADD=;
        %goto done;
        %end;

	%let bcpower=Box-Cox power;
	%if &fold>0 %then %let bcpower=Folded power [0,&fold];
   %put BOXCOX: Computing &bcpower transforms of &resp ...;
 
 /*
 /  Transpose the data set; transform all values for each power into
 /  variables NEW1, NEW2, ...; and then transpose back again.  Values
 /  are centered and scaled so as to be approximately in the same scale
 /  as originally and so that the transformed value of the geometric
 /  mean is the geometric mean.
 /-------------------------------------------------------------------*/
   data _tmp_;
      set _nomiss_; keep &resp;
   proc transpose data=_tmp_ out=_tmp_;
      var &resp;
   data _tmp_; set _tmp_;
      array col{&NOBS};
      array new{&NOBS};
      keep _name_ new1-new&NOBS;
 
      gmean = 0;
      do i = 1 to &NOBS; 
			%if &fold=0 %then %do;
				gmean = gmean + log(col{i}); 
				%end;
				%else %do;
*				col{j} = col{j} / &fold;
				gmean = gmean + log(col{i} * (&fold-col{i})); 
				%end;
				end;
      gmean = exp(gmean / &NOBS);
 
      inc = (&hipower-&lopower)/(&npower-1);
      do i = 0 to &npower-1;
         lambda = &lopower + (i * inc);

         if (abs(lambda) < (inc/2)) then do;
            z1 = log(gmean);
				%if &fold=0 %then %do;
            do j = 1 to &NOBS;
               new{j} = gmean + (log(col{j})-z1)*gmean;
               end;
				%end;
				%else %do;
            do j = 1 to &NOBS;
               new{j} = (log(col{j} / (&fold-col{j})))*gmean / &fold;
               end;
				%end;
            end;
				
         else do;
            z1 = gmean**lambda;
				%if &fold=0 %then %do;
            z2 = lambda*(gmean**(lambda-1));
            do j = 1 to &NOBS;
               new{j} = gmean + ((col{j}**lambda)-z1)/z2;
               end;
				%end;
				%else %do;
				z2 = 0;
				do j = 1 to &NOBS;
					z2 = z2 + log( col{j}**(lambda-1) + (&fold-col{j})**(lambda-1));
					end;
				z2 = lambda * exp(z2/ &NOBS);
            do j = 1 to &NOBS;
               new{j} = gmean + ((col{j}**lambda - (&fold-col{j})**lambda)-z1)/z2;
               end;
				%end;
            end;

         _name_ = "_tf" || left(put(i+1,best20.));
         output;
         end;

   proc transpose data=_tmp_ out=_tmp_;
      var new1-new&NOBS;
   data _tmp_;
      merge _nomiss_ _tmp_; drop _NAME_;
   run;
 
   %put BOXCOX: Regressing on transforms for &resp ...;
 /*
 /  Perform the regression on all transformed variates at once.
 /-------------------------------------------------------------------*/
   proc reg data=_tmp_ outest=_reg_ noprint covout;
      model _tf1-_tf&npower = &model;
   run;
 
   %put BOXCOX: Computing optimal transformation and confidence interval;
 
 /*
 /  Divide estimates by their standard errors and get rid of the
 /  covariance matrix.  If no degrees of freedom left for error, exit.
 /-------------------------------------------------------------------*/
   %let nmodp1 = %eval(&nmod+1);
   %let FLAG = 0;
   data _reg_;
      drop _i_ _j_ t1-t&nmodp1;
      drop _model_ _name_ _type_ _tf1-_tf&npower _depvar_;
      array e{&nmodp1} intercep &model;
      array t{&nmodp1};
      do _i_ = 1 to &npower;
         pnt = (_i_-1)*(1+&nmodp1) + 1;
         set _reg_ point = pnt;
         adxlam = &lopower+((_i_-1)*((&hipower-&lopower)/
                  (&npower-1)));
         if (_rmse_ ^= .) then do;
            do _j_=1 to &nmodp1; t{_j_} = e{_j_}; end;
            do _j_=1 to &nmodp1;
               pnt = (_i_-1)*(2+&nmod) + 1 + _j_;
               set _reg_ point = pnt;
               t{_j_} = t{_j_} / sqrt(e{_j_});
               end;
            do _j_=1 to &nmodp1; e{_j_} = t{_j_}; end;
            _like_ = -&NOBS*log(_rmse_);
            output;
            end;
         else do;
            call symput('FLAG','1');
            end;
         end;
      stop;
   run;
   %if (&FLAG) %then %do;
      %put BOXCOX: No degrees of freedom left to estimate error.;
      %put %str(         )Transformation cannot be estimated.;
      %goto done;
      %end;
 
 /*
 /  Compute the optimal transform, the one with minimum RMSE, and an
 /  approximate confidence interval.  The approximate .95 confidence
 /  interval is based on the fact that
 /           2{ L(lambda-hat) - L(lambda) } <= cinv(.95,1) = 3.84
 /-------------------------------------------------------------------*/
   proc transpose data=_reg_ out=_reg_; *proc print;
   data _reg_;
      set _reg_; keep _name_ _label_ col1-col&npower;
      array col{&npower};
      array rmse{&npower};
      retain imin rmse1-rmse&npower;
      if (trim(_name_) = '_RMSE_') then do;
         rmse{1} = col{1};
         minrmse = rmse{1};
         imin   = 1;
         do i = 2 to &npower;
            rmse{i} = col{i};
            if (rmse{i} < minrmse) then do;
               minrmse = rmse{i};
               imin    = i;
               end;
            end;
         call symput('_imin_',left(put(imin,best20.)));
         lambda = &lopower+((imin-1)*(&hipower-&lopower)/(&npower-1));
		*-- Store estimated lambda and nearest half-integer value;
         call symput('elambda',left(put(lambda,best10.)));
         call symput('clambda',left(put(round(lambda, 0.5),best5.)));		 
         end;
      else if (trim(_name_) = '_LIKE_') then do;
         call symput('maxlike',left(put(col{imin},best20.)));
         end;
run;
%*put elambda=&elambda  maxlike=&maxlike;

   proc transpose data=_reg_ out=_reg_;
*-- Added code for plot of logL vs. lambda   8/02/02 MF;
*-- Added code to distinguish minimum and convenient values in CONF   3/17/05 MF;
   data &outplot;
      set _reg_ end=eof;
      drop _name_  adxlam _lolam_ _hilam_ _hirmse_ tval; ** _label_ removed;
		drop _cutoff_;
      retain _lolam_ 10 _hilam_ -10 _hirmse_ 0;
      _lambda_ = adxlam;
      label _lambda_ = 'Box-Cox Power (lambda)'
            _like_   = 'Log Likelihood'
            conf     = "&conf Confidence Interval";
      length conf $2;
	   _cutoff_ = &maxlike - (cinv(&conf,1)/2);
      if (_like_ < _cutoff_ )  /* was 1.92 */
         then conf = " ";
         else do;
              conf = "*";
			  if abs(_lambda_ - &elambda) < 1e-6 
			  	then conf = '<';
			  if abs(_lambda_ - round(_lambda_, 0.5)) < 1e-6 
			  	then conf = trim(conf) || '+';
              _lolam_ = min(_lolam_,_lambda_);
              _hilam_ = max(_hilam_,_lambda_);
              _hirmse_= max(_hirmse_,_rmse_);
              end;
      if eof then do;
         call symput('lline',left(put(_cutoff_,best20.)));
         call symput('lolam',left(put(_lolam_,best20.)));
         call symput('hilam',left(put(_hilam_,best20.)));
         call symput('hirms',left(put(_hirmse_,best20.)));
         tval = tinv(1-(1-&conf)/2, &nobs-&nmodp1 );
         call symput('tval', left(put(tval,best10.)));
         end;
   run;
   options notes;
   proc print data=&outplot label noobs;
      var _lambda_ _like_ _rmse_ conf;
   run;
   %put BOXCOX: Estimated  power transformation: Lambda = &elambda.;
   %put BOXCOX: Simplified power transformation: Lambda = &clambda.;
 
 /*
 /  Plot likelihood and t-values for effects as functions of the
 /  power.
 /-------------------------------------------------------------------*/
   data &out; set _tmp_; drop _tf1-_tf&npower;
      &resp = _tf&_imin_;
      label &resp = "Transformed &resp (lambda=&elambda)";
 
   %let pplot = %upcase(&pplot);
   %if &pplot ^= NONE %then %do;
   %let sym = 1 2 3 4 5 6 7 8 9;
   %let sym = &sym A B C D E F G H I J K L M N O P Q R S T U V W X Y Z;
   %let sym = &sym a b c d e f g h i j k l m n o p q r s t u v w x y z;
   proc plot data=&outplot;
      %if %index(&pplot,RMSE)>0 %then %do;
         title 'RMSE for Box-Cox Power Transform';
         plot _rmse_ * _lambda_ = 'L'
            / href=&lolam &hilam vref=&hirms;
         label _rmse_ = "Root Mean Squared Error for &resp";
      run;
		%end;
    %if %index(&pplot,LOGL)>0 %then %do;
	 plot _like_ * _lambda_ = 'L' /
            / href=&lolam &hilam ;
      title "Box-Cox Power Transform for &resp";
      label _like_ = "Log Likelihood";
      %end;
      %if (&nmod>0) and %index(&pplot,EFFECT)>0 %then %do;
         plot
         %do i = 1 %to &nmod;
            %scan(&model,&i) * _lambda_ = "%scan(&sym,&i)"
            %end;
            / overlay href=&lolam &hilam ;
         title 't-values for Model Effects';
      run;
      %end;
   %end;
 
   %let gplot = %upcase(&gplot);
   %if &gplot ^= NONE %then %do;
   %let sym = dot star square circle triangle $ + hash x;
   %if (&nmod>0) and %index(&gplot,EFFECT)>0 %then %do;
   data _labels_;
      set &outplot end=eof;
      length function text $8;
      if eof then do;
         xsys='2'; ysys='2'; size=1.1;
         function='LABEL';  position='6';
         x = _lambda_;
         %do i = 1 %to &nmod;
            y = %scan(&model,&i) ;
            text = " %scan(&model,&i) "; output;
            %end;
         end;
   %end;
 
%let ftext = %sysfunc(getoption(ftext));
%if %index(&ftext,DEFAULT) %then %let ftext=%substr(&ftext,1,%eval(%index(&ftext,DEFAULT)-1));
%if ftext=%str() %then %let ftext=NONE;

proc gplot data=&outplot;
	symbol1 i=spline v=dot c=black;   *-- make sure symbol1 is defined;
   %do i = 1 %to &nmod;
      symbol&i i=spline v=%scan(&sym,&i) c=black ;
      %end;
      axis1 label=(a=90);
      axis2 label=('Box-Cox Power (' f=cgreek 'l' f=&ftext ')' ) 
            offset=(3);
      axis3 label=(a=90 't-value');
      axis4 label=('Box-Cox Power (' f=cgreek 'l' f=&ftext ')' ) 
            offset=(2,9);
   %if %index(&gplot,RMSE)>0 %then %do;
      plot _rmse_ * _lambda_ = 1 /
            href=&lolam &hilam lhref=20 chref=red
            vref=&hirms        lvref=33 cvref=red
            vaxis=axis1 haxis=axis2 hminor=1 vminor=1
				des="BoxCox plot of RMSE * Lambda for &resp";
      title h=1.5 "Box-Cox Power Transform for &resp";
      label _rmse_ = "Root Mean Squared Error";
      run;
	%end;

    %if %index(&gplot,LOGL)>0 %then %do;
	 plot _like_ * _lambda_ = 1 /
			href=&lolam &hilam lhref=20 chref=red 
			vref=&lline lvref=33 cvref=red
		   vaxis=axis1 haxis=axis2 hminor=1 vminor=1
				des="BoxCox plot of Log Likelihood * Lambda for &resp";
      title h=1.5 "Box-Cox Power Transform for &resp";
      label _like_ = "Log Likelihood";
		run;
	%end;

	   %if %index(&gplot,EFFECT)>0 or %index(&gplot,INFL)>0 %then %do;
			%gskip;
			%end;
      %end;
   %if (&nmod>0) and %index(&gplot,EFFECT)>0 %then %do;
proc gplot data=&outplot;
      plot
         %do i = 1 %to &nmod;
            %scan(&model,&i) * _lambda_ = &i
            %end;
         / overlay anno=_labels_
            href=&lolam &hilam lhref=20 chref=red
            vref=&tval -&tval  lvref=33 cvref=red
           vaxis=axis3 haxis=axis4 hminor=1
			  des="BoxCox Effect plot for &resp";
       title h=1.5 "t-values for Model Effects on &resp";
       run;
	   %if %index(&gplot,INFL)>0 %then %do;
			%gskip;
			%end;
   %end;  /* if &gplot ^= NONE */
   %if &pplot ^= NONE or &gplot ^= NONE %then %do;
   title ;
   %end;
 
%done:
   proc datasets nolist nowarn;
      delete _NOBS_  _tmp_ _nomiss_ _reg_;
      %if (&nmod>0) and %index(&gplot,EFFECT)>0 %then %do;
      delete _labels_;
      %end;
   quit;

   %if %index(&gplot,INFL)>0 or %index(&pplot,INFL)>0
      %then %do;
         %boxinfl(data=&data, resp=&resp, model=&model, id=&id, fold=&fold,
                  gplot=&gplot, pplot=&pplot);
      %end;
%exit:
	%*-- Restore global options;
	%if &sysver >= 7 %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend;
 
 /*------------------------------------------------------------------*/
 /* NAME:    BOXINFL                                                 */
 /* PURPOSE: Computes Atkinson's score-test for power transformation */
 /*          and produces a constructed-variable influence plot for  */
 /*          the impact of observations on the choise of power.      */
 /*------------------------------------------------------------------*/
%macro boxinfl(
    resp=,                   /* name of response variable            */
    model=,                  /* independent variables in regression  */
    data=_last_,             /* input dataset                        */
    id=,                     /* ID variable for observations         */
	 fold=0,
    gplot=INFL,              /* Graphic plot?                        */
    pplot=INFL);             /* Printer plot?                        */
 
   data _cvar_;
      set &data;
		%if &fold=0 %then %do;
	      logy = log(&resp);       *-- geometric mean =mean log(y);
			%end;
		%else %do;
	      logy = log(&resp * (&fold-&resp));
			%end;
      %if %length(&id)=0 %then %do;
        %let id =_id_;
        _id_ = _n_;
      %end;
   proc means noprint;
      var logy;
      output out=_gm_ mean=gmean;
   data _cvar_;
      set _cvar_;
      drop gmean;
      if _n_=1 then do;
         set _gm_;
         gmean = exp(gmean);
         end;
		%if &fold=0 %then %do;
	      g = &resp * ( log ( &resp / gmean ) - 1);
			%end;
		%else %do;
	      g = &resp * log(&resp/gmean) - (&fold-&resp) * log(&fold-&resp/gmean) ;
			%end;
      label g='Constructed variable';
 
 /* produce values for partial-regression plot of residuals from
 /  &resp vs. residuals from constructed variable.  1-slope =
 /  power for transformation based on score test
 / -------------------------------------------------------------*/
   proc reg data=_cvar_ outest=_parm_ ;
     id &id;
    m0: model &resp=&model g;       * y = Xb + lambda g;
        output out=m0 rstudent=_resy_ cookd=_infl_;
   proc reg data=_cvar_ noprint;
     id &id;
    m1: model &resp=&model;
        output out=m1 r=_resx_;     * y - Xb;
    m2: model g =&model;
        output out=m2 r=_resg_;     * g - Xb;
   data _part_;
      keep _resx_ _resg_ &id _resy_ _infl_;
      merge m0 m1 m2;
   proc means noprint data=_part_;
      var _resg_;
      output out=_gm_ min=rg_min max=rg_max;
   data _slope_;
      set _parm_(keep=_model_ g);
      if (_model_='M0');
      xsys='1'; ysys='1';
      x=8; y=90;
      function = 'LABEL';
      position='3';  size=1.5;
      text = 'Slope:  '|| put(g,best5.);  output;
      position='F';
      text = 'Power:  '|| put(round(1-g,.25),best5.);  output;
      call symput('slope',put(g,best5.));
      call symput('power',put(round(1-g,.25),best5.));
      run;
   %if %index(&pplot,INFL)>0 %then %do;
      proc plot data=_part_;
         plot _resx_  * _resg_ = &id / vref=0;
         label _resx_ ="Partial &resp"
               _resg_ ='Partial Constructed Variable';
         title 'Partial Regression Influence plot for Box-Cox power';
         title2 "Slope: &slope   Power: &power";
      run;quit;
   %end;
 
%if %index(&gplot,INFL)>0 %then %do;
data _anno_;
   set _part_ ;
   length text $16;
   if _n_=1 then set _gm_;
   if abs(_resg_/(rg_max-rg_min)) > .5
    | abs(_resy_) > 3
    | _infl_> 4/(n-1);
   xsys='2'; ysys='2';
   x =  _resg_; y=_resx_ ;
   function='LABEL';
   if _resg_ > 0 then position='1';
                 else position='3';
   text=&id;
   data _anno_;
      set _anno_ _slope_;
   proc gplot data=_part_;
      plot _resx_  * _resg_ /
         vaxis=axis1 vminor=1 hminor=1
         vref=0 lvref=34 anno=_anno_
			name='boxinfl'
			des="BoxCox influence plot for &resp";

      axis1 label=(a=90);
      symbol i=rl h=1.3 v=circle ci=red;
      label _resx_ ="Partial &resp"
            _resg_ ='Partial Constructed Variable';
      title h=1.4 'Partial Regression Influence plot for Box-Cox power';
   run;quit;
	title;
%end;
   proc datasets nolist;
      delete _cvar_ _parm_ m0 m1 m2 _slope_;
   quit;
%mend;
 /*-------------------------------------------------------------------*/
 /* Copyright (c) 1987 by SAS Institute Inc. Cary NC 27511  USA       */
 /*                                                                   */
 /* NAME:    NUM(ber of )WORDS                                        */
 /* PURPOSE: Returns the number of words in a given list, with an     */
 /*          optional specification of word delimiters.               */
 /*-------------------------------------------------------------------*/
%macro numwords(lst,wordchar);
   %let i = 1;
   %if (%length(&wordchar)) %then %do;
      %let v = %scan(&lst,&i,&wordchar);
      %do %while (%length(&v) > 0);
         %let i = %eval(&i + 1);
         %let v = %scan(&lst,&i,&wordchar);
         %end;
      %end;
   %else %do;
      %let v = %scan(&lst,&i);
      %do %while (%length(&v) > 0);
         %let i = %eval(&i + 1);
         %let v = %scan(&lst,&i);
         %end;
      %end;
   %eval(&i - 1)
%mend;
