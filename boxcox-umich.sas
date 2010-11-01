 /*------------------------------------------------------------------*
  *    Name: boxcox.sas                                              *
  *   Title: Power transformations by Box-Cox method                 *
  *          with graphic display of maximum likelihood solution,    *
  *               t-values for model effects, and influence of       *
  *               observations on choice of power.                   *
  *     Doc: http://www.math.yorku.ca/SCS/sasmac/boxcox.html         *
  *------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@YorkU.ca>        *
  * Created:  23 Oct 1991 10:20:14                                   *
  * Revised:  28 Jul 2000 11:26:57                                   *
  * Version:  1.2                                                    *
  *  1.2  Added ADD= parameter (additive constant); Allow no MODEL=  *
  *  -handle special missing values (.A-.Z). Fixed V8 lc varname bug *
  *                                                                  *
  *------------------------------------------------------------------*/
 
 /*------------------------------------------------------------------*/
 /* NAME:    BOXCOX                                                  */
 /* PURPOSE: Computes the maximum likelihood power transformation for*/
 /*          a linear regression with one or more predictors.        */
 /* INPUT:                                                           */
 /*   data=   the name of the data set holding the response and      */
 /*           predictor variables. (Default: most recently created)  */
 /*   resp=   name of the response variable for analysis.            */
 /*   model=  the independent variables in the regression, i.e.,     */
 /*           the terms on the right side of the = sign in the MODEL */
 /*           statement for PROC REG.                                */
 /* OUTPUT:                                                          */
 /*   out=    the name of the data set to hold the transformed data. */
 /*           Contains all original variables, with the transformed  */
 /*           response replacing the original variable.              */
 /*   outplot=the name of the data set containing _RMSE_, and t-     */
 /*           values for each effect in the model, with one observa- */
 /*           tion for each power value tried.                       */
 /* Source:                                                          */
 /*   This program incorporates portions of program code from the    */
 /*   macro ADXTRANS in the ADX system for experimental design       */
 /*   distributed as part of the SAS/QC product.                     */
 /*   Those programs bear the following copyright notice:            */
 /* Copyright (c) 1987 by SAS Institute Inc.  Cary NC 27511  USA     */
 /*------------------------------------------------------------------*/
%macro boxcox(
    resp=,                   /* name of response variable            */
    model=,                  /* independent variables in regression  */
    id=,                     /* ID variable for observations         */
    data=_last_,             /* input dataset                        */
    out=_data_,              /* output dataset with transformed resp */
    outplot=_plot_,          /* output dataset for plot of powers    */
    pplot=RMSE EFFECT INFL,  /* printer plots: RMSE, EFFECT, INFL    */
    gplot=RMSE EFFECT INFL LOGL ZOOM,
						/* graphic plots: RMSE, EFFECT, INFL, LOGL    */
	 add=0,                   /* additive constant for response */
    lopower=-2,              /* low value for power                  */
    hipower=2,               /* high value for power                 */
    npower=41, /*JB*/        /* number of power values in interval   */
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

/* 
   %if (%length(&model) = 0) %then %do;
       %put ERROR: BOXCOX: MODEL= must be specified;
       %goto exit;
       %end;
*/
   %if (%length(&resp) = 0) %then %do;
       %put ERROR: BOXCOX: RESP= must be specified;
       %goto exit;
       %end;
 
   %let nmod = %numwords(&model);
	%if %upcase(&data)=_LAST_ %then %let data = &syslast; 

 /*
 /  Get the number of non-missing observations; quick exit if none.
 /-------------------------------------------------------------------*/
   data _nomiss_; set &data;
		%if (&add>0) %then %do;
			&resp = &resp + &add;
			%end;
      if &resp > .Z;
   proc contents noprint data=_nomiss_ out=_NOBS_;
   data _null_; set _NOBS_;
      call symput('NOBS',left(put(nobs,best20.)));
   run;
   %if (&nobs <= 0) %then %goto done;
 
 /*
 /  Check for valid data (positive).
 /-------------------------------------------------------------------*/
   %let FLAG = 0;
   data _null_; set _nomiss_;
      if (&resp <= 0) then do;
         put "ERROR: Non-positive value of dependent variable &resp"
             "(obs=" _n_ ") " &resp ;
         call symput('FLAG','1');
         end;
   run;
   %if (&FLAG) %then %do;
        %put BOXCOX: Cannot compute Box-Cox transformation. Use ADD=;
        %goto done;
        %end;
   %put BOXCOX: Computing transforms of &resp ...;
 
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
** Datastep modified by JBesse 11/8/00 **;
   data _tmp_; set _tmp_;
      length _name2_ $20;
      array col{&NOBS};
      array new{&NOBS};
      keep _name2_ new1-new&NOBS;
      gmean = 0;
      do i = 1 to &NOBS; gmean = gmean + log(col{i}); end;
      gmean = exp(gmean / &NOBS);
 
      inc = (&hipower-&lopower)/(&npower-1);
      do i = 0 to &npower-1;
         lambda = &lopower + (i * inc);
         if (abs(lambda) < (inc/2)) then do
            z1 = log(gmean);
            do j = 1 to &NOBS;
               new{j} = gmean + (log(col{j})-z1)*gmean;
               end;
            end;
         else do;
            z1 = gmean**lambda;
            z2 = lambda*(gmean**(lambda-1));
            do j = 1 to &NOBS;
               new{j} = gmean + ((col{j}**lambda)-z1)/z2;
               end;
            end;
         _name2_ = "_tf" || left(put(i+1,best20.));
         output;
         end;
	data _tmp_; set _tmp_; rename _name2_ = _name_;
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
         call symput('elambda',left(put(lambda,best20.)));
         end;
      else if (trim(_name_) = '_LIKE_') then do;
         call symput('maxlike',left(put(col{imin},best20.)));
         end;
   proc transpose data=_reg_ out=_reg_;
*-------- Data step modified by JBesse 11-9-00 ----------*;
*   1. symput the log likelihood cutoff;
*   2. create macro variables for zoom plot axes;
*      (includes next _null_ data step to use symput vars);
*--------------------------------------------------------*;
   data &outplot;
      set _reg_ end=eof;
      drop _name_  adxlam _lolam_ _hilam_ _hirmse_ tval; ** _label_ removed;
      retain _lolam_ 10 _hilam_ -10 _hirmse_ 0 _lxzoom_ _hxzoom_;
      _lambda_ = adxlam;
      label _lambda_ = 'Box-Cox Power (lambda)'
            _like_   = 'Log Likelihood'
            conf     = "&conf Confidence Interval";
	   _cutoff_ = &maxlike - (cinv(&conf,1)/2);
      if (_like_ < _cutoff_ )  /* was 1.92 */
         then conf = " ";
         else do;
              conf = "*";
              _lolam_ = min(_lolam_,_lambda_);
              _hirmse_= max(_hirmse_,_rmse_);
			  if _lambda_ gt _hilam_ then do;
			    _hilam_ = _lambda_;
				_lxzoom_ = round(_hilam_-0.5,0.1);
				_hxzoom_ = round(_hilam_+0.5,0.1);
				end;
              end;

      if eof then do;
         call symput('lline',left(put(_cutoff_,best20.)));
         call symput('lolam',left(put(_lolam_,best20.)));
         call symput('hilam',left(put(_hilam_,best20.)));
         call symput('lxzoom',left(put(_lxzoom_,best20.)));
         call symput('hxzoom',left(put(_hxzoom_,best20.)));
         call symput('hirms',left(put(_hirmse_,best20.)));
         tval = tinv(1-(1-&conf)/2, &nobs-&nmodp1 );
         call symput('tval', left(put(tval,best10.)));
         end;
   run;

   data _null_; set &outplot end=eof; retain _y1_ _y2_ _minl_ _maxl_;
     if _n_=1 then do; _y1_=.; _y2_=.; _minl_=_like_; _maxl_=_like_; end;
	 if _like_ lt _minl_ then _minl_=_like_;
	 if _like_ gt _maxl_ then _maxl_=_like_;
     if round(_lambda_,0.1) = &lxzoom then _y1_=_like_; 
   	 if round(_lambda_,0.1) = &hxzoom then _y2_=_like_;
	 if eof then do;
	   _add_=(0.05)*(_maxl_-_minl_);
	   _ymax_=_maxl_+_add_;
	   call symput('hiyzoom',left(put(_ymax_,best20.)));
	   call symput('add',left(put(_add_,best20.)));
	   _ymin_=min(_y1_,_y2_);
	   call symput('loyzoom',left(put(_ymin_,best20.)));
	   end;
   run; quit;
options nomprint nomlogic nosymbolgen;

   options notes;
   proc print data=&outplot label;
      var _lambda_ _like_ _rmse_ conf;
   run;
   %put BOXCOX: Estimated Power Transformation, Lambda = &elambda.;
 
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
            / href=&lolam &hilam ;
         label _rmse_ = "Root Mean Squared Error for &resp";
      run;
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
 
proc gplot data=&outplot;
   %do i = 1 %to &nmod;
      symbol&i i=spline v=%scan(&sym,&i) c=black h=1;
      %end;
      axis1 label=(a=90);
      axis2 label=('Box-Cox Power (' f=cgreek 'l' ')' ) 
            offset=(3);
      axis3 label=(a=90 't-value');
      axis4 label=('Box-Cox Power (' f=cgreek 'l' ')' ) 
            offset=(2,9);

   %if %index(&gplot,RMSE)>0 %then %do;
      plot _rmse_ * _lambda_ = 1 /
            href=&lolam &hilam lhref=20 chref=red
            vref=&hirms        lvref=33 cvref=red
            vaxis=axis1 haxis=axis2 hminor=1 vminor=1
				des="BoxCox plot of RMSE * Lambda for &resp";
      title h=1.5 "Box-Cox Power Transform for &resp";
      label _rmse_ = "Root Mean Squared Error";
	%end;

** 2 plots added by JBesse 11/8/00;
    %if %index(&gplot,LOGL)>0 %then %do;
	 plot _like_ * _lambda_ = 1 /
	   href=&lolam &hilam lhref=20 
	   vref=&lline lvref=3
	   vaxis=axis1 haxis=axis2 hminor=1 vminor=1
				des="BoxCox plot of Log Likelihood * Lambda for &resp";
      title h=1.5 "Box-Cox Power Transform for &resp";
      label _like_ = "Log Likelihood";
	%end;
    %if %index(&gplot,ZOOM)>0 %then %do;
	 axis5 order=(&loyzoom to &hiyzoom by &add) label=(a=90);
     axis6 label=('Box-Cox Power (' f=cgreek 'l' ')' ) 
            order=(&lxzoom to &hxzoom by .2) offset=(3);
	 plot _like_ * _lambda_ = 1 /
	   href=&lolam &hilam lhref=20 
	   vref=&lline lvref=3
	   vaxis=axis5 haxis=axis6 hminor=1 vminor=1
				des="BoxCox plot of Log Likelihood * Lambda for &resp";
      title h=1.5 "Box-Cox Power Transform for &resp";
      label _like_ = "Log Likelihood";
	  
	  run;
     %end;

	   %if %index(&gplot,EFFECT)>0 or %index(&gplot,INFL)>0 %then %do;
			*%gskip;
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
			*%gskip;
			%end;
      %end;
   %end;  /* if &gplot ^= NONE */
   %if &pplot ^= NONE or &gplot ^= NONE %then %do;
   title ;
   %end;
 
%done:
   proc datasets nolist;
      delete _NOBS_  _tmp_ _nomiss_ _reg_;
      %if (&nmod>0) and %index(&gplot,EFFECT)>0 %then %do;
      delete _labels_;
      %end;
   quit;
   %if %index(&gplot,INFL)>0 or %index(&pplot,INFL)>0
      %then %do;
         %boxinfl(data=&data, resp=&resp, model=&model, id=&id,
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
    gplot=INFL,              /* Graphic plot?                        */
    pplot=INFL);             /* Printer plot?                        */
 
   data _cvar_;
      set &data;
      logy = log(&resp);  *--find geometric mean =mean log(y);
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
      g = &resp * ( log ( &resp / gmean ) - 1);
      label g='Constructed variable';
 
 /* produce values for partial-regression plot of residuals from
 /  &resp vs. residuals from constructed variable.  1-slope =
 /  power for transformation based on score test
 / -------------------------------------------------------------*/
   proc reg data=_cvar_ outest=_parm_ ;
     id &id;
    m0: model &resp=&model g;       * y = Xb + lambda g;
   proc reg data=_cvar_ noprint;
     id &id;
    m1: model &resp=&model;
        output out=m1 r=_resx_;     * y - Xb;
    m2: model g =&model;
        output out=m2 r=_resg_;     * g - Xb;
   data _part_;
      keep _resx_ _resg_ &id;
      merge m1 m2;
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
   if abs(_resg_/(rg_max-rg_min)) > .5;
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
      delete _cvar_ _parm_ m1 m2 _slope_;
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
