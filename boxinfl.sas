 /*------------------------------------------------------------------*/
 /* NAME:    boxinfl.sas                                             */
 /* PURPOSE: Computes Atkinson's score-test for power transformation */
 /*          and produces a constructed-variable influence plot for  */
 /*          the impact of observations on the choise of power.      */
 /*------------------------------------------------------------------*/
/*  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created: 12 Nov 1991 11:21:20                                     *
 * Revised:  10 Dec 1991 16:43:59                                    *
 * Version:  1.1                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
%macro boxinfl(
    resp=,                   /* name of response variable            */
    model=,                  /* independent variables in regression  */
    data=_last_,             /* input dataset                        */
    id=,                     /* ID variable for observations         */
    class=,                  /* names of class variables if any      */
    gplot=INFL,              /* Graphic plot?                        */
    pplot=INFL);             /* Printer plot?                        */
 
   data _cvar_;
      set &data;
      if &resp ^=. ;
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
 
 /* Handle class variables via dummy variables from GLMMOD
    Change the terms in the model to reflect the COL1-COLn
    variables produced by GLMMOD.
  */
   %if %length(&class)>0 %then %do;
   proc glmmod data=_cvar_ outdesign=_cmat_ outparm=_parm_ noprint;
      class &class;
      model &resp = g &model / noint;
   data _null_; set _parm_ end=eof;
      by effname notsorted;
      length modl $100;
      retain modl ' ';
      if effname='G' then return;
      else do;
         keepit = not last.effname;
         effname= ' COL'||trim(left(put(_colnum_,2.)));
         end;
      if keepit then modl = trim(modl) || effname;
      if eof then call symput('model',modl);
   run;
   data _cvar_;
      merge _cmat_(rename=(col1 = g))
            _cvar_(keep=&id);
   %end;
 
 /* produce values for partial-regression plot of residuals from
 /  &resp vs. residuals from constructed variable.  1-slope =
 /  power for transformation based on score test
 / -------------------------------------------------------------*/
   proc reg data=_cvar_ outest=_parm_ ;
     id &id;
    m0: model &resp=&model g;       * y = Xb + lambda g;
        output out=m0 rstudent=_resy_ cookd=_infl_;
*proc print data=_parm_;
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
      length function $8 text $20;
      if (_model_='M0');
      xsys='1'; ysys='1';
      x=8; y=90;
      function = 'LABEL';
      position='3';  size=1.2;
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
   set _part_ nobs=n;
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
      set _slope_ _anno_;
   proc gplot data=_part_;
      plot _resx_  * _resg_ /
         vaxis=axis1 haxis=axis1 vminor=1 hminor=1
         vref=0 lvref=34 anno=_anno_;
      axis1 label=(h=1.3) value=(h=1.2);
      symbol i=rl h=1.3 v=circle;
      label _resx_ ="Partial &resp"
            _resg_ ='Partial Constructed Variable';
      title h=1.4 'Partial Regression Influence plot for Box-Cox power';
   run;quit;
%end;
   proc datasets nolist;
      delete _cvar_ _parm_ m1 m2 _slope_;
   quit;
%mend;
