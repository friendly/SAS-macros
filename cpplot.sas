/*-------------------------------------------------------------------*
 *    Name: cpplot.sas                                               *
 *   Title: Plots of Mallow's C(p) and related statistics            *
 *          for model selection in linear models                     *
       Doc: http://www.datavis.ca/sasmac/cpplot.html           
 *                                                                   *
 * Any one or more of the following plots may be requested:          *
 *   CP  - Plot of C(p) vs. p for each subset of variables           *
 *   CD  - Plot of C(p)/p vs. p  (reference value CD=1)              *
 *   F   - Plot of F for omitted variables (reference value F=1)     *
 *   PROBF - Plot of Prob>F for omitted variables (ref value Pr=.05) *
 *   AIC  - Plot Akaike's Information Criterion vs. p                *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  8 Jun 1990 12:12:47                                     *
 * Revised:  26 May 2003 16:25:13                                    *
 * Version:  1.5                                                     *
 *   - use REG, not RSQUARE                                          *
 *   - added AIC plot                                                *
 * 1.5 Fixed validvarname for V7+.  Fixed GOUT=                      *
 *   - added htext=                                                  *
 *-------------------------------------------------------------------*/
%macro CPPLOT(
       yvar=,                /* name of dependent variable        */
       xvar=,                /* list of independent variables     */
       data=_LAST_,          /* name of input data set            */
       plotchar=1 2 3 4 5 6 7 6 8 9 0, /* to identify variables   */
       gplot=CP,             /* what to plot: any one or more of  */
       plot=CP,              /* what to plot: any one or more of  */
                             /* CP CD F PROBF AIC */
       pplot=NONE,           /* printer plots to do               */
       cpmax=30,             /* max value of C(p) plotted         */
       fmax=30,              /* max value of F plotted            */
	   htext=1.5,            /* height of text labels             */
	   options=,             /* additional REG model stmt opts    */
       name=CPPLOT,          /* name for graphic catalog entry    */
       gout=
       );

	%*-- Reset required global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=V6;
		%end;
	%else %do;
	   options nonotes;
		%end;

%let gplot = %upcase(&gplot);
%if &gout^=%str()  %then %let gout=GOUT=&gout;

%if %length(&yvar) = 0 %then %do;
   %put CPPLOT: YVAR = must be specified;
   %goto done;
   %end;
%if %length(&xvar) = 0 %then %do;
   %put CPPLOT: XVAR = must be specified;
   %goto done;
   %end;

proc reg data=&data outest=_models_ ;
     model &yvar = &xvar / selection=rsquare cp
	  %if %index(&gplot,AIC)>0 or %index(&pplot,AIC)>0 %then aic ;
	  &options;
 
data _models_;
     set _models_ end=eof;
     array indepvar{*} &xvar;
     drop _MODEL_ _DEPVAR_ _TYPE_ _IN_ _RMSE_
          &yvar &xvar intercep i chars maxp;
     %if &pplot = NONE %then %str(drop _lprf_ ;);
     retain maxp 0;
     maxp = max(maxp,_p_);
     _cpd_ = _cp_ / _p_;
     if dim(indepvar)>_P_-1 then do;
        _f_   = 1 + (_cp_-_p_)/( 1+dim(indepvar) - _p_);
       _probf_ = max(1- probf(_f_, 1+dim(indepvar), _edf_),.0001);
       _lprf_  = log10 (_probf_);
     end;
 
     label _cpd_ = 'C(P) / P'
           _f_   = 'F for Omitted Variables'
           _probf_='Prob>F for Omitted Variables'
           _lprf_ ='log (Prob>F for Omitted Variables)';
     * Calculate plotting symbol based on predictors in the model;
     Length symbol $12;
     chars = "&plotchar";
     symbol = '';
     do i=1 to dim(indepvar);
        if indepvar(i) ^= . then
        symbol = compress( symbol || scan(chars,i) );
        end;
     if eof then call symput('maxp',put(maxp,2.));
*proc print;
 
*-- Printer plots? --( only useful for Version >=6.07);
%if &pplot ^= NONE %then %do;
   %if &sysver < 6.08
       %then %do;
          %put WARNING: CPPLOT cannot label models adequately using
               PROC PLOT in SAS &sysver - use SAS 6.08 or later;
          %let symbol = %str( = symbol );
          %let place =;
       %end;
       %else %do;
           %let symbol = $ symbol = '*';
           %let place = placement=((h=2 -2 : s=right left)
                                   (v=1 -1 * h=0 -1 to -3 by alt)) ;
       %end;
 
   proc plot data=_models_;
      %if %index(&pplot,CP)>0 %then %do;
      plot _cp_ * _p_ &symbol
            _p_ * _p_ = '+'/ overlay
         &place haxis=1 to &maxp vaxis=0 to &cpmax by 2;
      %end;
      %if %index(&pplot,F )>0 %then %do;
      plot  _f_ * _p_ &symbol /
         &place haxis=1 to &maxp vref=1 vaxis=0 to &fmax by 2;
      %end;
      %if %index(&pplot,CD)>0 %then %do;
      plot _cpd_ * _p_ &symbol /
         &place haxis=1 to &maxp vref=1 vaxis=0 to &cpmax by 2 ;
      %end;
      %if %index(&pplot,PROBF)>0 %then %do;
      plot _lprf_ * _p_ &symbol /
         &place haxis=1 to &maxp vref = -1.3 vaxis=by 1 ;
      %end;
%end;
 
%if %index(&gplot,CP)>0 %then %do;
data labels;
   set _models_;
   by _P_;
   drop count symbol;
   length function color $8;
   xsys='2';
   x = _P_;
   text = symbol; function = 'LABEL';
   if first._P_ then do;
      out=0;
      count=0; end;
   if _CP_ > &cpmax then do;
      out+1;
      ysys='1'; y =  min(90 + 1.5*out, 99.5);
      color='RED';  size=1.2;
      if mod(out,2)=1
         then do; x=x -.05; position='4';  end;
         else do; x=x +.05; position='6';  end;
      output;
      end;
   else do;
      count+1;
      ysys = '2'; y = _CP_;
      color='BLACK'; size=&htext;
      if mod(count,2)=1
         then do; x = x -.05; position='4'; end;
         else do; x = x +.05; position='6'; end;
      output;
      end;
   if last._P_ & out>0 then do;
      x=_P_; y=90; ysys='1'; position='5';
      function='SYMBOL'; size=3 ; color='RED';
      style='MATH'; text='h';
      output;
      end;
data pline;
   xsys = '2'; ysys = '2';
   color='BLUE' ; line=10;
   x = 2;     y = 2;     function='MOVE'; output;
   x = &maxp; y = &maxp; function='DRAW'; output;
data labels;
   set labels pline;
%end;
 
*include label;
%if %index(&gplot,CD)>0 %then %do;
%label(data=_models_,x=_P_, y=_CPD_, text=symbol,
       xoff=-.05, pos=4, size=&htext,
       subset=_CPD_<&cpmax, out=labeld);
%end;
 
proc gplot data=_models_ &GOUT ;
   %if %index(&gplot,CP)>0 %then %do;
   plot _CP_ * _P_ / anno=labels
                     vaxis=axis1 haxis=axis2 name="&name"
							des="cpplot for &yvar";
   %end;
   %if %index(&gplot,CD)>0 %then %do;
   plot _CPD_* _P_ / anno=labeld vref=1 lvref=30
                     vaxis=axis1 haxis=axis2 ;
   %end;
   axis1 order=(0 to &cpmax by 10)
         offset=(1,12 pct)  label=(a=90 r=0);
   axis2 offset=(4) minor=none
;*         value=(h=1.3) label=(h=1.5);
   symbol v=dot h=1.1 ;
   run; quit;
 
%if %index(&gplot,F )>0 %then %do;
%label(data=_models_,x=_P_, y=_F_, text=symbol,
       xoff=.15, pos=6, size=&htext,
       subset=_F_<&fmax and _F_^=., out=labelf);
proc gplot data=_models_ &GOUT ;
   plot _F_  * _P_ / anno=labelf vref=1 lvref=30
                     vaxis=axis3 haxis=axis2 name="&name"
							des="cpplot for &yvar" ;
   axis3 order=(0 to &fmax by 10)
         offset=(1,2 pct) 
/*		 value=(h=1.3) label=(h=1.5 a=90 r=0); */
		 label=(a=90 r=0);
   run; quit;
%end;
%if %index(&gplot,PROBF)>0 %then %do;
%label(data=_models_,x=_P_, y=_PROBF_, text=symbol,
       xoff=-.05, pos=4, size=&htext,
       subset=1,             out=labelp);
proc gplot data=_models_ &GOUT ;
   plot _PROBF_* _P_ / anno=labelp
                     vaxis=axis4 haxis=axis2  name="&name"
							des="cpplot for &yvar";
   axis4 logstyle=expand logbase=10
         offset=(1,2 pct) 
/*		 value=(h=1.3) label=(h=1.5 a=90 r=0); */
		 label=(a=90 r=0);

   run; quit;
%end;

%if %index(&gplot,AIC)>0 %then %do;
%label(data=_models_,x=_P_, y=_AIC_, text=symbol,
       xoff=-.05, pos=4, size=&htext,
       subset=1,             out=_lab_);
proc gplot data=_models_ &GOUT ;
   plot _AIC_* _P_ / anno=_lab_
                     vaxis=axis1 haxis=axis2  name="&name"
							des="cpplot for &yvar";
   axis1 
         offset=(2) 
/*		 value=(h=1.3) label=(h=1.5 a=90 r=0); */
		 label=(a=90 r=0);
   axis2 offset=(4) minor=none
/*         value=(h=1.3) label=(h=1.5); */
   ;
   symbol v=dot h=1.1 ;
   run; quit;
%end;

%done:;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
%mend cpplot;
