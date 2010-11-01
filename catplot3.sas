/*-------------------------------------------------------------------*
 * CATPLOT SAS    Macro to plot observed and predicted logits for    *
 *                logit models fit by PROC CATMOD.                   *
 *                                                                   *
 * The horizontal variable may be character (XC=) or numeric (X=).   *
 * A separate curve is drawn for each value of the CLASS= variable,  *
 * connecting predicted values, with optional standard error bars,   *
 * and separate plots are drawn for each value of the BYVAR= variable*
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <FRIENDLY@VM1.YorkU.CA>     *
 * Created:  9 May 1991 12:20:09         Copyright (c) 1992          *
 * Revised:  28 Feb 1992 17:04:51                                    *
 * Version:  1.2                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
%macro catplot(
    data=_last_,  /* OUT= data set from PROC CATMOD                 */
    class=,       /* variable for curves within each plot           */
    byvar=,       /* plot for each level of by variables            */
    byfmt=$16.,   /* format for by variable                         */
    x=,           /* horizontal value for plot [NUMERIC]            */
    xc=,          /* horizontal value for plot [CHAR]               */
    y=_obs_,      /* ordinate for plotted points (_pred_ or _obs_)  */
    z=1,          /* std. error multiple for confidence intervals   */
                  /* e.g., z=1.96 gives 95% CI. No error bars: z=0  */
    zoff=0,       /* horizontal offset for error bars, in %         */
    anno=,        /* additional input annotate data set             */
    vaxis=axis1,  /* axis statement for logit axis                  */
    haxis=axis2,  /* axis statement for horizontal axis             */
    legend=,      /* legend statement for custom CLASS legend       */
    colors=BLACK RED BLUE GREEN);  /* colors for class levels       */
    ;
 
%if &x ^= %str() %then %do;
   %let px = &x;
   %end;
%else %do;
   %if &xc = %str() %then %do;
       %put CATPLOT: Either X= or XC= variable must be specified;
       %goto DONE;
       %end;
   %let px = &xc;
   %end;
 
 %*-- Select logit (_type_='FUNCTION') observations ;
data _pred_;
   set &data;
   drop _sample_ _type_ _number_ ;
   if _type_='FUNCTION';
proc sort;
   by &byvar &class &px;
proc print;
   id &byvar &class &px;
   var _obs_ _seobs_ _pred_ _sepred_ _resid_;
   format _obs_ _pred_ 8.3 _seobs_ _sepred_ _resid_ 8.4;
 
data _anno_;
   set _pred_;
   by &byvar &class;
   length function color $8;
   retain cl 0;
   drop _seobs_ _sepred_ _resid_ cl;
   %if &byvar ^= %str() %then %do;
      goptions hby=0;
      if first.&byvar then do;
         xsys='1'; ysys='1';
         x = 5; y=95; position='6';
         text = put(&byvar,&byfmt);
         function = 'LABEL'; output;
         end;
      if first.&byvar then cl=0;
      %end;
 
   xsys = '2'; ysys='2';
   %*-- Set X or XC variable ;
   %if &x ^= %str() %then %do;
      x = &x;
      %end;
   %else %do;
      xc = &xc;
      %end;
 
   if first.&class then cl+1;
   line=cl;
   color = scan("&colors",cl);
   if first.&class then do;
      y = _pred_; function='MOVE'; output;
      end;
   else do;
      y = _pred_; function='DRAW'; output;
      end;
 
    %if &z > 0 %then %do;
    %*-- plot value +- &z * std error;
    line = 33;
    y = _pred_ + &z * _sepred_ ;
    y = min( 5,y);  /* trim */   function='MOVE'; output;
    xsys = '7'; xsave= x;
    x = &zoff*(mod(cl,2) -.5);   function='MOVE'; output;
    x = 0;
    y = _pred_ - &z * _sepred_ ;
    y = max(-5,y);  /* trim */   function='DRAW'; output;
    xsys = '2'; xsave= x;
    y = _pred_                 ; function='MOVE'; output;
    %end;
 
%if &anno ^= %str() %then %do;
   data _anno_;
      set _anno_ &anno;
   %end;
 
proc gplot data=_pred_;
   plot &y * &px = &class
        / anno=_anno_ frame
        %if &legend ^= %str() %then %do;
          legend=&legend
        %end;
          haxis=&haxis hminor=0
          vaxis=&vaxis vminor=1;
   %if &byvar ^= %str() %then %do;
      by &byvar;
      %end;
*symbol1 i=none v=circle h=1.8 c=black;
*symbol2 i=none v=square h=1.6 c=red  ;
*symbol3 i=none v=$      h=1.6 c=blue ;
*symbol4 i=none v=:      h=1.6 c=green;
run; quit;
%done:
   goptions hby=;
%mend catplot;
