/*-------------------------------------------------------------------*
 * CATPLOT SAS    Sample macro to plot observed and predicted logits *
 *                for logit models fit by PROC CATMOD.               *
 *                                                                   *
 * The program is not very general. See GLOGIT SAS for the data set  *
 * for which it was designed.                                        *
 *-------------------------------------------------------------------*
 *  Author:  Michael Friendly            <friendly@yorku.ca>         *
 * Created:  9 May 1991 12:20:09                                     *
 * Revised:  9 May 1991 12:20:09                                     *
 * Version:  1.0                                                     *
 *                                                                   *
 *-------------------------------------------------------------------*/
%macro catplot(
    data=_last_,  /* out= data set from PROC CATMOD               */
    class=,       /* variable for curves within each plot [0/1]   */
    byvar=,       /* plot for each level of by variables          */
    x=,           /* horizontal value for plot [NUMERIC]          */
    xc=,          /* horizontal value for plot [CHAR]             */
    z=1,          /* std. error multiple for confidence intervals */
    anno=)        /* additional input annotate data set           */
    ;
data &data;
   set &data;
   drop _sample_ _type_ _number_ ;
   if _type_='FUNCTION';
proc sort;
   by &byvar &class &x;
proc print;
   id &byvar &class &x;
   var _obs_ _seobs_ _pred_ _sepred_ _resid_;
   format _obs_ _pred_ 8.3 _seobs_ _sepred_ _resid_ 8.4;
data anno;
   set &data;
   by &byvar &class;
   length function $8;
   xsys = '2'; ysys='2';
   %if &x ^= %str() %then %do;
      %let px = &x;
      x = &x;
      %end;
   %else %do;
      %let px = &xc;
      xc = &xc;
      %end;
/*
   if &class=0 then color='BLACK';  *NB: assumes class var is binary;
               else color='RED';
   line=1;
   if first.&class then do;
      y = _pred_; function='MOVE'; output;
      end;
   else do;
      y = _pred_; function='DRAW'; output;
      end;
*/
   *-- plot value +- &z * std error;
    line = 33;
    y = _pred_ + &z * _sepred_ ; function='MOVE'; output;
    y = _pred_ - &z * _sepred_ ;
    y = max(-5,y);  /* trim */   function='DRAW'; output;
    y = _pred_                 ; function='MOVE'; output;
 
%let ganno=;
%if &anno ^= %str() %then %do;
   %let ganno=anno=&anno;
   %end;
 
proc gplot data=&data &ganno;
   plot _pred_ * &px = &class
        / anno=anno frame
          haxis=axis1 hminor=0 vaxis=axis2;  /* Must define AXIS stmts*/
   %if &byvar ^= %str() %then %do;
      by &byvar;
      %end;
*symbol1 i=none v=+      h=1.5 c=black;
*symbol2 i=none v=square h=1.5 c=red  ;
%mend catplot;
