 /*-------------------------------------------------------------------*
  *    Name: DOTPLOT.SAS                                              *
  *   Title: Macro for dot charts                                     *
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  14 May 1989 09:12:26                                    *
  * Revised:  12 Feb 1997 15:33:02                                    *
  * Version:  1.1                                                     *
  *  - Added WHERE= parameter                                         *
  *      From ``SAS System for Statistical Graphics, First Edition''  *
  *      Copyright(c) 1991 by SAS Institute Inc., Cary, NC, USA       *
  *                                                                   *
  *-------------------------------------------------------------------*/
%macro dotplot(
       data=_LAST_,          /* input data set                         */
       xvar=,                /* horizontal variable (response)         */
       xorder=,              /* plotting range of response             */
       xref=,                /* reference lines for response variable  */
       yvar=,                /* vertical variable (observation label)  */
       ysortby=&xvar,        /* how to sort observations               */
       ylabel=,              /* label for y variable                   */
       group=,               /* vertical grouping variable             */
       class=,               /* synonym for group=                     */
		 where=,               /* WHERE clause to subset data            */
		 by=,
       gpfmt=,               /* format for printing group variable     */
                             /* value (include the . at the end)       */
       connect=DOT,          /* draw lines to ZERO, DOT, AXIS, or NONE */
       dline=2,              /* style of horizontal lines              */
       dcolor=BLACK,         /* color of horizontal lines              */
       errbar=,              /* variable giving length of error bar    */
                             /* for each observation                   */
       name=DOTPLOT);        /* Name for graphic catalog entry         */
 
%if &yvar= %str() %then %do;
   %put ERROR: You must specify a yvar= variable;
   %goto ENDDOT;
   %end;

%if %upcase(&data) = _LAST_ %then %let data = &syslast;

%let connect=%upcase(&connect);
%if &ylabel = %str() %then %let ylabel=%upcase(&yvar);

%global nobs vref;
 /*--------------------------------------------------*
  | Sort observations in the desired order on Y axis |
  *--------------------------------------------------*/

%let group = %scan(&group &class,1,%str( ));    %*-- handle synonym;
%if &group ^= %str() OR &ysortby ^= %str() %then %do;
proc sort data=&data;
   by &by &group &ysortby;
%end;
 
 /*-----------------------------------------------------*
  | Add Sort_Key variable and construct macro variables |
  *-----------------------------------------------------*/
data _dot_dat;
  set &data;
  %if &group = %str() %then %do;
     %let group= _GROUP_;
     _group_ = 1;
  %end;
  %if %length(&where)>0 %then %do;
     where (&where);
  %end;
run;
 
data _dot_dat;
  set _dot_dat end=eof;
  retain vref ; drop vref;
  length vref $60;
     by &group;
  sort_key + 1;
  call symput( 'val' || left(put( sort_key, 3. )), trim(&yvar) );
  output;     /* output here so sort_key is in sync */
 
  if _n_=1 then vref='';
  if last.&group & ^eof then do;
     sort_key+1;
     vref = trim(vref) || put(sort_key, 5.);
     call symput('val'|| left(put(sort_key, 3.)), '  ' );
     end;
  if eof then do;
     call symput('nobs', put(sort_key, 4.));
     call symput('vref', trim(vref));
     end;
run;
 
%if &nobs=0 %then %do;
   %put DOTPLOT: Data set &data has no observations;
   %goto ENDDOT;
   %end;
%makefmt(&nobs);
 
 /*---------------------------------------------------*
  | Annotate data set to draw horizontal dotted lines |
  *---------------------------------------------------*/
data _dots_;
   set _dot_dat;
      by &by &group;
   length function $ 8 text $ 20;
   text = ' ';
   %if &connect = ZERO
       %then %str(xsys = '2';) ;
       %else %str(xsys = '1';) ;
   ysys = '2';
   line = &dline;
   color = "&dcolor";
   y  = sort_key;
   x = 0;
   function ='MOVE'; output;
 
   function ='DRAW';
   %if &connect = DOT | &connect = ZERO
       %then %do;
          xsys = '2';
          x = &xvar; output;
       %end;
       %else %if &connect = AXIS
          %then %do;
          function='POINT';
          do x = 0 to 100 by 2;
             output;
             end;
          %end;
 
   %if &group ^= _GROUP_ %then %do;
      if first.&group then do;
         xsys = '1';
         x = 98; size=1.5;
         function = 'LABEL';
         color='BLACK';
         position = 'A';
         %if &gpfmt ^= %str()
            %then %str(text = put(&group, &gpfmt ) ;) ;
            %else %str(text = &group ;) ;
         output;
      end;
   %end;
 
%if &errbar ^= %str() %then %do;
data _err_;
   set _dot_dat;
   xsys = '2'; ysys = '2';
   y = sort_key;
   x = &xvar - &errbar ;
   function = 'MOVE ';   output;
   text = '|';
   function = 'LABEL';   output;
   x = &xvar + &errbar ;
   function = 'DRAW ';   output;
   function = 'LABEL';   output;
data _dots_;
   set _dots_ _err_;
%end;
 /*-----------------------------------------------*
  | Draw the dot plot, plotting formatted Y vs. X |
  *-----------------------------------------------*/
proc gplot data= _dot_dat &GOUT ;
   plot sort_key * &xvar
        /vaxis=axis1 vminor=0
         haxis=axis2 frame
         name="&name"
			des="dotplot of &data" 
     %if &vref ^= %str()
     %then    vref=&vref ;
     %if &xref ^= %str()
     %then    href=&xref lhref=21 chref=red ;
         annotate=_dots_;
	%if %length(&by)>0 %then %do;
		by &by;
		%end;
   label   sort_key="&ylabel";
   format  sort_key _yname_.;
   symbol1 v='-' h=1.4 c=black;
   axis1   %if %length(&by)=0 %then
	           %str(order=(1 to &nobs by 1));
           major=none value=(j=r);
   axis2   %if %length(&xorder)>0 %then order=(&xorder) ;
           offset=(1);
   run;
%enddot:
%let nobs=;
%let vref=;
%mend dotplot;
 
 /*-----------------------------------------*
  |  Macro to generate a format of the form |
  |    1 ="&val1"  2="&val2" ...            |
  |  for observation labels on the y axis.  |
  *-----------------------------------------*/
%macro makefmt(nval);
  %if &sysver < 6 & "&sysscp"="CMS"
      %then %do;
        x set cmstype ht;              /* For SAS 5.18 on CMS, must    */
        x erase _yname_ text *;        /* erase format so that dotplot */
        x set cmstype rt;              /* can be used more than once   */
      %end;                            /* in a single SAS session      */
  %local i ;
  proc format;
       value _yname_
    %do i=1 %to &nval ;
       &i = "&&val&i"
       %end;
       ;
%mend makefmt;
