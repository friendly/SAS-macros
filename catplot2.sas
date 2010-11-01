%macro catplot(
    data=_last_,  /* out= data set from PROC CATMOD               */
    class=,       /* variable for curves within each plot         */
    byvar=,       /* plot for each level of by variables          */
    byfmt=,       /* format for values of by variables            */
    x=,           /* horizontal value for plot [NUMERIC]          */
    xc=,          /* horizontal value for plot [CHAR]             */
    y=_pred_,     /* ordinate for points (_pred_ or _obs_)        */
	 type=FUNCTION,
    z=1,          /* std. error multiple for confidence intervals */
    anno=,        /* additional input annotate data set           */
    clfmt=,       /* how to format values of class variable       */
    clside=last,  /* side for labels of class var (FIRST|LAST)    */
    xfmt=,        /* format for X variable                        */
    posfmt=,      /* format to translate class var to position    */
    vaxis=axis1,
    haxis=axis2,
	 htext=1.4,
	 name=catplot)
    ;
%let class1=%scan(&class,1);
%let class2=%scan(&class,2);
 
%if &x ^= %str() %then %do;
   %let px = &x;
   %end;
%else %do;
   %if &x ^= %str() %then %do;
       %put CATPLOT: Either X= or XC= variable must be specified;
       %goto DONE;
       %end;
   %let px = &xc;
   %end;
 
proc summary data=&data nway;
   class &byvar &class &px;
   var _pred_;
	where (_type_="&type");
   output out=pred2 mean=_pred_;
proc sort;
   by &byvar &class &px;
proc print;
   id &byvar &class &px;
data anno2;
   set pred2;
   by &byvar &class1;
   length text $20 function $8;
   drop _type_ _freq_ _pred_ &class;
   xsys='2'; ysys='2';
*   style='DUPLEX';
	size=&htext;
   %if &x ^= %str() %then %do;
      x = &x;
      %end;
   %else %do;
      xc = &xc;
      %end;
   if &clside..&class1 then do;
      y=_pred_;
      text='00'x ||put(&class1,&clfmt) ||'00'x;
      %if %upcase(&clside) = LAST %then %do;
         position = '6';
      %end;
      %else %do;
         position='4'; %end;
      %if &posfmt ^= %str() %then %do;
      position = put(&class,&posfmt);
      %end;
      function = 'LABEL'; output;
      end;
   %if &byvar ^= %str() %then %do;
      goptions hby=0;
      if first.&byvar then do;
         xsys='1'; ysys='1';
         x = 5; y=95; position='6';
         text = put(&byvar,&byfmt);
         function = 'LABEL'; output;
         end;
      %end;
*roc print;
%let ganno=;
%if &anno ^= %str() %then %do;
   %let ganno=anno=&anno;
   %end;
 
proc gplot data=pred2 &ganno;
   format &px &xfmt;
   plot &y * &px = &class
        / anno=anno2 frame nolegend
          haxis=&haxis vaxis=&vaxis
          hminor=0 vminor=1
			 name="&name";
   %if &byvar ^= %str() %then %do;
      by &byvar;
      %end;
%done:
%mend;
