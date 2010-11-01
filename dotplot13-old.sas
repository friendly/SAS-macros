 /*-------------------------------------------------------------------*
  *    Name: dotplot.sas                                              *
  *   Title: Macro for dot charts                                     *
  *                                                                   *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  14 May 1989 09:12:26                                    *
  * Revised:  30 Dec 2001 17:48:38                                    *
  * Version:  1.3                                                     *
  *  - Added WHERE= parameter                                         *
  *  - Added XLOGBASE= parameter for log axis                         *
  *  - Added SYMBOL=                                                  *
  *  - Added ability to plot multiple XVAR (same scale)               *
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
       symbol=dot circle square,   /* symbol for observation */
       symht=1.5,
       color=black red blue,
       dline=34,             /* style of horizontal lines              */
       dcolor=gray,          /* color of horizontal lines              */
       errbar=,              /* variable giving length of error bar    */
                             /* for each observation                   */
       xlogbase=,
       xlogstyl=expand,
       name=DOTPLOT,        /* Name for graphic catalog entry         */
		 gout=                /* Name for graphic catalog               */
		 );
 
%if &yvar= %str() %then %do;
   %put ERROR: You must specify a YVAR= variable;
   %goto ENDDOT;
   %end;
%if &xvar= %str() %then %do;
   %put ERROR: You must specify XVAR= variable(s);
   %goto ENDDOT;
   %end;

%if "%upcase(&data)" = "_LAST_" %then %let data = &syslast;

%let connect=%upcase(&connect);
%if &ylabel = %str() %then %let ylabel=%upcase(&yvar);

%global nobs vref;
 /*--------------------------------------------------*
  | Sort observations in the desired order on Y axis |
  *--------------------------------------------------*/

%let group = %scan(&group &class,1,%str( ));    %*-- handle synonym;
%if (%length(&group.&ysortby)>0) AND &ysortby ^= NONE %then %do;
proc sort data=&data;
   by &group &ysortby;
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
  call symput( 'val' || left(put( sort_key, 3. )), trim(left(&yvar)) );
  output;     /* output here so sort_key is in sync */
 
  if _n_=1 then vref='';
  if last.&group & ^eof then do;
     sort_key+1;
     vref = trim(vref) || put(sort_key, 5.);
     call symput('val'|| trim(left(put(sort_key, 3.))), '  ' );
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
 
%let nv = %words(&xvar, root=_v_);
 /*---------------------------------------------------*
  | Annotate data set to draw horizontal dotted lines |
  *---------------------------------------------------*/
data _dots_;
   set _dot_dat;
      by &group;
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
			 %if &nv>1 
          	%then %str(x = max(of &xvar);); 
          	%else %str(x = &xvar;); 
			 output;
       %end;
       %else %if &connect = AXIS
          %then %do;
             xsys = '1';
             x = 100;  output;
             /*
          function='POINT';
          do x = 0 to 100 by 2;
             output;
             end;
            */
          %end;
 
   %if &group ^= _GROUP_ %then %do;
      if first.&group then do;
         xsys = '1';
         x = 98; size=1.5 * &symht;
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
   %do i=1 %to &nv;
       x = &&_v_&i - &errbar ;
       function = 'MOVE ';   output;
       text = '|';
       function = 'LABEL';   output;
       x = &&_v_&i + &errbar ;
       function = 'DRAW ';   output;
       function = 'LABEL';   output;
       %end;
data _dots_;
   set _dots_ _err_;
%end;
%gensym(n=&nv, symbols=&symbol, colors=&color, h=&symht);

*-- retrieve graphics parameters from GDSI,
    and make sure vpos= is large enough;

data _null_;
   rc=ginit();
   call gask('vpos',  vpos, rc);
   call gask('hpos',  hpos, rc);
	put "Original: " vpos= hpos=;
   call symput('ovpos',  compress(put(vpos,3.)));
   call symput('ohpos',  compress(put(hpos,3.)));
	if vpos < 1.75 * &nobs then do;
		hpos = ceil(hpos * (1.75 * &nobs)/vpos); 
		vpos = ceil(1.75 * &nobs);
	put "New: " vpos= hpos=;
		end;
   call symput('vpos',  compress(put(vpos,3.)));
   call symput('hpos',  compress(put(hpos,3.)));
run;
goptions vpos=&vpos hpos=&hpos;

 /*-----------------------------------------------*
  | Draw the dot plot, plotting formatted Y vs. X |
  *-----------------------------------------------*/

%if %length(&gout) %then %let gout=gout=&gout; 
proc gplot data= _dot_dat &GOUT ;
   plot
    %do i=1 %to &nv;
        sort_key * &&_v_&i = &i
        %end;
        /vaxis=axis1 vminor=0
         haxis=axis2 frame
         %if &nv>1 %then overlay;
         name="&name"
            des="dotplot of &data (&xvar)" 
     %if &vref ^= %str()
     %then    vref=&vref ;
     %if &xref ^= %str()
     %then    href=&xref lhref=21 chref=red ;
         annotate=_dots_;
   label   sort_key="&ylabel";
   format  sort_key _yname_.;
*   symbol1 v=&symbol h=1.4 c=black;
   axis1   order=(1 to &nobs by 1)
           major=none value=(j=r)
              offset=(1);
   axis2   %if %length(&xorder)>0 %then order=(&xorder) ;
     %if &xlogbase ^= %str()
          %then %do;
               logbase = &xlogbase
               %if &xlogstyl ^= %str()
                    %then %do;
                         %let xlogstyl = %upcase(&xlogstyl);
                         %if &xlogstyl ^= POWER & &xlogstyl ^= EXPAND
                            %then %do;
                             %put NOTE: XLOGSTYL must be "EXPAND" or "POWER".;
                             %put NOTE: "EXPAND" was used.;
                             %let xlogstyl = EXPAND;
                             %end;
                    logstyle = &xlogstyl
                    %end;
               %end;
           offset=(2);
   run; quit;

%enddot:
%let nobs=;
%let vref=;
goptions reset=symbol vpos=&ovpos hpos=&ohpos;
%mend dotplot;
 
 /*-----------------------------------------*
  |  Macro to generate a format of the form |
  |    1 ="&val1"  2="&val2" ...            |
  |  for observation labels on the y axis.  |
  *-----------------------------------------*/
%macro makefmt(nval);
  %if &sysver < 6 & "&sysscp"="CMS"
      %then %do;
        x set cmstype ht;              /* For CMS, must                */
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

%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(&word^= );
    %*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;
