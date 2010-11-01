 /*-------------------------------------------------------------------*
  *    Name: intplot.sas                                              *
  *   Title: Draw an interaction plot matrix for an n-way design      *
  *     Displays all main effects and two-factor interactions in an   *
  *     n-way factorial design.                                        *
  * %intplot(data=, class=, response=);                               *
  *-------------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>         *
  * Created:  4 Sep 1993 11:17:40                                     *
  * Revised:  04 Aug 2005 11:58:59                                    *
  * Version:  1.4                                                     *
  *  1.4                                                              *
  *  - Fixed error min/max/inc bug when n=1 per cell                  *
  *  - Added calculation of # of factor levels                        *
  *  - Fixed buglet with ADJUST                                       *
  *  - Fixed buglet with upcase(name), and witrh scan                 *
  *-------------------------------------------------------------------*/
 
%macro INTPLOT(
   data =_LAST_,          /* data set to be plotted             */
   response=,             /* response variable                  */
   var  =,                /* names of factor variables          */
   class=,                /* names of factor variables          */
   tvalue=2,              /* SE multiple for error bars         */
   min=, max=, inc=,      /* min, max, incr for response scale  */
   adjust=,               /* multiple comparison adj for lsmeans*/
   symbols=%str(circle square dot - $ triangle + : = _),
   colors=BLACK RED BLUE GREEN BROWN YELLOW ORANGE PURPLE,
   ltype=1 20 41 21 7 14 33 12,
   interp=join join join join join join join join,
   gout=GSEG);            /* graphic catalog for plot matrix    */
 
 options nonotes;

%let abort=0;
%if %length(&var)=0 %then %let var=&class;
%if %length(&var)=0 %then %do;
   %put ERROR: You must specify VAR= (or CLASS=) factor variables;
   %let abort=1;
   %goto DONE;
	%end;
	
%if %length(&response)=0 %then %do;
   %put ERROR: You must specify the RESPONSE= variable;
   %let abort=1;
   %goto DONE;
	%end;
	
*-- Parse variables list;
*let var = %upcase(&var);
%let nvar = %numwords(&var);
%if &nvar < 2 or &nvar > 10 %then %do;
   %put Cannot do an interaction plot matrix for &nvar variables ;
   %goto DONE;
   %end;
 
proc contents data=&DATA out=_work_ noprint;
data _null_;
   set _work_(keep=name type label);
   do i=1 to &nvar;
       if upcase(name) = scan(upcase("&var"),i,' ') then do;
          if type = 2 then miss="' '";
                      else miss='.';
          call symput('VMISS'||put(i,1.), miss);
       end;
   end;
run;

*-- Determine number of factor levels;
proc summary data=&data;
	class &var;
	var &response;
	output out=_work_ n=n;
	
proc freq data=_work_ %if &sysver>6.10 %then noprint;;
   tables _type_ / noprint out=_levels_;
data _levels_;
	set _levels_;
	if _type_ in (1 2 4 8 16 32 64 128 256 512 1024);
proc sort out=_levels_;
	by descending _type_;
*proc print;
data _null_;
	set _levels_ end=eof;
	length levels $30;
	retain levels '';
	levels = trim(levels) || put(count, 3.);
	if eof then do;
		call symput('levels',trim(left(levels)));
		end;
run;
%put NOTE: Levels = &levels;

 /*----------------------------------------------------*
  | Determine least squares means and stderr           |
  *----------------------------------------------------*/
%let model = %scan(&var,1);
%do i=2 %to &nvar;
    %let model = &model | %scan(&var,&i);
    %end;
  proc glm data=&data outstat=_stat_ noprint;
    class &var;
    model &response = &model ;
    lsmeans &model @2 /
	 %if (%length(&adjust)>0) and (&sysver>6.10) %then
	 	adjust=&adjust ; 
	 	stderr out=_means_;

%*-- extract MSE and dfe;
data _null_;
     set _stat_;
     where (_source_='ERROR');
     mse = ss / df;
     call symput('MSE', trim(left(put(mse,12.4))));
     call symput('DFE', trim(left(put(df,12.))));
run;
data _means_;
   set _means_;
   drop _name_ lsmean ebar;
   &response = lsmean;
	if stderr ^=.
		then ebar = &tvalue * stderr;
		else ebar = 0;   /* should use &tvalue * sqrt(&MSE);  */
   _lower_ = sum(lsmean, -ebar);
   _upper_ = sum(lsmean,  ebar);
proc print data=_means_;
 
 /*----------------------------------------------------*
  | Determine min/max of response means for all panels |
  *----------------------------------------------------*/
%if &min=%str() | &max=%str() %then %do;
proc summary data=_means_;
   var _lower_ _upper_;
   output out=_range_ min=minlo minhi max=maxlo maxhi;
 
data _null_;
   set _range_;
   min = minlo;
   max = maxhi;
   inc= abs(max - min)/5;
   pow = 10**floor( log10(inc) );
   nice=1000;
   do in = 1, 2, 2.5, 4, 5;
      ut = in * pow;
      if abs(inc-ut) < nice then do;
         nice = abs(inc-ut);
         best = ut;
      end;
   end;
   inc=best;
   min = inc * floor(min/inc);
   max = inc * ceil (max/inc);
   put min= max= inc=;
   call symput('max', trim(left(put(max,best.))) );
   call symput('min', trim(left(put(min,best.))) );
   call symput('inc', trim(left(put(inc,best.))) );
run;
%end;
%gdispla(OFF);
 
  /* determine how many graphs are already in the catalog */
%let grafnum=0;    * number of plots in catalog;
  proc catalog catalog=gseg et=grseg;
  	contents out=_work_;
	run;
  data _null_; 
   set _work_ end=last; 
	if (last) then call symput('grafnum',_n_); 
	run;
%put grafnum= &grafnum;

title h=0.01 ' ';
%let plotnum=0;    * number of plots made;
%let replay = ;    * replay list;
%let lh = &nvar;   * label height;
 
%do i = 1 %to &nvar;                   /* rows */
   %let vi = %scan(&var , &i );
   %let li = %scan(&levels , &i );
   %let li1= %eval(&li + 1 );
 
   %do j = 1 %to &nvar;                /* cols */
      %let vj = %scan(&var , &j );
      %let grafnum = %eval(&grafnum+1);
      %let plotnum = %eval(&plotnum+1);
      %let replay  = &replay &plotnum:&grafnum ;
 
      %*--- Set the symbol statements: plot symbols constant in each row;
      %let in = %scan(&interp , &j );
      %gensym(n=&li, h=&nvar, symbols=&symbols, colors=&colors,
              l=&ltype,int=&in );
 
      %*--- compute where clause to select means for this panel;
      %do k = 1 %to &nvar;
          %let wh&k = %scan(&var, &k) = &&vmiss&k;
          %end;
      %let wh&i = &vi ^= &&vmiss&i;
      %let wh&j = &vj ^= &&vmiss&j;
      %let where= (&wh1);
      %do k=2 %to &nvar;
          %let where = &where & (&&wh&k);
          %end;
 
      %put plotting &vi vs. &vj  where: (&where);
         proc sort data=_means_ out=_panel_;
            where (&where);
            by &vi;
 
      %*--- Construct annotate data set for error bars;
      %if &&vmiss&j = .
          %then %bars(data=_panel_, x=&vj, class=&vi, colors=&colors);
          %else %bars(data=_panel_,xc=&vj, class=&vi, colors=&colors);
 
      %if &i = &j %then %do;           /* diagonal panel */
*  proc print data=_bars_;
         data title;
            length text $8;
            xsys = '1'; ysys = '1';
            x = 50; y = 0;
            text = "&vi";
            position='2';
            size = 2 * &nvar;
            function = 'LABEL';  output;
            %if &&vmiss&i = .
               %then %points(data=_panel_, x=&vi, response=&response,
                       class=&vi, colors=&colors, sym=&symbols);
               %else %points(data=_panel_,xc=&vi, response=&response,
                       class=&vi, colors=&colors, sym=&symbols);

*proc print data=_points_;
         data _bars_;
            set _bars_ _points_ title;
         proc gplot data = _panel_;
            plot &response * &vi = &li1
            / frame anno=_bars_ vaxis=axis1 haxis=axis2;
         axis1 label=none
               %if &j > 1 %then value=none ;
                          %else value=(h=&lh) ;
               order=(&min to &max by &inc)
               major=none minor=none offset=(2);
         axis2 label=none
               %if &i < &nvar %then value=none ;
                          %else value=(h=&lh) ;
               major=none minor=none offset=(10);
         run;
      %end;
 
      %else %do;                       /* off-diagonal panel */
         proc gplot data = _panel_;
            plot &response * &vj = &vi
            / frame anno=_bars_ nolegend vaxis=axis1 haxis=axis2;
         axis1 label=none
               %if &j > 1 %then value=none ;
                          %else value=(h=&lh) ;
                   /*else major=(h=-%eval(&nvar-1)) ; */
               order=(&min to &max by &inc)
               major=none minor=none offset=(2);
         axis2 label=none
               %if &i < &nvar %then value=none ;
                          %else value=(h=&lh) ;
                   /*else major=(h=-%eval(&nvar-1)) ; */
               major=none minor=none offset=(8);
         run;
      %end;
 
   %end; /* cols */
%end;    /* rows */
 
%gdispla(ON);
 
proc greplay igout=gseg
              gout=&gout  nofs
             template=scat&nvar
             tc=templt ;
%if &nvar = 2 %then %do;
  TDEF scat2 DES="scatterplot matrix 2x2"
           1/ ULX=0  ULY=100   URX=52  URY=100
              LLX=0  LLY=52    LRX=52  LRY=52
           2/ copy=1 XLATEX= 48       /*  Panels are numbered: */
           3/ copy=1 XLATEY=-48                /*    1   2     */
           4/ copy=3 XLATEX= 48;               /*    3   4     */
%end;
 
%if &nvar = 3 %then  %TDEF(&nvar,33,33);
%if &nvar = 4 %then  %TDEF(&nvar,25,25);
%if &nvar = 5 %then  %TDEF(&nvar,20,20);
%if &nvar = 6 %then  %TDEF(&nvar,17,16);
%if &nvar = 7 %then  %TDEF(&nvar,15,14);
%if &nvar = 8 %then  %TDEF(&nvar,13,12);
%if &nvar = 9 %then  %TDEF(&nvar,12,11);
%if &nvar =10 %then  %TDEF(&nvar,10,10);
 
   TREPLAY &replay;
run;
%*-- Clean up;
proc datasets nolist nowarn library=work;
	delete _work_ _bars_  _levels_ _means_
	;
    quit;

%DONE:
%if &abort %then %put ERROR: The INTPLOT macro ended abnormally.;
  options notes;
%mend INTPLOT;
 
 /*------------------------------------------------------*
  |  Macro to construct standard error bars around means |
  *------------------------------------------------------*/
%macro bars(data=, x=, xc=, class=, colors=);
  data _bars_;
     set &data;
     by &class;
   length function color $8;
   retain cl 0;
   drop cl;
   xsys = '2'; ysys='2';
   %*-- Set X or XC variable ;
   %if &x ^= %str() %then %do;
      x = &x;
      %end;
   %else %do;
      xc = &xc;
      %end;
 
   if first.&class then cl+1;
*  line= scan("&lines",cl);
   color = scan("&colors",cl);
   y = _upper_; function = 'MOVE    '; output;
   y = _lower_; function = 'DRAW    '; output;
 
   xsys = '7';  *-- relative percent for top/bottom of error bars;
   x = -1     ; function = 'MOVE    '; output;
   x = +2     ; function = 'DRAW    '; output;
   y = _upper_;
   x =  0     ; function = 'MOVE    '; output;
   x = -2     ; function = 'DRAW    '; output;
%mend bars;
 
 /*------------------------------------------------------*
  |  Macro to draw points with specified color/symbol    |
  *------------------------------------------------------*/
%macro points(data=, x=, xc=, response=, class=, colors=, sym=);
  data _points_;
     set &data;
     by &class;
   length function color $8;
   retain cl 0;
   drop cl;
   xsys = '2'; ysys='2';
   %*-- Set X or XC variable ;
   %if &x ^= %str() %then %do;
      x = &x;
      %end;
   %else %do;
      xc = &xc;
      %end;
 
   if first.&class then cl+1;
   text  = scan("&sym",cl);
   color = scan("&colors",cl);
   size = &nvar;
   y = &response; function = 'SYMBOL  '; output;
%mend points;
 
%macro TDEF(nv, size, shift );
%* ---------------------------------------------------------------;
%* Generate a TDEF statement for a scatterplot matrix             ;
%* Start with (1,1) panel in upper left, and copy it across & down;
%* ---------------------------------------------------------------;
%local i j panl panl1 lx ly;
 
   TDEF scat&nv DES="scatterplot matrix &nv x &nv"
   %let panl=0;
   %let lx = &size;
   %let ly = %eval(100-&size);
   %do i = 1 %to &nv;
   %do j = 1 %to &nv;
       %let panl  = %eval(&panl + 1);
       %if &j=1 %then
          %do;
             %if &i=1 %then %do;      %* (1,1) panel;
               &panl/
                ULX=0  ULY=100   URX=&lx URY=100
                LLX=0  LLY=&ly   LRX=&lx LRY=&ly
                %end;
             %else
                %do;                  %* (i,1) panel;
                   %let panl1 = %eval(&panl - &nv );
               &panl/ copy= &panl1 xlatey= -&shift
                %end;
          %end;
       %else
          %do;
               %let panl1 = %eval(&panl - 1);
               &panl/ copy= &panl1 xlatex= &shift
          %end;
   %end;
   %end;
     %str(;);      %* end the TDEF statement;
%mend TDEF;
 
 /*----------------------------------------------------*
  |  Macro to generate SYMBOL statement for each level |
  *----------------------------------------------------*/
%macro gensym(n=1, h=1.5,
              symbols=%str(- + : $ = X _ Y),
              l=,
              int=,
              colors=BLACK RED GREEN BLUE BROWN YELLOW ORANGE PURPLE);
    %*-- note: only 8 symbols & colors are defined;
    %*--    revise if more than 8 groups (recycle);
  %local chr col k;
  %do k=1 %to &n ;
     %let chr =%scan(&symbols, &k,' ');
     %let col =%scan(&colors, &k, ' ');
     %let lin =%scan(&l,      &k, ' ');
     symbol&k h=&h v=&chr  c=&col i=&int l=&lin;
  %end;
  %let k=%eval(&n+1);
  *  symbol&k i=std1jt v=none c=black ;
     symbol&k i=&int   v=none c=black ;
%mend gensym;
 
 /*-------------------------------------------------------------------*/
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
