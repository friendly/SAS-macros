          /*
 **********************************************************************;
 **
 ** Macro:  Jitplot
 **
 ** Function:  Produce Gplot of continuous var(y-axis) vs a group var(x-
 **            axis) in such a way that no points are hidden.  Two types
 **            of plots may be requested as shown below:
 **
 **
 **
 **                      Left Justifed              Centered
 **                |      .       .            |    .       .
 **                |      ..      .            |   . .      .
 **            y   |      ...     ....      y  |   ...    .. ..
 **                |      .       ..           |    .      . .
 **                |      .       .            |    .       .
 **                |                           |
 **                |------|-------|-------     |----|-------|-----
 **                     level 1  level 2         level 1   level 2
 **                         Group                    Group
 **
 **            This type of plot is most effective when N is not large and
 **            you wish to display all of the data points.
 **
 ** Programmer:  E.  Bergstralh
 **
 ** Date:  February 9, 1993
 **        Jan 9, 1998     *added output dataset(jlk)
 **        April 20, 1995  *created _wk dataset & sorted it
 **                        *deleted group levels not specifed
 **                        *deleted group levels with missing values
 **                        *specify mn_md=m, x or b for median, mean or both
 **
 ** Call statement:
 **
 **              %jitplot(data=, y=, group=, x00=, x10=, x20=,
 **                       x30=, x40=, x50=, x60=, x70=, x80=, x90=,
 **                       justify=l, space=1, sym=dot, ht=.1cm,
 **                       yaxis=, glabel=, mn_md=n, outdata=);
 **
 ** Parameter definitions:
 **
 **       Note:  If the macro variable values contain special characters,
 **              e.g. ";)(,=" , you must use the %str(value) function to avoid
 **              processing errors.
 **
 **       Required parameters:
 **
 **           data=name of input sas data set
 **
 **              y=variable name for y-var
 **
 **          group=variable name for group-var(x).  Macro assumes the group
 **                var has a maximum of 9 levels.
 **
 **         x00,
 **         x10,...,
 **         x90=group-var levels to be plotted at positions 00-90 on
 **                the x-axis, which is defined as numeric running from 0-100.
 **                Do not use position x00 with a centered plot(see below).
 **                You need define only the positions that will be needed
 **                for your plot.  For example, if the group-var was named
 **                "grade" with 3-levels such as "normal,mild", "mod" and
 **                "severe" then specifying:
 **
 **                    x20=%str(normal,mild)
 **                    x50=mod,
 **                    x80=severe,
 **
 **                with group=grade would yield a well balanced plot.
 **
 **        Optional:
 **
 **        justify= l for left-justifed plot(the default), c for centered
 **
 **          space=horizontal distance between points with the same y-value.
 **                The default value is 1, which allows up to 10 tied points
 **                to be displayed if groups were plotted at x=10,20, etc.
 **                Values from 0.5 to 2.0 seem to produce reasonable results.
 **
 **            sym=plotting symbol to use in the SYMBOL statement. Choices
 **                are listed in Table 16.1 of the SAS/Graph(V6) manual
 **                (Vol. 1, p.421).  Default is the dot symbol.
 **
 **             ht=plot symbol height to use in the SYMBOL statement.  Choices
 **                are listed on page 410 of the SAS/Graph(V6) manual. Default
 **                value is .1cm.
 **
 **          yaxis=vaxis, vref and vminor options from PROC GPLOT.  These
 **                options will almost always have to be specified using the
 **                %str(value) function.  For example:
 **
 **                   yaxis=%str(vaxis=10 to 20 by 1 vref=15 vminor=1)  .
 **
 **                Do NOT change the haxis settings.
 **
 **         glabel=label for group-var.  Note that any existing label for the
 **                group-var is not available to jitplot.
 **
 **          mn_md=M if you want the median y-value noted on the plot.  Use
 **                mn_md=X if you want the mean y-value and mn_md=B if you
 **                want both the mean and the median.  Default is not to
 **                print either statistic.
 **
 **          outdata= name of output dataset containing jittered values
 **************************************************************************;
              */

 %macro   jitplot(data=, y=, group=, x00=, x10=, x20=, x30=,
                  x40=, x50=, x60=, x70=, x80=, x90=,
                  justify=l, space=1, sym=dot, ht=.1cm, yaxis=, glabel=,
                  mn_md=n,outdata=);

data _wk; set &data;
 keep &group &y;
proc freq data=_wk; tables &group*&y/noprint out=_p1;
proc sort data=_wk; by &group;
proc univariate data=_wk noprint; by &group; var &y;
  output out=_mnmd   mean=mn median=md;

data _p2; set _p1 _mnmd; by &group;
  **convert group var levels to numeric 00=>90;
 if &group="" then delete; **delete "missing" group levels;
 if &group="&x00"  then gp= 0;
 if &group="&x10"  then gp=10;
 if &group="&x20"  then gp=20;
 if &group="&x30"  then gp=30;
 if &group="&x40"  then gp=40;
 if &group="&x50"  then gp=50;
 if &group="&x60"  then gp=60;
 if &group="&x70"  then gp=70;
 if &group="&x80"  then gp=80;
 if &group="&x90"  then gp=90;
 if gp=. then delete;      **delete unspecifed group levels;
 *** figure out xvariables;
 gp2=gp; **x-var for left justifed plot;
 gp3=gp; **x-var for centered plot;
 med=(count+1)/2;
 if count ne . then do;
  do i=1 to count;
   gp2=gp+( (i-1)*&space);
   gp3=(gp2+1)-med*&space;
   output;
  end;
 end;
 else do;
  gp2=gp-2*&space; gp3=gp-4*&space; output;
 end;
 %if &outdata^= %then %do;
    data &outdata; set _p2;
 %end;
proc format; value gpf 0="&x00 "  10="&x10 "   20="&x20 "   30="&x30 "
                      40="&x40 "  50="&x50 "   60="&x60 "   70="&x70 "
                      80="&x80 "  90="&x90 "   other=" ";

 symbol v=&sym h=&ht i=none;
 symbol2 v=x    i=none;  **mean;
 symbol3 v=plus i=none; **median;

  %if %upcase(&justify)= C %then %do;  **centered plot;
proc gplot gout=jit;
  plot &y*gp3=1

    %if %upcase(&mn_md)= X or %upcase(&mn_md)= B %then %do;
       mn*gp3=2
    %end;
    %if %upcase(&mn_md)= M or %upcase(&mn_md)= B %then %do;
       md*gp3=3
    %end;
  %end;

  %if %upcase(&justify)= L %then %do;  **left justified plot;
proc gplot gout=jit;
  plot &y*gp2=1

    %if %upcase(&mn_md)= X or %upcase(&mn_md)= B %then %do;
       mn*gp2=2
    %end;
    %if %upcase(&mn_md)= M or %upcase(&mn_md)= B %then %do;
       md*gp2=3
    %end;
  %end;

   / overlay haxis=0 to 100 by 10 hminor=0 &yaxis ;

    %if %upcase(&mn_md)= M %then %do;
     footnote .j=l .h=1 'Note: + = median';
    %end;
    %if %upcase(&mn_md)= X %then %do;
     footnote .j=l .h=1 'Note: X = mean ';
    %end;
    %if %upcase(&mn_md)= B %then %do;
     footnote .j=l .h=1 'Note: X = mean,  + = median';
    %end;

 %if &glabel ^= %then %Do;
  label gp2="&glabel" gp3="&glabel";
 %end;
 %else %do;
  label gp2="&group" gp3="&group";
 %end;

 format gp2 gp3 gpf.;
 run; quit;
 goptions reset=symbol;
 options _last_=&data ;
 proc datasets nolist; delete _wk _p1 _p2 _mnmd;
 footnote; run; quit;
 %mend jitplot;
 /*
data one;
 y=2; x='';x2=.; output; output;output;
 y=4;x='b';x2=2; output; output; output;
 y=1; x='a';x2=1; output; output; output;
proc print;
proc freq; tables x/out=ch ;
proc contents data=ch;
proc print;
 %jitplot(data=one,y=y,x40=a,x60=b,group=x,mn_md=x,outdata=xxx)
run;
 */
