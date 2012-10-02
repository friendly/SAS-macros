/*
  Animated Graphs of Correlation Coefficient

Description
  This macro creates animated gif files of scatter plots with different 
  correlation coefficients. For example, without any argument, simply 
  call the macro will create a gif file called corrdemoa.gif in 
  C:\temp directory. The correlation coefficients change from -1 to +1 
  by 0.1. The delay is set to be 25ms that controls how fast the gif 
  file changes each of its frame.  

Syntax

  %corrdemoa(out= ,lo= ,hi= ,inc= ,size= ,
            gopt= ,axis1= , axis2= , symbol= , plot= ,title= )

    out     - name of the gif file to be created, 
              the default is c:\temp\corrdemoa.gif
    lo      - the starting value for the correlation coefficient
    hi      - the ending value for the correlation coefficient
    inc     - the amount the correlation should increment by in
              each frame of the animation.
    size    - the size of the gif to be created
    gopt    - options for the "goptions" statement
    axis1   - options for the "axis1" statement (the x axis)
    axis2   - options for the "axis2" statement (the y axis)
    symbol  - options for the "symbol" statement
    plot    - options for the "plot" statement 
    title   - the title displayed at the top of the graph

Examples

  %corrdemoa

  %corrdemoa(out="c:\temp\corrdema.gif")

  %corrdemoa(out="c:\temp\corrdema.gif",
            title="Correlation Coefficient = &costr");

Author
  
  Xiao Chen
  Statistical Computing and Consulting
  UCLA Academic Technology Services
  Animated Graphs of Correlation Coefficient

Description
  This macro creates animated gif files of scatter plots with different 
  correlation coefficients. For example, without any argument, simply 
  call the macro will create a gif file called corrdemoa.gif in 
  C:\temp directory. The correlation coefficients change from -1 to +1 
  by 0.1. The delay is set to be 25ms that controls how fast the gif 
  file changes each of its frame.  

Syntax

  %corrdemoa(out= ,lo= ,hi= ,inc= ,size= ,
            gopt= ,axis1= , axis2= , symbol= , plot= ,title= )

    out     - name of the gif file to be created, 
              the default is c:\temp\corrdemoa.gif
    lo      - the starting value for the correlation coefficient
    hi      - the ending value for the correlation coefficient
    inc     - the amount the correlation should increment by in
              each frame of the animation.
    size    - the size of the gif to be created
    gopt    - options for the "goptions" statement
    axis1   - options for the "axis1" statement (the x axis)
    axis2   - options for the "axis2" statement (the y axis)
    symbol  - options for the "symbol" statement
    plot    - options for the "plot" statement 
    title   - the title displayed at the top of the graph

Examples

  %corrdemoa

  %corrdemoa(out="c:\temp\corrdema.gif")

  %corrdemoa(out="c:\temp\corrdema.gif",
            title="Correlation Coefficient = &costr");

Author
  
  Xiao Chen
  Statistical Computing and Consulting
  UCLA Academic Technology Services

*/

%macro corrdemoa(
	size=500,
	out="corrdemoa.gif",
	lo=-1,hi=1,inc=.1,
	gopt=,
    axis1=, axis2=, 
	symbol=, plot=,
    title="Correlation Coefficient = &costr");

  filename outani clear ;
  filename outani &out;

  goptions reset=all;

  /* set the graphics environment */
  goptions gunit=pct 
     cback=liy ctext=black vsize=3in hsize=3in
     colors=(blue green red) ftext=swiss
     ftitle = swiss htitle=5 htext=4;

  /* assign graphics options for the animation */
  goptions dev=gifanim gsfname=outani gcopies=0 iteration=0 delay=25;

  goptions &gopt;

  %let lo2 = %sysevalf(&lo*100) ;
  %let hi2 = %sysevalf(&hi*100) ;
  %let inc2 =%sysevalf(&inc*100) ;
  
  %do corr = &lo2 %to &hi2 %by &inc2;
  
    %if &corr = &lo2 %then %do; goptions gsfmode=replace; %end;
    %if &corr > &lo2 %then %do; goptions gsfmode=append; %end;
    %if &corr >= &hi2 %then %do; goptions gepilog='3B'x; %end;

    %let corr2 = %sysevalf(&corr/100.0) ;
	/* set correlation coefficients to be the same width*/
    %if &corr = 0 %then %let costr=%str(+0.00) ;
	%else %if &corr = -100 %then %let costr=%str(-1.00);
	%else %if &corr = 100  %then %let costr=%str(+1.00);
	%else %if &corr < 0 %then  %let costr=%substr(%str(%sysevalf(&corr2))%str(00), 1, 5);
	%else %let costr=%substr(%str(+)%str(%sysevalf(&corr2))%str(00), 1, 5);

	%corrdemo(corr=&corr2,size=&size,axis1=&axis1, axis2=&axis2, 
              symbol=&symbol, plot=&plot,title=&title);
  %end;

  filename out clear ;
  goptions reset=all;
%mend corrdemoa;


