/*-------------------------------------------------------------*
*     Name: corrdemo.sas & corrdema.sas                        *
*     Title: Demo on Correlation Coefficient & Animated Gif    *
*     %corrdemo(corr=, size=);                                 *
*--------------------------------------------------------------*/

%macro corrdemo(corr=.7, size=100, 
	outfile=' ', data=_corrdem , dev=gif373,
    title="Correlation Coefficient = &corr",
	axis1=, axis2=, symbol=, plot=); 

  data &data;
     do subject=1 to &size;
     y=rannor(0);
	 x=&corr*y+sqrt(1-(&corr)**2)*rannor(0);
    output;
    end;
  run;

  /*goptions reset=all; */
  %if &outfile ne ' ' %then %do;
    goptions dev=&dev;
    goptions gsfname=out;
    filename out &outfile;
  %end;

  axis1 label= (r=0 a=90) order=( -4 to 4 by 2) minor=none &axis1;
  axis2 order= (-4 to 4 by 2) minor=none &axis2;
  symbol v=circle i=none h=0.5 c=yellow &symbol;
  title &title;
  proc gplot;
    plot y*x=1 / haxis=axis2 vaxis=axis1 cframe=lib &plot;
    label x='Variable X';
    label y ='Variable Y';
  run;
  quit;
  title;
*  goptions reset = all;
%mend corrdemo;

