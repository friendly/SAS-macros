
/***************************************************
*   Making a spin-plot simply rotating itself      *
*  The arguments that the macro takes includes     *
* all the arguments that the macro sp_plot takes   *
* and also includes the arguments on angles.       *
* The option slen is used for setting up the width *
* of the string of the parameters varing through   *
* the process to keep the length of the title      *
* stay unchanged.                                  *
***************************************************/
%macro sp_plota(
    outfile="sp_plota.gif", 
    data=_spinplt ,
    x1lo=-10, x1hi=10, x1by=2, 
    x2lo=-10, x2hi=10, x2by=2,
    cons=0, bx1=0, bx2=0, 
    bx1x2=0, bx1x1=0, bx2x2=0, bx1x1x2=0, bx1x2x2=0,
    title=Regression surface, 
    title2=,
    slen=5,
	delay=50,
    plot=, gopt=, 
    ang_lo=0, ang_hi=360, ang_by=15, 
    cmd=plot );    

  * reset graph settings;
  goptions reset=all;

  * filename out clear ;
  filename out &outfile;
  
  /* assign graphics options for the animation */
  goptions gsfname=out dev=gifanim gcopies=0 iteration=0 delay=&delay &gopt;

   %local bstring;
  %fixlen(&bx1x2, &slen);
  %let title2=%str(Y = );
  %if %quote(&cons) ^=0 %then %let title2=&title2%str(&cons + );
  %let title2=&title2%str(&bx1)%str(*X1 + );
  %let title2=&title2%str(&bx2)%str(*X2 );
  %if %quote(&bx1x2) ^=0 %then %let title2=&title2%str(&bstring*X1*X2);
  %if %quote(&bx1x1) ^=0 %then %let title2=&title2%str(+&bx1x1*X1*X1);
  %if %quote(&bx2x2) ^=0 %then %let title2=&title2%str(+&bx2x2*X2*X2);
  %if %quote(&bx1x1x2) ^=0 %then %let title2=&title2%str(+&bx1x1x2*X1*X1*X2);
  %if %quote(&bx1x2x2) ^=0 %then %let title2=&title2%str(+&bx1x2x2*X1*X2*X2);
  %*let title2=%str(y=&cons+)%str(&bx1)%str(*x1+&bx2*x2&bstring*x1*x2+&bx1x1*x1x1+&bx2x2*x2x2+&bx1x1x2*x1x1x2+&bx1x2x2*x1x2x2);

  %do angle = &ang_lo %to &ang_hi %by &ang_by ;
  
    %if &angle = &ang_lo  %then %do; goptions gsfmode=replace; %end;
    %if &angle > &ang_lo  %then %do; goptions gsfmode=append; %end;
    %if &angle >= &ang_hi %then %do; goptions gepilog='3B'x; %end;
   
    %let title =&title;
    %let title2=&title2;
    
    
    %sp_plot( angle=&angle, data=&data ,
              x1lo=&x1lo, x1hi=&x1hi, x1by=&x1by, x2lo=&x2lo, x2hi=&x2hi, x2by=&x2by,
              cons=&cons, bx1=&bx1, bx2=&bx2, bx1x2=&bx1x2,bx1x1=&bx1x1, 
              bx2x2=&bx2x2, bx1x1x2=&bx1x1x2, bx1x2x2=&bx1x2x2, cmd=&cmd,
              title=&title, title2=&title2, plot=&plot, gopt=&gopt,reset=no); 
  %end;

  filename out clear ;
  goptions reset=all;

%mend sp_plota;
