
/*******************************************************************
 *  Making spin-plot varing the coefficient for variable x1 only   *
********************************************************************/
%macro sp_plotbx1(
    outfile="sp_plotbx1.gif", 
    data=_spinplt ,
    x1lo=-10, x1hi=10, x1by=2, 
    x2lo=-10, x2hi=10, x2by=2,
    cons=0, bx1=0, bx2=0, 
    bx1x2=0, bx1x1=0, bx2x2=0, 
    bx1x1x2=0, bx1x2x2=0, 
    title=%str(Regression surface, varying b1), 
    title2=,
    plot=, 
    gopt=, 
    angle=100, 
    slen=6,
    delay=50, 
    bx1_lo=-5, bx1_hi=5, bx1_by=1, cmd=plot); 

  * reset graph settings ;
  goptions reset=all;

 * filename out clear ;
  filename out &outfile ;

  /* assign graphics options for the animation */
 
  goptions gsfname=out dev=gifanim gcopies=0 iteration=0 delay=&delay &gopt;

  %let bx1_lo_ = %sysevalf(&bx1_lo*100);
  %let bx1_hi_ = %sysevalf(&bx1_hi*100);
  %let bx1_by_ = %sysevalf(&bx1_by*100);

  * make graph going forward from bx1lo to bx1hi ;
  %do bx1_ = &bx1_lo_ %to &bx1_hi_ %by &bx1_by_ ;
    %let bx1 = %sysevalf(&bx1_ / 100);
 
    %if &bx1_ =  &bx1_lo_ %then %do; goptions gsfmode=replace; %end;
    %if &bx1_ >  &bx1_lo_ %then %do; goptions gsfmode=append; %end;
    /* %if &bx1_ >= &bx1_hi_ %then %do; goptions gepilog='3B'x; %end; */

  %local bstring;
  %fixlen(&bx1, &slen);
  %let title =&title;

  %let title2=%str(Y = );
  %if %quote(&cons) ^=0 %then %let title2=&title2%str(&cons + );
  %let title2=&title2%str(&bx1)%str(*X1 + );
  %let title2=&title2%str(&bx2)%str(*X2 );
  %if %quote(&bx1x2) ^=0 %then %let title2=&title2%str(&bstring*X1*X2);
  %if %quote(&bx1x1) ^=0 %then %let title2=&title2%str(+&bx1x1*X1*X1);
  %if %quote(&bx2x2) ^=0 %then %let title2=&title2%str(+&bx2x2*X2*X2);
  %if %quote(&bx1x1x2) ^=0 %then %let title2=&title2%str(+&bx1x1x2*X1*X1*X2);
  %if %quote(&bx1x2x2) ^=0 %then %let title2=&title2%str(+&bx1x2x2*X1*X2*X2);
  %* let title2=%str(y=&cons)%str(&bstring)%str(*x1+&bx2*x2+&bx1x2*x1*x2+&bx1x1*x1x1+&bx2x2+x2x2&bx1x1x2*x1x1x2+&bx1x2x2*x1x2x2);


    %sp_plot( angle=&angle,data=&data ,
              x1lo=&x1lo, x1hi=&x1hi, x1by=&x1by, x2lo=&x2lo, x2hi=&x2hi, x2by=&x2by,
              cons=&cons, bx1=&bx1, bx2=&bx2, bx1x2=&bx1x2,bx1x1=&bx1x1, 
              bx2x2=&bx2x2, bx1x1x2=&bx1x1x2, bx1x2x2=&bx1x2x2, 
              title=&title, title2=&title2,plot=&plot,gopt=&gopt,cmd=&cmd, reset=no); 
  %end;

  * make graph going backward from bx1hi to bx1lo ;
  %do bx1_ = &bx1_hi_ %to &bx1_lo_ %by -&bx1_by_ ;
    %let bx1 = %sysevalf(&bx1_ / 100);

    %if &bx1_ >  &bx1_lo_ %then %do; goptions gsfmode=append; %end;  
    %if &bx1_ <= &bx1_lo_ %then %do; goptions gepilog='3B'x; %end;

  %local bstring;
  %fixlen(&bx1, &slen);
  %let title =&title;
  %let title2=%str(Y = );
  %if %quote(&cons) ^=0 %then %let title2=&title2%str(&cons + );
  %let title2=&title2%str(&bx1)%str(*X1 + );
  %let title2=&title2%str(&bx2)%str(*X2 );
  %if %quote(&bx1x2) ^=0 %then %let title2=&title2%str(&bstring*X1*X2);
  %if %quote(&bx1x1) ^=0 %then %let title2=&title2%str(+&bx1x1*X1*X1);
  %if %quote(&bx2x2) ^=0 %then %let title2=&title2%str(+&bx2x2*X2*X2);
  %if %quote(&bx1x1x2) ^=0 %then %let title2=&title2%str(+&bx1x1x2*X1*X1*X2);
  %if %quote(&bx1x2x2) ^=0 %then %let title2=&title2%str(+&bx1x2x2*X1*X2*X2);
  %* let title2=%str(y=&cons)%str(&bstring)%str(*x1+&bx2*x2+&bx1x2*x1*x2+&bx1x1*x1x1+&bx2x2+x2x2&bx1x1x2*x1x1x2+&bx1x2x2*x1x2x2);

  %sp_plot( angle=&angle, data=&data ,
            x1lo=&x1lo, x1hi=&x1hi, x1by=&x1by, x2lo=&x2lo, x2hi=&x2hi, x2by=&x2by,
            cons=&cons, bx1=&bx1, bx2=&bx2, bx1x2=&bx1x2, bx1x1=&bx1x1, 
            bx2x2=&bx2x2, bx1x1x2=&bx1x1x2, bx1x2x2=&bx1x2x2, 
            title=&title, title2=&title2, plot=&plot, gopt=&gopt,cmd=&cmd, reset=no); 
  %end;

  filename out clear ;
  goptions reset=all;
%mend sp_plotbx1;

/*****************************************************
 *      Pad or trim a tring to a fixed length        *
 * It is used to ajust the title width in display so *
 * that in the animated gif file the length of the   *
 * title will remain the same.                       *
 *****************************************************/
%macro fixlen(in_num, len);

options nonotes;
    data _null_;
      in_n = input(&in_num, 16.);
      call symput('sgn', sign(in_n));
    run;
 %if %sysevalf(%sysevalf(&in_num) = 0) %then 
     %let bstring = %substr(%str(+0.0000), 1, %sysevalf(&len));
 %else %do;
    %let tstr=%str(.);
    %if %sysevalf(%index(%str(&in_num), &tstr) ne 0) %then %do;
        %if %sysevalf(%eval(&sgn = 1)) %then 
         %let bstring = %substr(%str(+)%str(&in_num)%str(00000), 1, %sysevalf(&len));
        %else %let bstring = %substr(%str(&in_num)%str(00000), 1, %sysevalf(&len));
    %end;
    %else %do;
      %if %sysevalf(%eval(&sgn = 1)) %then 
          %let bstring = %substr(%str(+)%str(&in_num)%str(.0000), 1, %sysevalf(&len));
      %else   %let bstring = %substr(%str(&in_num)%str(.0000), 1, %sysevalf(&len));
    %end;
 %end;
 options notes;
%mend;
