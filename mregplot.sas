
/*************************************************************
title:  Plot a multiple regression response surface

  The parameters, x1's, x2's and b's are for the purpose of generating data. 
  The data can be saved to a data file using the data= option. 

  The title and title2 option is     
  used for the diplay purpose and can be specified.           

  The last three options, plot=, gopt=, and cmd= are used to  
  modified the detailed options on the size, color, or the    
  the style of the plot.                                      
*************************************************************/
%macro mregplot(
    angle=0,
    x1lo=-10, x1hi=10, x1by=2,
    x2lo=-10, x2hi=10, x2by=2,
    cons=0, bx1=0, bx2=0, 
    bx1x2=0, bx1x1=0, bx2x2=0, 
    bx1x1x2=0, bx1x2x2=0,
    data=_spinplt, 
    title= Multiple regression response surface,
    title2 = y=&cons+&bx1*x1+&bx2*x2+&bx1x2*x1*x2+&bx1x1*x1x1+&bx2x2*x2x2+
         &bx1x1x2*x1x2x2+&bx1x2x2*x1x2x2,
    colors = black blue red,
    slen=5,
	function = y,
	ylabel = Y,
    plot=, gopt=, cmd=plot, reset=no,
	name=mreg
	); 

    %let x1lo_ = %sysevalf(&x1lo*100);
    %let x1hi_ = %sysevalf(&x1hi*100);
    %let x1by_ = %sysevalf(&x1by*100);
    %let x2lo_ = %sysevalf(&x2lo*100);
    %let x2hi_ = %sysevalf(&x2hi*100);
    %let x2by_ = %sysevalf(&x2by*100);

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

  * make data from x1lo to x1hi by x1by, from x2lo to x2hi by x2by ;
  * since these may not be integers, multiply them by 100 ; 
  data &data;
      do x1_ = &x1lo_ to &x1hi_ by &x1by_ ;
      do x2_ = &x2lo_ to &x2hi_ by &x2by_ ;
        x1 = x1_ / 100;         
        x2 = x2_ / 100;
        y = &cons + &bx1*x1 + &bx2*x2 + &bx1x2*x1*x2 + &bx1x1*x1*x1 
                + &bx2x2*x2*x2 + &bx1x1x2*x1*x1*x2 + &bx1x2x2*x1*x2*x2 ;
		y = &function;
        output ;
      end;
    end;
  run;

  goptions gunit=pct  colors=(&colors)  
      htitle=5 htext=4 &gopt ;
 
  * make the plot below , change plot options with plot macro ;
  * can change cmd to "scatter" for scatterplots ;
  proc g3d data=&data ;
    &cmd x2*x1=y / rotate=&ANGLE 
                   xticknum=3 yticknum=3 zticknum=3 zmin=-50 zmax=50 &plot
				   name="&name";
      label x1 = 'X1'
            x2 = 'X2'
             y = "&ylabel" ;
      title h=5 "&title";
      title2 h=5 "&title2";

   run;
   quit;

   %if &reset ~= no %then goptions reset=all;

%mend;
