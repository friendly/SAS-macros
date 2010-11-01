
/*************************************************************
title:  Animated spin plot for multiple regression surface

  The parameters, x1's, x2's and b's are for the purpose of generating data. 
  The data can be saved to a data file using the data= option. 

  The graphic device can be also be specified. For example, the default 
  used here is a gif with size 373x280 pixels. You can change it by using the    
  dev= option. 

  The outfile option allows you to save the gif  
  file to a specific file. 

  The title and title2 option is     
  used for the diplay purpose and can be specified.           

  The last three options, plot=, gopt=, and cmd= are used to  
  modified the detailed options on the size, color, or the    
  the style of the plot.
 
  Added ability to transform Y by some function, e.g.,
      function = 1 / (1 + exp(-y))
  gives logistic regression surfaces
                                        
*************************************************************/
%macro sp_plot(
    angle=0,
    x1lo=-10, x1hi=10, x1by=2,
    x2lo=-10, x2hi=10, x2by=2,
    cons=0, bx1=0, bx2=0, 
    bx1x2=0, bx1x1=0, bx2x2=0, 
    bx1x1x2=0, bx1x2x2=0,
    outfile=' ', 
    data=_spinplt, 
    dev=gif373,
    title= 3-D Plot,
    title2 = y=&cons+&bx1*x1+&bx2*x2+&bx1x2*x1*x2+&bx1x1*x1x1+&bx2x2*x2x2+
         &bx1x1x2*x1x2x2+&bx1x2x2*x1x2x2,
	function = y,
    plot=, gopt=, cmd=plot, reset=yes); 

    %let x1lo_ = %sysevalf(&x1lo*100);
    %let x1hi_ = %sysevalf(&x1hi*100);
    %let x1by_ = %sysevalf(&x1by*100);
    %let x2lo_ = %sysevalf(&x2lo*100);
    %let x2hi_ = %sysevalf(&x2hi*100);
    %let x2by_ = %sysevalf(&x2by*100);

  * make data from x1lo to x1hi by x1by, from x2lo to x2hi by x2by ;
  * since these may not be integers, multiply them by 100 ;
  options nonotes;
  data &data;
      do x1_ = &x1lo_ to &x1hi_ by &x1by_ ;
      do x2_ = &x2lo_ to &x2hi_ by &x2by_ ;
        x1 = x1_ / 100;         
        x2 = x2_ / 100;
        y = &cons + &bx1*x1 + &bx2*x2 + &bx1x2*x1*x2 + &bx1x1*x1*x1 
                + &bx2x2*x2*x2 + &bx1x1x2*x1*x1*x2 + &bx1x2x2*x1*x2*x2 ;
		y = &function ;
        output ;
      end;
    end;
  run;
  options notes;

  * if making a file, set options below ;
  %if &outfile ne ' ' %then 
     %do;
     filename out &outfile;
     goptions dev=&dev gsfname=out;
  %end;

  goptions gunit=pct cback=white ctext=black vsize=2.5in hsize=3.5in
             colors=(yellow blue red) ftext=swissb ftitle=centbu 
             htitle=5 htext=4 &gopt ;
 
  * make the plot below , change plot options with plot macro ;
  * can change cmd to "scatter" for scatterplots ;
  proc g3d data=&data ;
    &cmd x2*x1=y / rotate=&ANGLE 
                   xticknum=3 yticknum=3 zticknum=3 zmin=-50 zmax=50 &plot;
      label x1 = 'X1'
            x2 = 'X2'
            y = 'Y' ;
      title h=5 c=purple f=swissb "&title";
      title2 h=5 c=black f=simplex "&title2";

   run;
   quit;

   %if &reset ~= no %then goptions reset=all;

%mend sp_plot;
