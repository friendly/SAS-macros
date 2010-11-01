/*****************************************************************/
/* 	The COLORSCALE macro can be used to determine a list of      */
/* 	colors in a gradient.  The TOP and BOTTOM colors are         */
/* 	required; a middle color is optional.  The value N sets the  */
/* 	desired number of intermediate colors.  For example,  if N   */
/* 	is 10 and no middle color is specified, 12 colors are shown  */
/* 	in the output.  If a middle color is specified, 13 colors    */
/* 	would be shown in the output.                                */
/*                                                               */
/*	The macro takes the following parameters:                    */
/*                                                               */
/*     TOP: color displayed on top of the output                 */
/*  MIDDLE: optional middle color; the gradient is               */
/*          forced through this color                            */
/*  BOTTOM: color displayed on the bottom of the output          */
/*       N: the number of intermediate colors                    */
/*     DSN: name of the dataset that stores the colors.          */
/*          The variable RGB contains the color values,          */
/*          the variable NUMCOL contains the number              */
/*          of colors.                                           */
/*  SWATCH: if "Y", display a sample of the colors.              */
/*                                                               */
/*  Colors should be represented as RGB hex values, such as      */
/*  FFFFFF for white or 000000 for black.  See Technical         */
/*	Support document TS-688 for more information.                */
/*                                                               */
/*	This macro uses the INCR macro, below, to calculate the      */
/*	intermediate color values.                                   */
/*                                                               */
/*	Because values must be rounded, slightly different results   */
/*	may occur if the values for the top and bottom colors are    */
/*	reversed.  If the last intermediate color seems to 'jump'    */
/*	from the top or bottom color, try reversing the values for   */
/*	the top and bottom colors.                                   */
/*                                                               */
/*	When invoking the macro, remember that the parameters are    */
/*	positional. If no middle color is specified, the comma       */
/*	should remain:  %colorscale(000000,,FFFFFF,3,anno);          */
/*                                                               */
/*                                              Revised 20SEP02  */
/*****************************************************************/

%macro colorscale(top,middle,bottom,n,dsn,swatch);

/* If there is a middle color,                                   */
%if "&middle" NE "" %then %do;
   /* set the number of intermediate colors between each section    */ 
   /* (top and middle or bottom and middle). If N is even, N/2      */
   /* intermediate colors are calculated; if N is odd, (N-1)/2      */
   /* intermidiate colors are calculated. The customer is warned    */
   /* of this change below.                                         */  
	   data _null_;
		   if mod(&n,2)=0 then call symput('halfn',left(put(&n/2,5.)));
		   else call symput('halfn',left(put((&n-1)/2,5.)));
	   run;

   /* Calculate the top to middle and middle to bottom             */
   /* ranges, and combine these into the T2B data set.             */
	 %incr(&top,&middle,t2m,&halfn,N);
	 %incr(&middle,&bottom,m2b,&halfn,Y);
	   data t2b;
		 set t2m m2b;
	   run;
   /* Determine the size of the bars for the annotation,          */
   /* and warn the user if N-1 values were calculated instead of  */
   /* N values.                                                   */
	   data _null_;
	   if mod(&n,2) EQ 0 then call symput('draw',left(put(&n+3,6.)));
	   else do;
  	   call symput('draw',left(put(&n+2,6.)));
		   put "Warning:  You have entered an odd value: &n..";
       put "          %eval(&n-1) colors were created instead.";
	   end;
   run;
   %end;

/* If there is no middle color,                                */
%else %do;
   /* Calculate the N intermediate colors between TOP and BOTTOM, */
	 %incr(&top,&bottom,t2b,&n,y);
   /* and determine the size of the bars for the annotation.      */
	   data _null_;
  	   call symput('draw',left(put(&n+2,6.)));
	   run;
   %end;

%if %upcase("&swatch")="Y" or %upcase("&swatch")="YES" %then %do;
   /* The T2B dataset now contains the original and intermediate  */
   /* colors, in order of creation.  This annotation will display */
   /* both a color swatch and the RGB hex value for each color.   */
   data swatch;
	  length color function $8. style $10.; 
	  retain xsys ysys '1' when 'b' ;
	  set t2b;
	  if _n_=1 then y=98;
	  function='move'; x=0;output;
	  function='bar '; x=60;y+-floor(100/(&draw));
	  	style='solid';color="cx"||rgb;output;
	  function='label';x=62;position='3';
	  	text=rgb;size=1;color='black';style='SWISS';output;
	  run;

   /* Create an image with the annotation.  To export this image, */
   /* use a GOPTIONS and FILENAME statement before invoking       */
   /* the macro.                                                  */ 
   goptions cback=white;
   proc gslide anno=swatch;
   title1 j=r "Sample" j=r "Colors";
   run;quit;
   title1;
   %end;

data &dsn;
   set t2b;
   colnum+1;
   retain numcol;
   numcol=&draw;
   *keep rgb numcol;
   run;

%mend;



%macro incr(start,end,dsn,div,lc);
data &dsn;
   length start end sred ered sblue eblue sgreen egreen $6.;
   drop start end sred ered sblue eblue sgreen egreen;
   /* Get the colors.                                               */
   start=upcase("&start");
   end=upcase("&end");
   /* Find the starting and ending values for Red, Green, and Blue. */
	sred=substr(start,1,2);
	ered=substr(end,1,2);
	sgreen=substr(start,3,2);
	egreen=substr(end,3,2);
	sblue=substr(start,5,2);
	eblue=substr(end,5,2);
   /* Calculate the increment for Red, Green, and Blue.            */
   incrn=(input(ered,hex2.)-input(sred,hex2.))/(&div+1);
   incgn=(input(egreen,hex2.)-input(sgreen,hex2.))/(&div+1);
   incbn=(input(eblue,hex2.)-input(sblue,hex2.))/(&div+1);

   /* Determine the direction of the value, either increasing      */
   /* increasing or decreasing.                                    */
   if incrn<0 then multr=-1;else multr=1;
   incr=left(put(incrn*multr,hex2.));
   if incgn<0 then multg=-1;else multg=1;
   incg=left(put(incgn*multg,hex2.));
   if incbn<0 then multb=-1;else multb=1;
   incb=left(put(incbn*multb,hex2.));

   /* Add the start color to the output.                          */
   rgb=start;
   output;
   /* Calculate the incremental values for red, green, and blue   */
   /* and combine them into a single value, RGB.                  */
   do i=1 to &div;
	  red=input(sred,hex2.)+input(incr,hex2.)*i*multr;
	  green=input(sgreen,hex2.)+input(incg,hex2.)*i*multg;
	  blue=input(sblue,hex2.)+input(incb,hex2.)*i*multb;
		rgb=put(red,hex2.)||put(green,hex2.)||put(blue,hex2.);
	  output;
	  end;

   /* Add the last color to the output.  This step is             */
   /* not performed for the T2M dataset; otherwise T2M and        */
   /* M2B would each write out the color, creating two bands      */
   /* of the middle color in the final output.                    */
   if upcase("&lc")="Y" then do;
	   rgb=end;
	   output;
   end;
   run;
%mend;

