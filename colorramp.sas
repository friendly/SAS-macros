 /*--------------------------------------------------------------*
  *    Name: colorramp.sas                                       *
  *   Title: Construct a sequential or diverging color set       *
        Doc: http://www.datavis.ca/sasmac/colorramp.html   
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 05 Dec 2005 17:51:52                                *
  * Revised: 07 Dec 2005 09:09:07                                *
  * Version: 1.0                                                 *
  *  1.0 Re-written from a colorscale macro (author unknown, SI) *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The COLORRAMP macro constructs a set of RGB colors ranging from a
 starting color to an ending color, optionally going through a middle
 color.  The result appears as the COLORVAR= variable in an OUT=
 output data set, and optionally as a macro variable named by the
 RESULT= parameter.

 The ends and middle of the color scale can be specified either as
 6-character RRGGBB hex strings, as 8-character HLS strings (Hhhllss),
 or as the predefined SAS/Graph color names, e.g., 'very light purple'.
 Except for RRGGBB, this requires the SAS-supplied COLORMAC macro,
 available with SAS V8+.

==Method:

 START=, END= and MIDDLE= are each converted to decimal red, green,
 blue components, and linear interpolation is performed on each,
 from START= to END=, possibly through MIDDLE=.  This means that
 if you prefer to use HLS colors, the result will not be exactly
 linear in HLS space, though the difference will probably be small
 unless very few colors are used.
 
 In general, other color spaces (CIE Lab, HSV and HCL) are more
 perceptually uniform, and give smoother color ramps, but these are
 more difficult to implement computationally.
 
=Usage:

 The COLORRAMP macro is defined with keyword parameters.  The N=
 parameter is required.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%colorramp(n=10);
    %colorramp(start=red,mid=very light gray,end=0000FF, n=8, display=Y);
	%colorramp(start=red, end=blue, n=6, result=mycolors);
	%put Colors: &mycolors;

	%colorramp(start=H00080FF, end=H07880FF, n=10, out=mycolors);
	%genpat(n=nobs, data=mycolors);
	 
==Parameters:

* N=          Number of colors to be created

* START=      Starting color.  This should be either a 6-digit hex string
              (rrggbb), or an 8-char HLS string (Hhhhllss),
              or the name of a SAS/Graph color. [Default: START=FF0000]

* MID=        Middle color.  If specified, the resulting colors go from
              the START= color to the MIDDLE= color, then to the END=
			  color.  When N= is an even number, you can get a symmetric
			  set of colors by specifying DUPMID=1;  otherwise, there is
			  one fewer color in the set from START= to MID= than from
			  MID= to END=.

* END=        Ending color [Default: END=0000FF]

* DUPMID=     0 or 1: Duplicate middle color when N=even? [Default: DUPMID=0]

* COLORVAR=   Name of color the variable in the OUT= data set.
              This is a SAS/Graph RGB color of the form CXrrggbb. 
			  [Default: COLORVAR=COLOR]

* ORDER=      You can specify ORDER=REV to reverse the order of the colors in the OUT=
              data set and in the RESULT= macro variable.

* OUT=        The name of the output data set [Default: OUT=COLORS]

* DISPLAY=    Show a display of the colors? [Default: DISPLAY=N]
                
* RESULT=     Name of output macro variable containing color list.  If specified,
              a macro variable of that name is created with a list of all distinct
			  colors in the OUT= data set.
                

 =*/

%macro colorramp(
   n=,               /* Number of colors                            */
   start=FF0000,     /* Starting color                              */
   mid=,             /* Middle color                                */
   end=0000FF,       /* End color                                   */
   dupmid=0,         /* Duplicate middle color when n=even?         */
   colorvar=color,   /* Name of color variable                      */
   out=colors,       /* Name of output data set                     */
   order=,           /* How to sort the colors?                     */
   display=N,        /* Show a display of the colors?               */
   result=           /* output macro variable containing color list */
   );

%if %length(&n)=0 %then %do;
	%put ERROR: You must specify N= number of colors;
	%goto DONE;
	%end;

*-- load the colormac macros if any non-hex color;
%if %verify(&start.&mid.&end,0123456789ABCDEF) %then %do;
	%if %sysevalf(&sysver  < 8) %then %do;
		%put WARNING: You need the SAS supplied COLORMAC macro to use named or HLS colors;
		%end;
	%colormac(NOMSG);
	%end;

%let start=%makergb(&start);
*let mid=%upcase(&mid);
%let end=%makergb(&end);

/*
%if %verify(&start,0123456789ABCDEF) %then %do;
	%put NOTE: Apparent SAS Color name: &start;
	%let start = %hls2rgb(%cns(&start));
	%put NOTE: Converted to RGB: &start;
	%end;
%if %verify(&end,0123456789ABCDEF) %then %do;
	%put NOTE: Apparent SAS Color name: &end;
	%let end = %hls2rgb(%cns(&end));
	%put NOTE: Converted to RGB: &end;
	%end;
*/

/* If there is a middle color,                                   */
%if %length(&mid) %then %do;
	%let mid=%makergb(&mid);

	data _null_;
		n=&n;
		even = mod(n,2)=0;    /* is it even? */
		dup = &dupmid;
		halfn = floor(n/2);
		
		/* if n=even, either we duplicate the middle color, or use 1
		   less color in the first half
		*/
		if even then do;
			if dup then do;
				n1=halfn;
				d1=0;
				end;
			else do;
				n1=halfn+1;
				d1=1;
				end;
			end;
		else do;  /* n=odd */
			n1=halfn;
			d1=1;
			end;
		call symput('n1', left(put(n1,4.)));
		call symput('n2', left(put(halfn,4.)));
		call symput('d1', left(put(d1,4.)));
	run;

   /* Calculate the top to middle and middle to bottom             */
   /* ranges, and combine these into the &OUT= data set.           */
	%colorinc(&start, &mid, out=t2m, number=&n1, drop=&d1);
	%colorinc(&mid, &end, out=m2b, number=&n2, drop=0);
	
	data &out;
		 set t2m m2b;
		 &colorvar = 'CX' || left(rgb);
		 nc=&n;
		 colornum=_n_;
	label
		colornum  = 'Color number'
		nc = 'Number of colors'
		&colorvar = 'SAS/Graph RGB color (hex)'
		red = 'Red (dec)'
		green = 'Green (dec)'
		blue = 'Blue (dec)'
		rgb = 'RGB color (hex)'
	;
	   run;
%end;

/* If there is no middle color,                                */
%else %do;
	%colorinc(&start, &end, out=t2b, number=&n, drop=0);
	data &out;
		 set t2b;
		 &colorvar = 'CX' || left(rgb);
		 nc=&n;
		 colornum=_n_;
	label
		colornum  = 'Color number'
		nc = 'Number of colors'
		&colorvar = 'SAS/Graph RGB color (hex)'
		red = 'Red (dec)'
		green = 'Green (dec)'
		blue = 'Blue (dec)'
		rgb = 'RGB color (hex)'
	;
	   run;
%end;

%if %substr(%upcase(&display),1,1)=Y %then %do;
   /* The T2B dataset now contains the original and intermediate  */
   /* colors, in order of creation.  This annotation will display */
   /* both a color swatch and the RGB hex value for each color.   */
   data _swatch_;
	  length color function $8 style $10 text $20; 
	  retain xsys ysys '1' when 'b' ;
	  set &out;
	  if _n_=1 then y=90;
	  function='move'; x=0; output;
	  function='bar '; x=60; y+-floor(90/(&n));
	  	style='solid';
		color="cx"||rgb;output;
	  function='label';x=62;position='3'; style=' ';
	  	text=rgb; color='black'; output;
	  x=75;
	  	text='rgb: ' || put(red,4.) ||  put(green,4.) || put(blue,4.); output;
	  run;

   /* Create an image with the annotation.  To export this image, */
   /* use a GOPTIONS and FILENAME statement before invoking       */
   /* the macro.                                                  */ 
   *goptions cback=white;
   proc gslide anno=_swatch_;
   title1 "&n Colors: from &start to &end"
   %if %length(&mid) %then " via &mid";
    ;  
   run;quit;
   title1;
   %end;

%if %length(&order) %then %do;
	%if %substr(%upcase(&order),1,3)=REV %then %do;
	proc sort data=&out;
		by descending colornum;
		run;
		%end;
	%end;
	
%if %length(&result) %then %do;

	%global &result;
	proc sql noprint;
		select  &colorvar into :&result separated by ' '
		from &out;
		quit;
	%let &result = &&&result;
	%end;


%DONE:
%mend;

/*
Handle translation of various forms to RGB
*/

%macro makergb(name);
%local result;
%let name=%upcase(&name);

%if (%length(&name)=6 and
	%verify(&name,0123456789ABCDEF)=0) 
	%then %let result=&name;

	/* convert Hhhhllss to CXrrggbb */
%else %if (%substr(&name,1,1)=H and
	%length(&name)=8 and
	%verify(%substr(&name,2),0123456789ABCDEF)=0)
	%then %do;
		%let result= %hls2rgb(&name);
		%put NOTE: HLS color: &name converted to RGB: &result;
		%end;

	/*  convert SAS color name to CXrrggbb */
%else %if (%substr(&name,1,2)^=CX or 
	%length(&name)^=8 or
	%verify(&name,0123456789ABCDEF)) 
	%then %do;
		%let result = %hls2rgb(%cns(&name));
		%put NOTE: Apparent SAS Color name: &name converted to RGB: &result;
		%end;
%else %let result = &name;

	/* return rrggbb */
%if %substr(&result,1,2)=CX %then %let result=%substr(&result,3);
&result
%mend;

%macro colorinc(start, end, number=6, drop=1, out=data);

options nonotes;
data &out;
	keep red green blue rgb;
   /* Get the colors.                                               */
   start=upcase("&start");
   end=upcase("&end");
   if substr(start,1,2)='CX' then start=substr(start,3);
   if substr(end,1,2)='CX' then end=substr(end,3);

   /* Find the starting and ending values for Red, Green, and Blue. */
	sred=input(substr(start,1,2), hex.);
	ered=input(substr(end,1,2), hex.);
	sgreen=input(substr(start,3,2), hex.);
	egreen=input(substr(end,3,2), hex.);
	sblue=input(substr(start,5,2), hex.);
	eblue=input(substr(end,5,2), hex.);

   /* Calculate the increments for Red, Green, and Blue.            */
	incr = (ered-sred)/(&number-1);
	incg = (egreen-sgreen)/(&number-1);
	incb = (eblue-sblue)/(&number-1);

	do i=0 to &number-1-&drop;
		red   = round(sred + i*incr);
		green = round(sgreen + i*incg);
		blue  = round(sblue + i*incb);
		rgb=put(red,hex2.)||put(green,hex2.)||put(blue,hex2.);
	  	output;
	  	end;
run;
options notes;
%mend;

