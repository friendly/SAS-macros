 /*--------------------------------------------------------------*
  *    Name: genpat.sas                                          *
  *   Title:Generate multiple PATTERN statements                 *
        Doc: http://www.datavis.ca/sasmac/genpat.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 29 Aug 2004 12:49:05                                *
  * Revised: 13 Sep 2004 16:48:56                                *
  * Version: 1.0                                                 *
  *                                                              *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The GENPAT macro generates one or more PATTERN statements for a list
 of colors, specified either in the COLORS= macro argument, or in a
 DATA= data set, using the values of the COLORS= variable.

=Usage:

 The GENPAT macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%genpat(n=3);

 Produces:

     PATTERN1 fill=solid color=BLACK repeat=1;
     PATTERN2 fill=solid color=RED repeat=1;
     PATTERN3 fill=solid color=GREEN repeat=1;
 
==Parameters:

* N=           Number of PATTERN statements to generate.  If a DATA=
               data set is specified, you may use N=NOBS to mean the
			   number of observations in that data set. [Default: N=1]

* START=       Starting PATTERNn number [Default: START=1]

* COLORS=      Unless the DATA= parameter is specified, COLORS= gives a
               list of SAS/Graph color names or color specifications
               (e.g., RGB colors (CXrrggbb), HLS colors (Hhhhllss)
			   [Default: COLORS=BLACK RED GREEN BLUE BROWN ORANGE PURPLE YELLOW]

* FILL=        The fill type for the PATTERN statements (e.g., SOLID,
               EMPTY, Lx, Rx, Xx, MxXaaa etc.) [Default: FILL=SOLID]

* REPEAT=      Repeat value for each PATTERN statement [Default: REPEAT=1]

* DATA=       The name of an input data set, containing colors given in
              a variable specified by the COLORS= parameter.
                
=Examples:

	*-- 21 rainbow colors;
  %let rainbow=
   CXFF0000 CXFF4600 CXFF8B00 CXFFD100 CXE8FF00 CXA2FF00 CX5DFF00
   CX17FF00 CX00FF2E CX00FF74 CX00FFB9 CX00FFFF CX00B9FF CX0074FF
   CX002EFF CX1700FF CX5D00FF CXA200FF CXE800FF CXFF00D1 CXFF008B
   CXFF0046;

  %genpat(n=21, colors=&rainbow);
  
	*-- The same, with an input data set;
  data rainbow;
  	input color $ @@;
	datalines;
   CXFF0000 CXFF4600 CXFF8B00 CXFFD100 CXE8FF00 CXA2FF00 CX5DFF00
   CX17FF00 CX00FF2E CX00FF74 CX00FFB9 CX00FFFF CX00B9FF CX0074FF
   CX002EFF CX1700FF CX5D00FF CXA200FF CXE800FF CXFF00D1 CXFF008B
   CXFF0046
   ;
  %genpat(n=nobs, data=rainbow, colors=color);

=*/

%macro genpat(
   n=1,        /* number of PATTERN statements to generate */
   start=1,    /* starting PATTERNn number                 */
   colors=BLACK RED GREEN BLUE BROWN ORANGE PURPLE YELLOW,
   fill=solid, /* fill value                               */
   repeat=1,   /* repeat value                             */
   data=,      /* data set containing colors               */
   show=       /* show the pattern statements generated?   */
   );

	%local abort;
	%let abort=0;
	*--- Colors are listed in the macro COLORS= parameter;
	%if %length(&data) = 0 %then %do;   %* <-- if length(&data)=0 { ;
		%do k=&start %to %eval(&n + &start -1);  %*-- { ;

    	   %if %length(%scan(&fill, &k, %str( ))) = 0 
	    		%then %let fill = &fill &fill;
    	   %if %length(%scan(&colors, &k, %str( ))) = 0 
	    		%then %let colors = &colors &colors;

		   %let fil =%scan(&fill, &k,%str( ));
    	   %let col =%scan(&colors, &k, %str( ));

			%if &k=99 %then %let repeat=999;
    	   pattern&k value=&fil color=&col  r=&repeat;
		   %if %length(&show) %then %do;
    	   %put pattern&k value=&fil color=&col  r=&repeat%str(;);
		    %end;	
		  %if &k=99 %then %goto done;

		%*--  };		
		%end;  
	%*  if length(&data)=0 } --> ;
  	%end;


	%else %do;
	%if %length(%scan(&colors, 2, %str( ))) %then %do;
		%put ERROR: With the DATA=&data option, you must specify a single COLORS= variable in the &data data set;
		%let abort=1;
		%goto done;
		%end;
	data _null_;
		set &data end=eof nobs=nobs;
		%if %upcase(&n) = NOBS 
			%then %str(n = nobs;);
			%else %str(n = &n;); 
		if (_n_ <= n);
		call symput('color'||left(put(_n_,3.)),&colors);
		if eof or _n_ = n then call symput('nc', left(put(_n_,3.)));
		run;
	 %do i=1 %to &nc;
	 pattern%eval(&i+&start-1) value=&fill color=&&color&i repeat=&repeat;
	 %if %length(&show) %then %do;
	 %put pattern%eval(&i+&start-1) value=&fill color=&&color&i repeat=&repeat%str(;);
	  %end;	
	 %end;

	%end;
	
%done:
%mend;

/*
%let rainbow=
 CXFF0000 CXFF4600 CXFF8B00 CXFFD100 CXE8FF00 CXA2FF00 CX5DFF00
 CX17FF00 CX00FF2E CX00FF74 CX00FFB9 CX00FFFF CX00B9FF CX0074FF
 CX002EFF CX1700FF CX5D00FF CXA200FF CXE800FF CXFF00D1 CXFF008B
 CXFF0046;

options mprint;
%genpat(n=21, colors=&rainbow);
*/
