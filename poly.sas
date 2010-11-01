 /*--------------------------------------------------------------*
  *    Name: poly.sas                                            *
  *   Title: Generate polynomial contrasts                       *
        Doc: http://www.datavis.ca/sasmac/poly.html        
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 25 Feb 1998 13:05                                   *
  * Revised: 01 Oct 2008 15:16:28                                *
  * Version: 1.0-2                                               *
  *  -added levels=2                                             *
  *  -added documentation                                        *
  *  -fixed some errors in the coefficients (thx: Mike Newman)   *
  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The POLY macro generates a polynomial contrast for use in a CONTRAST
 statement with PROC GLM.  It assumes that the levels of the factor
 are equally spaced and the sample size for the levels of the factor
 are equal.  It returns the program text which should follow the
 CONTRAST (or ESTIMATE) keyword.

==Method:

The program simply uses coefficients from published tables, producing
text that can be inserted into CONTRAST or ESTIMATE statements.  
The original source for many of these was 
Fisher & Yates, "Statistical Tables for Biological, Agricultural
and Medical Research," Edinburgh: Oliver & Boyd.
This gave coefficients only up to degree 5 for up to 10 levels.

In the current version, some errors were corrected using Table VII
of 
Hayes, W. L (1994). "Statistics," 5th ed., Orlando, FL: Harcourt
Brace College Publishers.
This gives coefficients up to degree 6, for up to 15 levels.



=Usage:

 The poly macro is called with positional parameters.
 The arguments must be listed within parentheses, separated
 by commas. For example: 
 
    contrast %poly(4,1,A);
 yields:
    contrast "A-lin" A  -3  -1   1   3;
 
    %poly(4,1);
 yields:
    -3  -1   1   3
 
==Parameters:

* LEVELS=	Specifies the number of levels of the factor.  The number
            of levels must be between 2 and 10.

* DEGREE=	Specifies the degree of the polynomial contrast. The degree
            must be between 1 and min(9,levels-1).

* FACTOR=	Specifies the name of the factor.  If the factor name is
            not specified, the macro returns only the contrast coefficients.

 =*/
 
%macro poly(levels, degree, factor);
%local label coef;

%if %length(&factor) %then %do;
      %if &degree=1 %then %let label=&Factor-lin;
%else %if &degree=2 %then %let label=&Factor-quad;
%else %if &degree=3 %then %let label=&Factor-3rd;
%else %let label=&Factor-&degree.th;
%end;

%if &levels=2 %then %do;
	      %if &degree=1 %then %let coef = %str( -1   1);
	%else %let coef = %str(Not available for degree &degree);
%end;

%else %if &levels=3 %then %do;
	      %if &degree=1 %then %let coef = %str( -1   0   1);
	%else %if &degree=2 %then %let coef = %str(  1  -2   1);
	%else %let coef = %str(Not available for degree &degree);
%end;

%else %if &levels=4 %then %do;
	      %if &degree=1 %then %let coef = %str( -3  -1   1   3);
	%else %if &degree=2 %then %let coef = %str(  1  -1  -1   1);
	%else %if &degree=3 %then %let coef = %str( -1   3  -3   1);
	%else %let coef = %str(Not available for degree &degree);
%end;

%else %if &levels=5 %then %do;
	      %if &degree=1 %then %let coef = %str(  -2  -1   0   1   2);
	%else %if &degree=2 %then %let coef = %str(   2  -1  -2  -1   2);
	%else %if &degree=3 %then %let coef = %str(  -1   2   0  -2   1);
	%else %if &degree=4 %then %let coef = %str(   1  -4   6  -4   1);
	%else %let coef = %str(Not available for degree &degree);
%end;

%else %if &levels=6 %then %do;
	      %if &degree=1 %then %let coef = %str( -5  -3  -1   1   3   5);
	%else %if &degree=2 %then %let coef = %str(  5  -1  -4  -4  -1   5);
	%else %if &degree=3 %then %let coef = %str( -5   7   4  -4  -7   5);
	%else %if &degree=4 %then %let coef = %str(  1  -3   2   2  -3   1);
	%else %if &degree=5 %then %let coef = %str( -1   5 -10  10  -5   1);
	%else %let coef = %str(Not available for degree &degree);
%end;

%else %if &levels=7 %then %do;
	      %if &degree=1 %then %let coef = %str( -3  -2  -1   0   1   2   3);
	%else %if &degree=2 %then %let coef = %str(  5   0  -3  -4  -3   0   5);
	%else %if &degree=3 %then %let coef = %str( -1   1   1   0  -1  -1   1);
	%else %if &degree=4 %then %let coef = %str(  3  -7   1   6   1  -7   3);
/*
	%else %if &degree=5 %then %let coef = %str( -8  34 -42   0  42 -34   8);
	%else %if &degree=6 %then %let coef = %str(  3 -15  38 -51  38 -15   3);
*/
	%else %if &degree=5 %then %let coef = %str( -1  4  -5    0   5  -4   1);
	%else %if &degree=6 %then %let coef = %str(  1 -6  15  -20  15  -6   1);

	%else %let coef = %str(Not available for degree &degree);
%end;

%else %if &levels=8 %then %do;
	      %if &degree=1 %then %let coef = %str( -7  -5  -3  -1   1   3   5  7);
	%else %if &degree=2 %then %let coef = %str(  7   1  -3  -5  -5  -3   1  7);
	%else %if &degree=3 %then %let coef = %str( -7   5   7   3  -3  -7  -5  7);
	%else %if &degree=4 %then %let coef = %str(  7 -13  -3   9   9  -3 -13  7);
	%else %if &degree=5 %then %let coef = %str(-12  38 -28 -25  25  28 -38 12);
/*
	%else %if &degree=6 %then %let coef = %str( -5 -24  43 -24 -24  43 -24  5);
*/
	%else %if &degree=6 %then %let coef = %str(  1  -5   9  -5  -5   9  -5  1);

	%else %if &degree=7 %then %let coef = %str( -1   9 -28  46 -46  28  -9  1);
	%else %let coef = %str(Not available for degree &degree);
%end;
 
 
%else %if &levels=9 %then %do;
	      %if &degree=1 %then %let coef = -4  -3  -2  -1   0   1   2   3   4;
	%else %if &degree=2 %then %let coef = 28   7  -8 -17 -20 -17  -8   7  28;
	%else %if &degree=3 %then %let coef =-14   7  13   9   0  -9 -13  -7  14;
	%else %if &degree=4 %then %let coef = 14 -21 -11   9  18   9 -11 -21  14;
	%else %if &degree=5 %then %let coef = -4  11  -4  -9   0   9   4 -11   4;
	%else %if &degree=6 %then %let coef =  7 -30  38   2 -35   2  38 -30   7;
	%else %let coef = %str(Not available for degree &degree);
%end;

%else %if &levels=10 %then %do;
	      %if &degree=1 %then %let coef = -9  -7  -5  -3  -1   1   3   5   7   9;
/*
	%else %if &degree=2 %then %let coef =  3   1  -0  -2  -2  -2  -2  -1   1   3;
*/
	%else %if &degree=2 %then %let coef =  6   2  -1  -3  -4  -4  -3  -1   2   6;
	%else %if &degree=3 %then %let coef =-35  12  29  26  10 -10 -26 -29 -12  35;
	%else %if &degree=4 %then %let coef = 18 -22 -17   3  18  18   3 -17 -22  18;
	%else %if &degree=5 %then %let coef = -6  14  -1 -11  -6   6  11   1 -14   6;
	%else %if &degree=6 %then %let coef =  3 -11  10   6  -8  -8   6  10 -11   3;
	%else %let coef = %str(Not available for degree &degree);
%end;

%else %let coef = %str(Not available for &levels levels);

%*-- Return the info to go after 'contrast';
%if %length(&factor) %then %do;
"&label" &factor &coef;
%end;
%else %do;
&coef
%end;
%mend;

 /*=
=Description:

 The INTER macro generates a CONTRAST statement for the interaction
 between two polynomial contrasts.
 
=Usage:

 The INTER macro is called with positional parameters.
 The arguments must be listed within parentheses, separated
 by commas. For example: 
 
    contrast %inter(A, 2, 1, B, 3, 1);
 yields:
    contrast "A-lin x B-lin" A*B  -1  0  1  1  0  -1;
 
==Parameters:

* FAC1=  	Specifies the name of the 1st factor.  

* LEV1=  	Specifies the number of levels of the 1st factor.  

* DEG1=  	Specifies the degree of the polynomial for factor 1. 

* FAC2=  	Specifies the name of the 2nd factor.  

* LEV2=  	Specifies the number of levels of the 2nd factor.  

* DEG2=  	Specifies the degree of the polynomial for factor 2. 
 
 =*/
 
%macro inter(fac1, lev1, deg1, fac2, lev2, deg2);
%local c1 c2 coef label f1 f2;

%let c1 = %poly(&lev1, &deg1);
%let c2 = %poly(&lev2, &deg2);
%let coef = %dirpro(&c1, &c2);

%if %length(&fac1)>6 
	%then %let f1=%substr(&fac1,1,6);
	%else %let f1=&fac1;
%if %length(&fac2)>6 
	%then %let f2=%substr(&fac2,1,6);
	%else %let f2=&fac2;

      %if &lev1=2 %then %let label = &f1;
%else %if &deg1=1 %then %let label = &f1-lin;
%else %if &deg1=2 %then %let label = &f1-2nd;
%else %if &deg1=3 %then %let label = &f1-3rd;
%else %let label = &f1-&deg1.th;

      %if &lev2=2 %then %let label = &label x &f2;
%else %if &deg2=1 %then %let label = &label x &f2-lin;
%else %if &deg2=2 %then %let label = &label x &f2-2nd;
%else %if &deg2=3 %then %let label = &label x &f2-3rd;
%else %let label = &label x &f2-&deg1.th;

"&label" %str(&fac1 * &fac2) &coef;
%mend;

%macro dirpro(c1, c2);
%local i j w1 w2 r;

%let i=1;
%let w1 = %scan(&c1, &i, %str( ));
%do %while(&w1 ^= );

	%let j=1;
	%let w2 = %scan(&c2, &j, %str( ));

	%do %while(&w2 ^= );
		%let r = &r %eval(&w1 * &w2);
		%* put i=&i j=&j r=&r;
		%let j=%eval(&j+1);
		%let w2 = %scan(&c2, &j, %str( ));
		%end;

	%let i=%eval(&i+1);
	%let w1 = %scan(&c1, &i, %str( ));
	%end;
&r
%mend;

/*
%macro testit;
%do l=3 %to 7;
	%do d=1 %to 4;
	%put contrast %poly(&l, &d, A);
	%end;
	%end;

%* put contrast %dirpro(-3 -1 1 3, -1 0 1); ;
%* put contrast %dirpro(%poly(4,1), %poly(4,1)); ;
%put contrast %inter(A, 2, 1, B, 4, 1);
%mend;
%testit;
*/
