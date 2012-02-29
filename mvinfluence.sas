 /*--------------------------------------------------------------*
  *    Name: mvinfluence.sas                                     *
  *   Title: Influence measures for multivariate regression      *
  *     Doc: http://datavis.ca/sasmac/nodoc.html      
  *--------------------------------------------------------------*
  *  Author:  Michael Friendly            <friendly@yorku.ca>    *
  * Created: 15 Jan 2012 15:01:15                                *
  * Revised: 22 Jan 2012 12:23:05                                *
  * Version: 1.0-1                                               *
    - Added diagonal reference lines to LR plot

  *--------------------------------------------------------------*/
 /*=
=Description:
 
 The MVINFLUENCE macro calculates and plots measures of influence in a
 multivariate regression model.  These are generalizations of the usual
 influence measures for univariate regression described by Barrett &
 Ling (1992).  This is an initial, experimental implementation designed
 to explore thiese methods.
 
 Three types of plots are provided (controlled by the PLOTS= argument) 
 
 COOKD: scatterplot of generalized Cook's D against generalized leverage
 STRES: bubble plot of generalized squared studentized residual against generalized leverage using Cook's D as bubble size
 LR:    bubble plot of log residual factor against log leverage factor using Cook's D as bubble size 

=Usage:

 The MVINFLUENCE macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%mvinfluence(data=rohwer, x=n s ns na ss, y=SAT PPVT Raven)
 
==Parameters:

* DATA=       The name of the input data set [Default: DATA=_LAST_]

* Y=          Names of response variables

* X=          Names of predictor variables

* ID=         The name of an observation ID variable

* BUBBLE=     Bubble proportional to: COOKD.  No other choices in this implementation [Default: BUBBLE=COOKD]

* OUT=        The name of the output data set [Default: OUT=COOKD]

* PRINT=      Print the OUT= data set? Not yet implemented. [Default: PRINT=NONE]

* PLOTS=      Which plots to produce? NONE, ALL, or one or more of COOK, STRES, LR [Default: PLOTS=ALL]

* LABEL=      Points to label: ALL, NONE, or INFL [Default: LABEL=INFL]

* INFL=       Criteria for influential [Default: INFL=%STR(((HAT>&HCRIT)|COOKD>.5))]

* LSIZE=      Obs label size.  The height of other text is controlled by the HTEXT= goption [Default: LSIZE=1.6]

* LCOLOR=     Obs label color [Default: LCOLOR=BLACK]

* LPOS=       Obs label position [Default: LPOS=5]

* LFONT=      Obs label font

* BSIZE=      Bubble size scale factor [Default: BSIZE=10]

* BSCALE=     Bubble size proportional to AREA or RADIUS [Default: BSCALE=AREA]

* BCOLOR=     Bubble color [Default: BCOLOR=RED]

* BFILL=      Bubble fill? SOLID or GRADIENT

* REFCOL=     Color of reference lines [Default: REFCOL=BLACK]

* REFLIN=     Line style for reference lines; 0->NONE [Default: REFLIN=33]

* NAME=       The name of the graph in the graphic catalog [Default: NAME=MVINFL]

* GOUT=       The name of the graphics catalog
                
==Dependencies:

%labels
%gskip

=References:

* Barrett, B. E. & Ling, R. F. (1992). General classes of influence measures for multivariate
regression, I<JASA>, 87, # 417, 184-191.

* Barrett, B. E. (2003): Understanding Influence in Multivariate Regression, I<Communications in Statistics
- Theory and Methods>, 32:3, 667-680

* Some code from Timm & Mieczkowski, Univariate and Multivariate General Linear Models,
http://ftp.sas.com/samples/A55809, Program 5_6.sas, was used in the initial implementation.                      


=Examples:
	%include data(rohwer);
	data rohwer;
		set rohwer;
		where group=2;
		drop subjno;
		case = _n_;
	%include macros(mvinfluence);
	%mvinfluence(data=rohwer, x=n s ns na ss, y=SAT PPVT Raven, id=case,
	bfill=gradient, plots=ALL);

 =*/


%macro mvinfluence(
     data=_last_,
     y=,             /* Names of response variables               */
     x=,             /* Names of predictor variables              */
     id=,            /* Name of observation ID variable (char)    */
     bubble=COOKD,   /* Bubble proportional to: COOKD             */
     out=cookd,      /* Name of output data set                   */
	 print=NONE,     /* Print the OUT= data set?                */
	 plots=ALL,      /* Which plots to draw? ALL, NONE, COOKD,  */
     label=INFL,     /* Points to label: ALL, NONE, or INFL     */
     infl=%str(((Hat>&hcrit)|CookD>.5)), /* Criteria for influential */
     lsize=1.6,      /* obs label size.  The height of other text is controlled by the HTEXT= goption  */
     lcolor=BLACK,   /* obs label color                         */
     lpos=5,         /* obs label position                      */
     lfont=,         /* obs label font                          */
     bsize=10,       /* bubble size scale factor                */
     bscale=AREA,    /* bubble size proportional to AREA or RADIUS */
     bcolor=RED,     /* bubble color                            */
     bfill=,         /* bubble fill? SOLID or GRADIENT          */
     refcol=gray,    /* color of reference lines                */
     reflin=33,      /* line style for reference lines; 0->NONE */
     name=MVINFL,    /* Name of the graph in the graphic catalog */
     gout=
	);

	%*-- Reset required global options;

	%if %sysevalf(&sysver  >= 7) %then %do;
		%local o1 o2;
		%let o1 = %sysfunc(getoption(notes));
		%let o2 = %sysfunc(getoption(validvarname,keyword));
		options nonotes validvarname=upcase;
		%end;
	%else %do;
	   options nonotes;
		%end;


%let abort=0;
%if %upcase(&data)=_LAST_ %then %let data = &syslast;
%let label=%upcase(&label);
%let bubble=%upcase(&bubble);
%let print=%upcase(&print);
%let plots=%upcase(&plots);
%if &gout^=%str()  %then %let gout=GOUT=&gout;
%let me=MVINFLUENCE;
%let cleanup=;

%if %length(&x) = 0 %then %do;
    %put ERROR: (&me) List of predictors (X=) is empty.;
    %let abort=1;
    %goto done;
    %end;

%if %length(&y) < 2 %then %do;
    %put ERROR: (&me) At least 2 response variables (Y=) must been specified.;
    %let abort=1;
    %goto done;
    %end;

%let nx = %numwords(&x);
%let ny = %numwords(&y);


/* Calculations for Regression and Multivariate Cooks Distance */
*title 'Using PROC IML: Multivariate Cooks Distance';
proc iml;
   use &data;
   yn={&y};
   xn={&x};
   read all var yn into Y;
   read all var xn into X;
   n=nrow(Y);
   p=ncol(Y);
   X = j(n,1) || X;     *-- add intercept;
   xn = 'Int' || xn;
   k=ncol(X);

   Beta=inv(t(X)*X)*t(X)*Y;
   print 'Regression Coefficients', Beta[c=yn r=xn];

   E = Y - X * Beta;
   S = t(E)*E / (n-k);
   XPXI = inv(t(X) * X);
   EPEI = inv(t(E) * E);
   print 'Estimated Covariance Matrix', S[r=yn c=yn];

   V=inv(k*S) @ (t(X)*X);
   B=shape(t(Beta), p*k, 1);

   obs = 1:n;
   do i=1 to n;
      rows = (1:n)[loc(obs ^= i)];
      Xi = X[rows,];
      Yi = Y[rows,];
      Betai = inv(t(Xi)*Xi) * t(Xi)*Yi;
	  Ei = (Y - X * Betai)[i,];
	  *if (i=1) then print 'Leave-one-out coef', i Betai[r=xn c=yn];
	  Bi = shape(t(Betai), p*k, 1);
	  Di = t(B-Bi) * V * (B-Bi);
	  hat = X[i,] * XPXI * t(X[i,]);
	  res = Ei * EPEI * t(Ei);
	  Li = hat / (1-hat);
	  Ri = res / (1-hat);
	  result = result // (Di || Hat || Res || Li || Ri);
      end;

   cn = {'CookD', 'Hat', 'StRes', 'L', 'R'};
   rn = char(obs);
   print 'Multivariate Influence Statistics', result[r=rn c=cn f=best9.4];
   create &out from result[colname=cn];
   append from result;
quit;


data &out;
	merge &data &out end=eof;
	N=_n_; drop N p hcrit hcrit1;

	label CookD = "Cook's D"
	      Hat = 'Leverage'
		  StRes = 'Squared Studentized Residual'
		  L = 'Leverage factor'
		  R = 'Studentized residual factor'
		  logL='log Leverage factor'
		  logR = 'log Residual factor';
	logL = log(L);
	logR = log(R);
	if eof then do;
		p = &nx+1;
		hcrit = 2 * p / N;
		hcrit1 = 3 * p / N;
		call symput('hcrit', put(hcrit, 4.3));
		call symput('hcrit1', put(hcrit1, 4.3));
        put "&me: Hatvalue criteria: 2p/n=" hcrit 4.3 ', 3p/n=' hcrit1 4.3;
		end;
run;

%if &plots = NONE %then %goto done;

%if &plots = ALL or %index(&plots,COOK)>0  %then %do;
	%labels(data=&out, x=Hat, Y=CookD, text=trim(left(&id)), 
	/*		subset=((Hat>&hcrit)|CookD>.5),  */
			subset=&infl, 
			out=_label_, pos='2', size=&lsize, font=&lfont);

	proc gplot data=&out &gout;
		plot CookD * Hat  / 
    	  vaxis=axis1 
		  vminor=1 hminor=1
		  href=&hcrit &hcrit1 lhref=&reflin chref=&refcol 
          %if &label ^= NONE %then %do;
           annotate=_label_
          %end;
		  name="&name"
		  des="Multivariate COOKD influence plot for data &data"
			;
		symbol1 v=dot;
		axis1 label=(a=90);
		run; quit;
	%gskip;
	%end;


%if &plots = ALL or %index(&plots,STRES)>0 %then %do;
	%labels(data=&out, x=Hat, Y=stres, text=trim(left(&id)),
			subset=((Hat>&hcrit)|CookD>.5), 
			out=_label_, pos="&lpos", size=&lsize, when=A, font=&lfont);

	proc gplot data=&out &gout;
		bubble stres * Hat = CookD  / 
    	  vaxis=axis1 
		  vminor=1 hminor=1
    	  href=&hcrit &hcrit1 lhref=&reflin chref=&refcol
          %if &label ^= NONE %then %do;
           annotate=_label_
          %end;
    	  bsize=&bsize  bcolor=&bcolor  bscale=&bscale
    	  %if %length(&bfill) %then %do; 
				bfill=&bfill
    	  %end;
			name="&name"
			des="Multivariate STRES influence plot for data &data"
			;
		axis1 label=(a=90);
		run; quit;
	%gskip;
	%end;

%if &plots = ALL or %index(&plots,LR)>0 %then %do;
	%let cleanup = _minmax_ ;

	proc summary data=&out;
		var logL logR;
		output out=_minmax_ min=xmin ymin max=xmax ymax;
	run;
	data _lines_;
		set _minmax_(drop=_type_ _freq_);
		xmin = floor(xmin); xmax = ceil(xmax);
		ymin = floor(ymin); ymax = ceil(ymax);
		length color function $8;
		retain xsys ysys '2' color "&refcol" line &reflin when 'B';
		drop xmin ymin xmax ymax v;

		*-- lines with slope = -1 along x axis;
		do v=xmin+1 to xmax by 1;
		x=v;    y=ymin;          function='move'; output;
		x=xmin; y=v-(xmin-ymin); function='draw'; output;
		end;

		*-- lines with slope = -1 along y axis;
		do v=ymin+1 to ymax-1 by 1;
		x=xmax;    y=v;           function='move'; output;
		y=ymax; x = xmax - (y-v); function='draw'; output;
/*
		x=v-(ymin-xmin);  y=ymax; function='draw'; output;
		x=min(xmin, v-(ymin-xmin));  y=min(ymax, v-(x-xmax)); function='draw'; output;
*/
		end;
	run;

	%labels(data=&out, x=logL, Y=logR, text=trim(left(&id)),
			subset=((Hat>&hcrit)|CookD>.5), 
			out=_label_, pos=&lpos, size=&lsize, when=A, font=&lfont,
			in=_lines_);
	run;
	*proc print data=_lab_;

	proc gplot data=&out &gout;
		bubble logR * logL = CookD  / 
    	  vaxis=axis1 
		  vminor=1 hminor=1
          %if &label ^= NONE %then %do;
           annotate=_label_
          %end;
    	  bsize=&bsize  bcolor=&bcolor  bscale=&bscale
    	  %if %length(&bfill) %then %do; 
				bfill=&bfill
    	  %end;
			name="&name"
			des="Multivariate LR influence plot for data &data"
			;
		axis1 label=(a=90);
		run; quit;
	%end;

%done:
%if &abort %then %put ERROR: The &me macro ended abnormally.;
	%*-- Restore global options;
	%if %sysevalf(&sysver  >= 7) %then %do;
		options &o1 &o2;
		%end;
	%else %do;
	   options notes;
		%end;
	%*-- Reset symbol statements;
	goptions reset=symbol;
 /*------------------------------------*
  | Clean up datasets no longer needed |
  *------------------------------------*/
%if %length(&cleanup) %then %do;
proc datasets nofs nolist library=work memtype=(data);
    delete &cleanup;
	 run; quit;
%end;

%mend;

%macro numwords(lst);
   %let i = 1;
   %let v = %scan(&lst,&i);
   %do %while (%length(&v) > 0);
      %let i = %eval(&i + 1);
      %let v = %scan(&lst,&i);
      %end;
   %eval(&i - 1)
%mend;

