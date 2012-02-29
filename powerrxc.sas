 /*-------------------------------------------------------------------*
  *    Name: powerrxc.sas                                             *
  *   Title: Power for ChiSquare test of independence                 *
        Doc: http://www.datavis.ca/sasmac/powerrxc.html            
  *-------------------------------------------------------------------*
  * Original:  SAS Institute                                          *
  * Modified:  Michael Friendly            <friendly@yorku.ca>        *
  * Revised: 19 Dec 1999 10:15:45                                     *
  * Version: 1.0                                                      *
  *  - Added plots                                                    *
  *                                                                   *
  * From ``Visualizing Categorical Data'', Michael Friendly (2000)    *         
  *-------------------------------------------------------------------*/
 /*=
=Description:
 
 The POWERRXC macro computes approximate power for Pearson and Likelihood 
 Ratio Chi-square tests of independence in a two-way table.


=Usage:

 The POWERRXC macro is defined with keyword parameters.
 The arguments may be listed within parentheses in any order, separated
 by commas. For example: 
 
	%powerrxc();
 
==Parameters:

* DATA=       Specifies the SAS data set to be analyzed.  If the
              DATA= option is not supplied, the most recently
              created SAS data set is used [Default: DATA=_LAST_]

* ROW=        REQUIRED.  Specifies the variable defining the rows 
              of the table.

* COL=        REQUIRED.  Specifies the variable defining the 
              =columns of the table.

* COUNT=      Specifies a variable containing the cell counts of 
              the table.  Omit this option if each observation in 
              the DATA= data set represents only a single entry in 
              the table.  This variable, if specified, is used on 
              the WEIGHT statement in PROC FREQ.

* LEVEL=      The significance level of the test [Default: LEVEL=.05]

* ALPHA=      Synonym for LEVEL= 

* NRANGE=     Specifies the sample size or list of sample sizes for 
              which approximate power is to be computed.  If
              omitted, the actual sample size is used.  You may
              specify a list of values separated by commas, a range
              of the form x TO y BY z, or a combination of these.
              However, you must surround the NRANGE= value with
              %STR() if any commas appear in it.  For example,

                        nrange=20 to 200 by 20
                        nrange=%str(20,50,100,140)
                        nrange=%str(10, 20, 50 to 100 by 10)


* PLOT=       Specifies what to plot.  [Default: PLOT=POWERP * N]

* FREQOPT=   Specifies options for PROC FREQ. [Default: FREQOPT=NOROW 
             NOCOL NOPERCENT].

* OUT=       Specifies the name of the output dataset. 
             [Default: OUT=_POWER_] 

 =*/
/************************************************************************

                                POWERRxC
                                                                       

   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   PURPOSE:
     Computes approximate power for Pearson and Likelihood Ratio
     Chi-square tests of independence in FREQ.
                                                                       
   REQUIRES:
     POWERRxC requires only release 6.10 or later of base SAS Software.

   PRINTED OUTPUT:
     The FREQ procedure prints the table and related chi-square 
     statistics.  Following this, the macro prints the approximate power
     of the Pearson chi-square test and the Likelihood Ratio chi-square
     test for range of sample sizes requested.  For example:


            Approximate Power of Chi-square Tests for Independence
                                Test Level=.05

                               Power of        Power
                                Pearson       of L.R.
                        N     Chi-square    Chi-square

                        20      0.06570       0.06735
                        30      0.07393       0.07650
                        40      0.08241       0.08592
                        50      0.09111       0.09562
                        60      0.10003       0.10558
                        70      0.10916       0.11577
                        80      0.11847       0.12619
                        90      0.12797       0.13681
                       100      0.13763       0.14762

   DETAILS:
     See section 7.6 of the Agresti reference below for statistical
     details.

   LIMITATIONS:
     LIMITED ERROR CHECKING IS DONE.  If the DATA= option is specified,
     be sure the named data set exists.  If DATA= is not specified, a
     data set must have been created previously in the current SAS
     session.  Be sure that the variables specified in the ROW=, COL= 
     and COUNT= options exist on that data set.  Running PROC CONTENTS
     on the data set prior to using this macro is recommended for
     verifying the data set name and the names of variables.

   SEE ALSO:
     POWER2x2 -- Computes the power of a test comparing proportions from
                 two, equal-sized, independent samples.  Power is given
                 for various sizes of the total sample, allowing you to
                 pick the sample size that achieves the desired power.

     PWR2x2un -- Computes the power of a test comparing proportions from
                 two, unequally-sized, independent samples.

   REFERENCE:
     Agresti, A. (1990), Categorical Data Analysis, New York: John Wiley
     & Sons, Inc.

   EXAMPLES:
     From Example 3 in the FREQ procedure chapter.  Note that each 
     observation is a cell count instead of a single entry, so a count 
     variable (FREQ) is created to hold the cell counts.  The observed 
     sample size is used for computing approximate power.
        
        data a;
        do row=1 to 2; do col=0,1;
          input freq @@; output;
        end; end; cards;
        3 11
        6 2
        ;
        
        %powerRxC(row=row, col=col, count=freq)


     This example is identical to the previous one, except each 
     observation in data set AA is now a single entry in the table, so a 
     count variable is no longer required.

        data aa;
        do row=1 to 2; do col=0,1;
          input freq @@; 
          do i=1 to freq; 
            drop i freq;
            output; 
          end;
        end; end; 
        cards;
        3 11
        6 2
        ;

        %powerRxC(row=row, col=col)


     Suppose subjects are to be given one of four treatments and a
     binary response will be recorded for each subject resulting in a
     4x2 table.  Determine the sample size needed to detect a 0.1
     difference in response probabilities with adequate power.  This is
     done by entering cell values that exhibit the worst case scenario
     -- three rows with response probability .2 (=.05/(.2+.05)) and one
     row with response probability .1 (=.025/(.225+.025)).  Treatment 
     sample sizes are to be equal, so the marginal row probabilities are 
     all 0.25.  Here, column 2 is regarded as the response category,
     though the columns could be reversed without effect. 

        data bb;
        do trt=1 to 4; do response=0,1;
          input prob @@; output;
        end; end; cards;
        .2   .05
        .2   .05
        .2   .05
        .225 .025
        ;

        %powerRxC(row=trt, col=response, count=prob, 
                  nrange=%str(20,50,100 to 1000 by 100))


     Example from Agresti (1990) pp 241-243.  Column 1 probabilities for
     each row are hypothesized to be .63 and .57.  Row sample sizes are 
     to be equal, so the marginal row probabilities are both 0.5.
     
        data c; 
          do row=1 to 2; do col=0,1;
             input freq @@; 
             output; 
          end; end; 
          cards; 
        .315 .185 
        .285 .215
        ;

        %powerRxC(data=c, row=row, col=col, count=freq, 
                  nrange=%str(20,50 to 200 by 50))


     Get power for a .10 level test performed on previously-collected 
     data.

        data d; 
          input type $ sex $ ;
          cards;
        a m
        b m
        c f
        b m
        c f
        c f
        b f
        a m
        a f
        a m
        b m
        c f
        b m
        c f
        c f
        b f
        a m
        a f
        ;

        %powerRxC(row=type, col=sex, level=.10)                                                                       

************************************************************************/


%macro powerRxC(
	data=_last_,   /* input data set                          */
	row=,          /* the row variable - REQUIRED             */
	col=,          /* the column variable - REQUIRED          */
	count=,        /* the variable of frequency counts,
							if the input data are cell counts
							of a table                              */
	level=.05,     /* the significance level of the test      */
	alpha=&level,  /* synonym for level                       */
	
	nrange=,       /* the sample size or range of sample
							sizes for which power is desired. 
							If not specified, the actual sample
							size is used.  Examples:
							nrange=20 to 200 by 20
							nrange=%str(20,50,100,140)
							nrange=%str(20, 50 to 100 by 10)
							Note that %STR() should be used when
							commas appear in your range 
							specification.                      */
	plot = powerp * n,
	freqopt = norow nocol nopercent,
	out=_power_  
);

options nonotes;
%let lastds=&syslast;

%if %bquote(&row)= %then %do;
   %put ERROR: The ROW= argument must be specified.;
   %goto exit;
%end;

%if %bquote(&col)= %then %do;
   %put ERROR: The COL= argument must be specified.;
   %goto exit;
%end;

%if %sysevalf(&sysver  < 6.10) %then %do;
   %put The POWERRXC macro requires SAS Version 6.10 or later.;
   %goto exit;
%end;

proc freq data=&data;
  %if %bquote(&count) ne %then weight &count%str(;);
  tables &row * &col / &freqopt chisq out=_cells;
  output out=_chi pchi lrchi;
  run;

data &out;
  keep n alpha powerp noncenp powerlr noncenlr; 
  merge _cells _chi;
  if _n_=2 then stop;
  sampsize=100*count/percent;
  label powerp ="Power of/Pearson/Chisquare"
        powerlr="Power/of L.R./Chisquare"
		  noncenp='Pearson/Noncentrality'
		  noncenlr='L.R. Chisquare/Noncentrality';
  do n=  %if %bquote(&nrange)= %then sampsize; %else &nrange; ;
    noncenp = _pchi_*n/sampsize;
    noncenlr = _lrchi_*n/sampsize;
	 do alpha = &level;
		powerp =1-probchi(cinv(1-&level,df_pchi), df_pchi, noncenp);
		powerlr=1-probchi(cinv(1-&level,df_lrchi), df_lrchi, noncenlr);
		output;
	 	end;
  end;
  call symput('df', trim(left(put(df_pchi,3.))) );
  run;

	
*title 'Power for ChiSquare test of independence';
proc print data=&out noobs split='/';
  var n alpha powerp noncenp powerlr noncenlr; 
  title2  "Approximate Power of Chi-square Tests for Independence (&df df)";
*  title2 "Test Level alpha=&level (&df df)";
  run;
title;

%if %length(&plot)>0 %then %do;
data _null_;
	if (0=1) then set &out nobs=nobs;
	call symput('nobs', trim(left(put(nobs,3.))) );
	run;

	%if &nobs<2 %then %do;
		%put WARNING:  There are too few observations to plot.
		%goto exit;
		%end;

	proc gplot data=&out;
		plot &plot / frame hminor=1 vaxis=axis1 haxis=axis2;
	axis1 label=(a=90);
	axis2 offset=(3);
	symbol1 v=dot   i=join l=1 c=black;
%end;

%exit:;
options notes  /*_last_=&lastds */;
title2;
%mend powerRxC;
