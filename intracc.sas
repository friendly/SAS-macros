 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: INTRACC                                             */
 /*   TITLE: Intraclass Correlations                             */
 /* PRODUCT: STAT                                                */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS:                                                     */
 /*   PROCS: GLM                                                 */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: saswss                      UPDATE:  03Dec93        */
 /*     REF:                                                     */
 /*    MISC:                                                     */
 /*                                                              */
 /****************************************************************/

 /*-------------------------------------------------------------------

 DISCLAIMER:

       THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE
 TO ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
 EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
 PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
 CONTAINED HEREIN.

 -------------------------------------------------------------------*/

 /*******************************************************************

INTRACC.SAS -- macro to calculate reliabilities for Intraclass
correlations.  

This macro calculates the six intraclass correlations
discussed in Shrout, P.E., and Fleiss, J.L "Intraclass correlations:
uses in assessing rater reliability," Psychological Bulletin, 1979,
86, 420-428.  Additionally it calculates two intraclass correlations
using formulae from Winer, B.J. "Statistical Principles in
Experimental Design," which are identical to two of the six from
Shrout and Fleiss.  

I did that as a check on myself because it is
reassuring when you get the same result both ways.
Additionally it calculates the reliability of the mean of nrater
ratings where nrater is a parameter of the macro, using the
Spearmen-Brown prophecy formula, so that one can examine the effect
obtaining more raters would have on the reliability of a mean.

Notation used in calculating the three correlations via the Winer
formulae is taken from Winer while notation used in calculating the
six correlations using the Shrout and Fleiss formulae is taken from
Shrout and Fleiss.  That means that in some cases I used two
differently named variables to hold the same thing so I could use
a variable with the same name as the reference when calculating
a correlation taken from that reference.

In Shrout and Fleiss notation, these six correlations and their
uses are as follows:
   ICC(1,1): used when each subject is rated by multiple raters,
             raters assumed to be randomly assigned to subjects,
             all subjects have the same number of raters.
   ICC(2,1): used when all subjects are rated by the same raters
             who are assumed to be a random subset of all possible
             raters.
   ICC(3,1): used when all subjects are rated by the same raters
             who are assumed to be the entire population of raters.
   ICC(1,k): Same assumptions as ICC(1,1) but reliability for the
             mean of k ratings.
   ICC(2,k): Same assumptions as ICC(2,1) but reliability for the
             mean of k ratings.
   ICC(3,k): Same assumptions as ICC(3,1) but reliability for the
             mean of k ratings.  Assumes additionally no subject by
             judges interaction.

Instructions:
   The macro arguments are as follows, where "!" indicates a
   required argument:

* data=   SAS dataset containing data. Default is _LAST_.
! target= variable indexing the experimental units, often
			subjects or persons, each of whom is rated several
			times.
! rater=  variable indexing judge, or whatever is producing
			multiple ratings for each subject.
! depvar= dependent variable list, or list of variables for
			which each target was rated by each rater.
* nrater= For use in Spearman-Brown Prophecy formula to
			estimate the reliability of the mean of nrater
			ratings, where nrater is different than the number
			of raters actually used in the data. Default is 0,
			which omits this computation.
* out=    Name of output data set to contain the statistics.
			Default is _DATA_.
* print=  0 for no printout, 1 to print the intraclass
			correlations and related statistics, 2 to print
			the summary statistics from GLM as well, 3 to print
			all the GLM results as well. Default is 1.
* details= 1 to include detail variables, 
         MSW MSB WMS EMS EDF BMS BDF JMS JDF
			0 to exclude

If there are n targets and k ratings for each target, each target-
rating occupies one observation, or in other words, there are n*k
observations in the dataset.
The macro uses GLM to break the total variability into that due to
between targets, between judges, and residual.  For the formulae
which assume a one-way design, the SS and DF for between judges and
residual are added to give simply a within-targets SS.
This macro assumes that all targets are rated by judges numbered
with the same judge numbers, even if they are not the same judges.
In other words, each subject is rated by k judges, labeled, say,
1,2,...,k, even if they are not the same judges for each subject.
That is so GLM can break out a between judges SS.

Example:

 data ratings;
   do product=1 to 5;
     do judge=1 to 3;
       input rating @@;
       output;
     end;
   end;
 cards;
 1 1 5
 3 2 6
 5 3 7
 7 4 8
 9 5 9
 ;

 %intracc(depvar=rating,target=product,rater=judge,nrater=10);
 %intracc(data=rating,depvar=rating,target=product,rater=judge,
          print=3,out=intclcor);

Robert M. Hamer, Ph.D., Associate Professor of Psychiatry and
Biostatistics, Virginia Commonwealth University, 2-7-1991.

Copyright (C) 1990 by Robert M. Hamer, all rights reserved.
This macro may be distributed freely as long as all comments are
included.

 ********************************************************************/

%macro intracc(data=_LAST_,
	target=TARGET???,
	rater=RATER???,
   depvar=DEPVAR???,
	nrater=0,out=_DATA_,
	print=1,
	details=0);

title2 'Intraclass Correlations for Inter-Rater Reliability';
proc glm data=&data outstat=_stats_
  %if &print<3 %then noprint; ;
  * use glm to get sums of squares for use in reliability calculation;
  class &target &rater;
  model &depvar = &target &rater ;
  run;

proc sort data=_stats_;
  by _name_ _SOURCE_;
  where _type_ ^= 'SS1';
  run;

%if &print>=2 %then %do;
proc print data=_stats_;
  title3 'Statistics from 2-way ANOVA w/o Interaction';
  run;
%end;

data &out;
*  title3 'Calculate all reliabilities in one fell swoop';
  title3;
  retain msw msb wms ems edf bms bdf jms jdf k;
  set _stats_;
  by _name_;
  if _type_='SS1' then delete;
  if _source_='ERROR' then do;
     ems=ss/df;
     edf=df;
  end;
  if _source_="%upcase(&target)" then do;
     bms=ss/df;
     msb=bms;
     bdf=df;
  end;
  if _source_="%upcase(&rater)" then do;
     jms=ss/df;
     jdf=df;
     k=df+1;
  end;
  if last._name_ then do;
    msw=((ems*edf)+(jms*jdf))/(edf+jdf);
    wms=msw;
    n=bdf+1;
    theta=(msb-msw)/(k*msw);                   * used in Winer formulae;
    wsingle=theta/(1+theta);                   * Winer ICC(1,1);
    wk=(k*theta)/(1+k*theta);                  * Winer ICC(1,k);
    %if &nrater %then %do;
    wnrater=(&nrater*theta)/(1+&nrater*theta); * Winer reliability
                                                 if mean of nraters;
    %end;
    sfsingle=(bms-wms)/(bms+(k-1)*wms);        * ICC(1,1);
    sfrandom=(bms-ems)/
        ((bms)+((k-1)*ems)+((k*(jms-ems))/n)); * ICC(2,1);
    sffixed=(bms-ems)/(bms+((k-1)*ems));       * ICC(3,1);
    sfk=(bms-wms)/bms;                         * ICC(1,k);
    sfrandk=(bms-ems)/(bms+((jms-ems)/n));     * ICC(2,k);
    sffixedk=(bms-ems)/bms;                    * ICC(3,k) with no
                                                 interaction assumption;
    output;
  end;
  label wsingle="Winer: single score"
        wk="Winer: mean of k scores"
        %if &nrater %then %do;
        wnrater="Winer: mean of &nrater scores"
        %end;
        sfsingle="Shrout-Fleiss ICC(1,1): single score"
        sfrandom="Shrout-Fleiss ICC(2,1): random set"
        sffixed="Shrout-Fleiss ICC(3,1): fixed set"
        sfk="S-F ICC(1,k): mean k scores"
        sfrandk="S-F ICC(2,k): rand set mean k scrs"
        sffixedk="S-F ICC(3,k): fixd set mean k scrs";
run;

%if &print %then %do;
proc print label;
  id _name_;
  var
  %if &details %then msw msb wms ems edf bms bdf jms jdf k theta wsingle wk ;
  %if &nrater %then wnrater;
   sfsingle sfrandom sffixed 
	sfk sfrandk sffixedk;
run;
%end;

%mend intracc;
