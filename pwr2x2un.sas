/************************************************************************

                                PWR2x2un

   DISCLAIMER:
     THIS INFORMATION IS PROVIDED BY SAS INSTITUTE INC. AS A SERVICE TO
     ITS USERS.  IT IS PROVIDED "AS IS".  THERE ARE NO WARRANTIES,
     EXPRESSED OR IMPLIED, AS TO MERCHANTABILITY OR FITNESS FOR A
     PARTICULAR PURPOSE REGARDING THE ACCURACY OF THE MATERIALS OR CODE
     CONTAINED HEREIN.

   PURPOSE:
     Computes the power of a test comparing proportions from two,
     unequally-sized, independent samples.  This program is particularly 
     useful for assessing the power of a test that has already been
     performed on a table.
                                                                      
   REQUIRES:
     PWR2x2un requires only base SAS Software.

   USAGE:
     POWER2x2 is a DATA Step program.  At the beginning of the program
     is the following line:

          p=.4091;   level=.05;   diff=.75-.2143;   n1=14;   n2=8;

     Change the values on this line to suit your needs as follows:

          p=       Specifies an estimate of the overall "success" rate.
                 
          level=   Specifies the significance level or size of the test. 
                   This is the limit you place on the probability of a 
                   type 1 error.  It is a decimal value less that 1.  
                   For example, level=.05 sets the probability of a type
                   1 error at 0.05.

          diff=    Specifies the difference in the proportions that you 
                   want to detect.  This is the specification of the 
                   population proportion difference at which power is
                   computed.

          n1=      Specifies the sample size for one of the groups.

          n2=      Specifies the sample size for the other group.

     With the settings above, power and beta are computed for the test
     performed on the table shown in Example 3 in the FREQ chapter of
     the SAS Procedures Guide, Version 6, Third Edition or the SAS/STAT
     User's Guide, Version 6, Fourth Edition.  The Chi-square test in
     the Example shows a significant difference in Pr(B=1) for the A=1
     and A=2 groups (p=.014).  This program determines the power for
     detecting the observed difference (.75-.2143=.5357) given the
     sample sizes (14 and 8), the significance level of the test (0.05)
     and the observed overall success rate (.4091).

   PRINTED OUTPUT:
     Using the settings shown, the following output is generated:


                 Power for comparing two independent proportions   
               Overall p=0.4091; p1-p2=0.5357; test level=0.05

                        N1    N2     POWER       BETA

                        14     8    0.69091    0.30909


     The output shows that if the true difference in proportions is
     .5357 with an overall success rate of .4091, the power for
     detecting such a difference with sample sizes 14 and 8 is .69.  The
     type 2 error rate of the test is .31.

   DETAILS:
     Hypotheses in the test are:                                          

        H0: p1 = p2                                                       
        Ha: p1 ne p2                                                      

     where p1 and p2 are the success probabilities in the two
     populations.  The Pearson chi-square statistic tests the null
     hypothesis (H0) against the alternative hypothesis (Ha) and is
     available in the FREQ procedure when the CHISQ option is specified
     on the TABLES statement.
                                                                      
     The power is the probability of rejecting H0 and is a function of
     the true difference in proportions.  Power is often computed
     assuming many different settings of the true proportions.  The type
     2 error rate (denoted beta) is the probability of accepting H0 for
     some non-zero true difference and is equal to 1-power.  PWR2x2un
     computes power and beta for the sample sizes, significance level,
     and the true proportion differences that you specify.

   SEE ALSO:
     POWER2x2 -- Computes the power of a test comparing proportions from
                 two, equal-sized, independent samples.  Power is given
                 for various sizes of the total sample, allowing you to
                 pick the sample size that achieves the desired power.

     POWERRxC -- Computes power for Pearson and Likelihood Ratio
                 Chi-square tests of independence in FREQ.  Handles any 
                 number of rows and columns in a two-way table.

   REFERENCE:
     Agresti, A. (1990), Categorical Data Analysis, New York: John Wiley
     & Sons, Inc.

************************************************************************/


title;

data _power_;

  p=.4091;   level=.05;   diff=.75-.2143;   n1=14;   n2=8;

  /********************** Compute power ************************/
  /* Create macro variables for output labeling */
  call symput('p',trim(left(p)));
  call symput('level',trim(left(level)));
  call symput('diff',trim(left(diff)));

  /* Power and beta computation */
  invstd=1/sqrt(p*(1-p)/n1+p*(1-p)/n2);
  power=1-probnorm(-probit(level/2)-diff*invstd) +
          probnorm( probit(level/2)-diff*invstd);
  beta=1-power;
  run;

proc print noobs;
  var n1 n2 power beta;
  title  "Power for comparing two independent proportions";
  title2 "Overall p=&p; p1-p2=&diff; test level=&level";
  run;

title;

