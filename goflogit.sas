
 /****************************************************************/
 /*                                                              */
 /*    NAME: GOFLOGIT                                            */
 /*   TITLE: Global Goodness-of-Fit Tests in Logistic Regression */
 /* PRODUCT: IML, STAT, BASE                                     */
 /*    KEYS: Goodness-of-Fit, Logistic Regression, Sparse Data   */
 /*   PROCS: LOGISTIC, MACRO                                     */
 /* VERSION: 8.1                                                 */
 /*                                                              */
 /****************************************************************/

 /*-----------------------------------------------------------
 TITLE
 -----
 GOFLOGIT: A SAS/IML macro for computing goodness-of-fit tests in
           logistic regression


 BACKGROUND
 ----------
 It is known for long time that standard goodness-of-fit (GOF) tests
 in logistic regression (Pearson-Test and Deviance, both available
 through PROC LOGISTIC and PROC GENMOD) suffer heavily from sparse data
 if their significance is judged with the Chi-square distribution (McCullagh/Nelder, 1986).
 Sparse Data means, that there are many or continuous
 covariates in the logistic model, so that, in extreme cases, every observation
 has its own pattern of covariates, more the exception than the rule in
 today's data sets.
 
 To circumvent this problem, PROC LOGISTIC (LACKFIT-Option) offers
 an alternative goodness-of-fit-test, the so called Hosmer-Lemeshow-Test.
 But the HL-test is not the only alternative in the described
 situation and has certain deficiencies (Hosmer et al., 1997).
 Luckily, the statistical literature offers a variety of additional
 testing procedures for the given situation and the GOFLOGIT-Macro
 calculates five of them.  Simulation-based evidence shows, that
 these tests behave satisfactorily and in most situations give more
 reliable results than the Hosmer-Lemeshow-Test.


 DESCRIPTION
 -----------
 The GOFLOGIT-Macro is a kind of a stand-alone application for logistic regression
 since it also estimates the parameter estimates, standard errors and estimated
 probabilities (where the IML module from page  135-138 of SAS/IML-Software: Usage and Reference,
 Version 6, First Edition is used). So the macro should give identical results to PROC LOGISTIC.
 PROC LOGISTIC can be invoked on demand to get the Hosmer-Lemeshow-Test, but this is not necessary
 for calculating the implemented GOF-Tests.
 The following tests are calculated:

 1) PEARSON and DEVIANCE:
    The two standard tests. Please remember that these give reliable results only
    if there is a reasonable amount of observations for every specific pattern of covariates

 2) OSIUS (Osius/Rojek, 1992) and MCCULLAGH (McCULLAGH, 1985a, 1885b):
    Two tests that use the pearson statistic as a test statistic but calculate p-values
    from a normal distribution

 3) FARRING (Farrington, 1996):
    The Farrington-Test extends the usual Pearson statistic by an additive constant

 4) IM (White, 1982 and Orme, 1988)
    The information matrix test compares two estimators of the information matrix
    which should be equal under a correct specification of the model

 5) RSS (Copas, 1989 and Hosmer et al., 1997)
    The residual sum of squares test uses the denominator of the pearson statistic as
    a test statistic, p-values are calculated from a normal distribution

 LITERATURE
 ----------

 - Copas JB (1989): Unweighted Sum of Squares Test for Proportions. Appl Statist, 38, 71-80.
 - Farrington CP (1996): On Assessing Goodness of Fit of Generalized Linear Models to Sparse Data.
   J R Statist Soc B 58 (2), 349-360.
 - Hosmer DW, Lemeshow S (1980): Goodness of fit tests for the multiple logistic regression model.
   Commun Statist - Theor Meth 9 (10), 1043-1069.
 - Hosmer DW et al. (1997): A comparison of goodness-of-fit tests for the logistic regression model.
   Statistics in Medicine 16, 965-980.
 - McCullagh P (1985a): Sparse data and conditional tests. Bulletin of the International Statistics Institute,
   Proceedings of the 45th Session of ISI (Amsterdam), Invited Paper, 28, 1-10.
 - McCullagh P (1985b): On the Asymptotic Distribution of Pearson´s Statistic in Linear Exponential-Family Models.
   Int Stat Rev, 53, 61-67.
 - McCullagh P, Nelder JA (1986): Generalized Linear Models. Chapman & Hall.
 - Orme C (1988): The calculation of the information matrix test for binary data models.
   The Manchester School 54 (4), 370-376.
 - Osius G, Rojek D (1992): Normal Goodness-of-Fit Tests for Multinomial Models With Large Degrees of Freedom.
   JASA, 87, 1145-1152.
 - White H (1982): Maximum Likelihood Estimation of Misspecified Models. Econometrica, 50, 1-25.




 SYNTAX
 ------

 %goflogit(data=_last_,
           y=,
           trials=,
           xlist=,
           logistic=ON,
           work=2000
           syms=200
          )

 where

  data=    specifies the data set you are using.

  y=       specifies the variable that contains the number of
           observed events for each covariate pattern (successes).

  trials=  specifies the variable that contains the number of
           observed trials for each covariate pattern (treatments).

  xlist=   specifies the list of covariables in the model

  logistic= controls the optional running of PROC LOGISTIC (LOGISTIC
           (Default: LOGISTIC=ON)

  work=    specifies the worksize for IML (Default: WORK=2000)

  syms=    specifies the size of symbol space for IML (Default: SYMS=200)


  EXAMPLE
  -------
  An example is given below. It uses the dataset on low birthweight from
  the book of Hosmer and Lemeshow


  AUTHOR
  ------
  Comments, suggestions and error messages are very appreciated.
  Send them to:

  Oliver Kuss
  University of Halle-Wittenberg
  Institute of Medical Epidemiology, Biometry, and Informatics
  Magdeburger Str. 27
  06097 Halle/Saale
  Tel.: +49-345-5573582
  Fax:  +49-345-5573580
  e-mail: Oliver.Kuss@medizin.uni-halle.de

 -------------------------------------------------------------*/


%macro goflogit(data=_last_,
                y=,
                trials=,
                xlist=,
                logistic=OFF,
                work=2000,
                syms=200
          );


    %if &logistic=ON %then %do;
        proc logistic data=&data descending;
		%if %length(&trials)=0 %then %do;
			model &y         = &xlist /  aggregate lackfit scale=1;
			%end;
		%else %do;
			model &y/&trials = &xlist /  aggregate lackfit scale=1;
			%end;
        run;
    %end;


  proc iml symsize=&syms worksize=&work;

    use &data;
    read all var {&xlist} into xv;
    read all var {&y} into y;
   %if %length(&trials)=0 %then %do;
      m = 1;
      %end;
   %else %do;
    read all var {&trials} into m;
      %end;


    n=nrow(xv);                            /* N is the number of covariate patterns */
    design = repeat(1,n,1) || xv;          /* Generation of the design matrix */
    x = design//design;
    _y = repeat(1,n,1) // repeat(0,n,1);
    wgt = y      // (m-y);
    parm = {intercept &xlist}`;

  * Estimation of parameters and separation check;
    b = repeat(0,ncol(x),1); oldb=b+1; /* starting values */
    separat=0;
    do iter=1 to 20 until ((max(abs(b-oldb))<1e-8) | (separat=1)) ;
       oldb=b;
       p=1/(1+exp(-(x*b)));
       f=p#p#exp(-(x*b));
       do sep=1 to nrow(p);
          if p[sep,] < 0.000001 then do; separat=1; p[sep,] = 0.000001; end;
          if p[sep,] > 0.999999 then do; separat=1; p[sep,] = 0.999999; end;
       end;
       loglik =sum( ((_y=1)#log(p) + (_y=0)#log(1-p))#wgt);
       btransp = b`;
       w = wgt/(p#(1-p));
       xx = f # x;
       xpxi = inv(xx`*(w#xx));
       step = xpxi*(xx`*(w#(_y-p)));
       b = b + step;
    end;

    if separat=1 then do;
        print ,'***************************************************',
               '**              WARNING!!!!                      **',
               '** THE MACRO FOUND A SEPARATION IN YOUR DATASET. **',
               '** ML-ESTIMATORS DO NOT EXIST IN THIS CASE AND   **',
               '** AND ALL REPORTED RESULTS ARE NOT VALID !!!    **',
               '**        PLEASE CHECK YOUR DATA!!!              **',
               '***************************************************',;
    end;


  * Calculation of parameter tests;
  * Likelihood-Ratio-Test of no effect of all covariates;
    p0 = sum((_y=1)#wgt)/sum(wgt);  /* average response */
    loglik0 =sum( ((_y=1)#log(p0) + (_y=0)#log(1-p0))#wgt);
    chisq =  ( 2 # (loglik-loglik0));
    df    = ncol(x)-1;
    prob  = 1-probchi(chisq,df);

  * Wald-Tests for the single covariates;
    stderr = sqrt(vecdiag(xpxi));
    waldchi= (b/sqrt(vecdiag(xpxi)))##2;
    pwald  = 1 - probchi(waldchi,1);


    p2=p[1:nrow(p)/2,1];


  * Calculation of the various GOF-Tests;
  * Pearson-Test;
    pearsoni= ((y - m#p2)##2) / (m#p2#(1-p2));
    pearson= sum(((y - m#p2)##2) / (m#p2#(1-p2)));
    ppearson=1-probchi(pearson,nrow(p2)-ncol(x));

  * Deviance;
    devianci= (y          # log( (y          + (y=0))          / (m#p2   ))) +
              ((m-y) # log( ((m-y) + ((m-y)=0)) / (m#(1-p2))));
    deviance=2*sum(devianci);
    pdevianc=1-probchi(deviance,nrow(p2)-ncol(x));

  * Osius-Test;
    vosius  = sum(2 + (1/m)#(1/(p2#(1-p2))-6));
    qosius  = (design` * (1-2#p2))` * xpxi * (design` * (1-2#p2));
    varosius= vosius - qosius;
    tosius  = (pearson - nrow(p2)) / sqrt(varosius);
    posius  = 1-probnorm(tosius);

  * McCullagh-Test;
    mumccul = nrow(p2)-ncol(x);
    umccul  = (1 - 2#p2) / (m#p2#(1-p2));
    w       = diag(m#p2#(1-p2));
    rssmccul= umccul`*(w - w*design*xpxi*design`*w)*umccul;
    umccdach= design * inv(design`*w*design)*design`*w*umccul;
    k1      = m#p2;
    k2      = m#p2#(1-p2);
    k3      = m#p2#(1-p2)#(1-2#p2);
    k4      = m#(p2 - 7#p2##2 +  12#p2##3 -   6#p2##4);
    varlinpr= diag(design * inv(design` * w * design) * design`);
    ewmccul = mumccul - (0.5#sum(k4#(varlinpr#(1/k2)))) + (0.5#sum(umccdach#k3#varlinpr));
    varmccul= (1 - (ncol(x)/nrow(p2))) # (2#sum((m-1)/m) + rssmccul);
    zmccul  = (pearson - ewmccul)/sqrt(varmccul);
    pzmccul = 1-probnorm(zmccul);


  * Farrington-Test;
    farringi= ((1-2#p2)/(m#p2#(1-p2)))#(y - m#p2);
    farring = sum(pearsoni) - sum(farringi);

    ewfarr  = nrow(p2)-ncol(x) +
              sum( ((m#p2#(1-p2))##2)/((m##2)#p2#(1-p2))#
                   vecdiag(design * inv(design` * w * design) * design`));
    varfarr = 2#(1-(ncol(x)/nrow(p2)))#sum((m-1)/m);

    if varfarr = 0 then do;zfarr=0; pzfarr=1;end;

    if varfarr > 0 then do;
       zfarr   = (farring - ewfarr)/sqrt(varfarr);
       pzfarr  = 1-probnorm(zfarr);
    end;

  * IM-Test;
    do i=1 to nrow(m);
       himp2=repeat(p2[i,1],m[i,1],1);
       hdesigni=shape(design[i,],m[i,1],ncol(design[i,]));
       if y[i,1]=0 then do;himread=repeat(0,(m[i,1]),1);end;
       if y[i,1]=m[i,1] then do;himread=repeat(1,y[i,1],1);end;
       if (y[i,1]^=0) & (y[i,1]^=m[i,1]) then do;
          himread=repeat(1,y[i,1],1)//repeat(0,(m[i,1]-y[i,1]),1);
       end;
       imp2= imp2//himp2;
       imready=imready//himread;
       imdesign=imdesign//hdesigni;
    end;
    imtotal=repeat(1,nrow(imp2),1);
    m=nrow(imtotal);
    do i=1 to nrow(imtotal);
       zdiagi=vecdiag(imdesign[i,]`*imdesign[i,]);
       zdiagh=zdiagh||zdiagi;
    end;
    zdiag=zdiagh`;
    fim     = diag(sqrt(imp2#(1-imp2))#(1-2#imp2));
    dim     = diag(sqrt(imp2#(1-imp2)));
    rim     = (imready - imtotal#imp2) / sqrt(imtotal#imp2#(1-imp2));
    wsternid= dim*imdesign||fim*zdiag;

    im1d    = rim`*wsternid*ginv(wsternid`*wsternid)*wsternid`*rim;
    pim1d   = 1-probchi(im1d,ncol(zdiag));

  * RSS-Test;
    copas   = sum((imready - imtotal#imp2)##2);
    meacopas= sum(imtotal#imp2#(1-imp2));
    vcopas  = diag(imtotal#imp2#(1-imp2));
    varcopas= (1 - 2#imp2)` * (I(nrow(imp2)) - vcopas*imdesign*inv(imdesign`*vcopas*imdesign)*imdesign`) * vcopas *
              (1 - 2#imp2);
    scopas  = (copas-meacopas)/sqrt(varcopas);
    pcopas  =1-probchi(scopas##2,1);

    reset noname;


  * Preparation of results for output;
    infname1= "Response:               "//"Covariates:             ";
    infname2= "Number of observations:      "//"Number of covariate patterns:";
    INFO_OBS= m//n;
    INFO_VAR= "&y/&trials"//"&xlist";
    testname= "Standard Pearson Test"//"Standard Deviance"//"Osius-Test"//
              "McCullagh-Test"//"Farrington-Test"//"IM-Test"//"RSS-Test";
    cols    = "Value"//"p-Value";
    stats   = pearson//deviance//tosius//zmccul//zfarr//im1d//copas;
    pvalues = ppearson//pdevianc//posius//pzmccul//pzfarr//pim1d//pcopas;
    TEST    = stats||pvalues;


  * Print results;
    print "Global Goodness-of-Fit Tests in Logistic Regression",;
    print /*"Model Information (Observations):"*/
         INFO_VAR[rowname=infname1]
         INFO_OBS[rowname=infname2];

    reset name;
    print "Model Fit:",
          "Likelihood Ratio vs. Intercept-only Model" chisq df prob,
          "Parameter estimates and tests", parm b stderr waldchi pwald,;


    print "Results from the Goodness-of-Fit Tests"

    TEST[rowname=testname colname=cols format=10.3];


  quit;


%mend;


  *-------------------Birthweight Data------------------------*
   | Data for a study of Risk Factors Associated with Low      |
   | Infant Birth Weight. Data were collected at Baystata      |
   | Medical Center, Springfield, Massachusetts, during 1986.  |
   | Aus: Hosmer/Lemeshow: Applied Logistic Regression 247-252 |
   *-----------------------------------------------------------* ;

   data birthwgt;
        input age lwt race2 race3 smoke y nobs;
   cards;
   19 182 1  0  0  0 1
   33 155 0  1  0  0 1
   20 105 0  0  1  0 1
   21 108 0  0  1  0 1
   18 107 0  0  1  0 1
   21 124 0  1  0  0 1
   22 118 0  0  0  0 1
   17 103 0  1  0  0 1
   29 123 0  0  1  0 1
   26 113 0  0  1  0 1
   19  95 0  1  0  0 1
   19 150 0  1  0  0 1
   22  95 0  1  0  0 1
   30 107 0  1  0  0 1
   18 100 0  0  1  0 2
   15  98 1  0  0  0 1
   25 118 0  0  1  0 1
   20 120 0  1  0  0 2
   28 120 0  0  1  0 1
   32 121 0  1  0  0 1
   31 100 0  0  0  0 1
   36 202 0  0  0  0 1
   28 120 0  1  0  0 1
   25 120 0  1  0  0 1
   28 167 0  0  0  0 1
   17 122 0  0  1  0 1
   29 150 0  0  0  0 1
   26 168 1  0  1  0 1
   17 113 1  0  0  0 2
   24  90 0  0  1  0 1
   35 121 1  0  1  0 1
   25 155 0  0  0  0 1
   31 150 0  1  1  0 1
   23 115 0  1  1  0 1
   16 112 1  0  0  0 1
   16 135 0  0  1  0 2
   18 229 1  0  0  0 1
   25 140 0  0  0  0 1
   32 134 0  0  1  0 1
   20 121 1  0  1  0 1
   23 190 0  0  0  0 1
   22 131 0  0  0  0 1
   32 170 0  0  0  0 1
   30 110 0  1  0  0 1
   20 127 0  1  0  0 1
   23 123 0  1  0  0 1
   17 120 0  1  1  0 1
   19 105 0  1  0  0 1
   23 130 0  0  0  0 1
   36 175 0  0  0  0 1
   22 125 0  0  0  0 1
   24 133 0  0  0  0 1
   21 134 0  1  0  0 1
   19 235 0  0  1  0 1
   25  95 0  0  1  0 1
   29 135 0  0  0  0 1
   29 154 0  0  0  0 1
   19 147 0  0  1  0 2
   30 137 0  0  0  0 1
   24 110 0  0  0  0 1
   19 184 0  0  1  0 1
   24 110 0  1  0  0 2
   23 110 0  0  0  0 1
   25 241 1  0  0  0 1
   30 112 0  0  0  0 1
   22 169 0  0  0  0 1
   25 125 1  0  0  0 1
   29 140 0  0  1  0 1
   19 138 0  0  1  0 1
   27 124 0  0  1  0 1
   31 215 0  0  1  0 1
   33 109 0  0  1  0 1
   21 185 1  0  1  0 1
   19 189 0  0  0  0 1
   23 130 1  0  0  0 1
   21 160 0  0  0  0 1
   18  90 0  0  1  0 2
   32 132 0  0  0  0 1
   19 132 0  1  0  0 1
   24 115 0  0  0  0 1
   22  85 0  1  1  0 1
   22 120 0  0  0  0 1
   23 128 0  1  0  0 1
   30  95 0  0  1  0 1
   19 115 0  1  0  0 1
   16 110 0  1  0  0 1
   21 110 0  1  1  0 1
   30 153 0  1  0  0 1
   20 103 0  1  0  0 1
   17 119 0  1  0  0 2
   23 119 0  1  0  0 1
   28 140 0  0  0  0 1
   26 133 0  1  1  0 1
   20 169 0  1  0  0 1
   24 115 0  1  0  0 1
   28 250 0  1  1  0 1
   20 141 0  0  0  0 1
   22 158 1  0  0  0 1
   22 112 0  0  1  0 1
   20 150 0  0  1  1 1
   21 200 1  0  0  1 1
   24 155 0  0  1  1 1
   21 103 0  1  0  1 1
   20 125 0  1  0  1 1
   25  89 0  1  0  1 1
   19 102 0  0  0  1 1
   19 112 0  0  1  1 1
   26 117 0  0  1  1 1
   24 138 0  0  0  1 1
   17 130 0  1  1  1 1
   20 120 1  0  1  1 1
   27 130 1  0  0  1 1
   20  80 0  1  1  1 1
   17 110 0  0  1  1 1
   25 105 0  1  0  2 2
   20 109 0  1  0  1 1
   18 148 0  1  0  1 1
   18 110 1  0  1  1 1
   20 121 0  0  1  1 1
   21 100 0  1  0  1 1
   26  96 0  1  0  1 1
   31 102 0  0  1  1 1
   15 110 0  0  0  1 1
   23 187 1  0  1  1 1
   20 122 1  0  1  1 1
   24 105 1  0  1  1 1
   15 115 0  1  0  1 1
   23 120 0  1  0  1 1
   30 142 0  0  1  1 1
   22 130 0  0  1  2 3
   17 120 0  0  1  1 1
   23 110 0  0  1  1 1
   17 120 1  0  0  1 1
   26 154 0  1  0  1 1
   20 105 0  1  0  1 1
   18 120 0  0  1  0 1
   16 170 1  0  0  0 1
   32 186 0  0  0  0 1
   18 120 0  1  0  0 1
   29 130 0  0  1  0 1
   33 117 0  0  0  0 1
   20 170 0  0  1  0 1
   28 134 0  1  0  0 1
   14 135 0  0  0  0 1
   28 130 0  1  0  0 1
   25 120 0  0  0  0 1
   16  95 0  1  0  0 1
   20 158 0  0  0  0 1
   26 160 0  1  0  0 1
   21 115 0  0  0  0 1
   22 129 0  0  0  0 1
   25 130 0  0  0  0 1
   31 120 0  0  0  0 1
   35 170 0  0  0  0 1
   19 120 0  0  1  0 1
   24 116 0  0  0  0 1
   45 123 0  0  0  0 1
   28 120 0  1  1  1 1
   29 130 0  0  0  1 1
   34 187 1  0  1  1 1
   25  85 0  1  0  1 1
   27 150 0  1  0  1 1
   23  97 0  1  0  1 1
   24 128 1  0  0  1 1
   24 132 0  1  0  1 1
   21 165 0  0  1  1 1
   32 105 0  0  1  1 1
   19  91 0  0  1  1 1
   25 115 0  1  0  1 1
   16 130 0  1  0  1 1
   25  92 0  0  1  1 1
   26 190 0  0  1  1 1
   14 101 0  1  1  1 1
   28  95 0  0  1  1 1
   14 100 0  1  0  1 1
   23  94 0  1  1  1 1
   17 142 1  0  0  1 1
   ;run;


%goflogit(data=birthwgt,y=y, trials=nobs,
				xlist=age lwt race2 race3 smoke,
				logistic=OFF);

