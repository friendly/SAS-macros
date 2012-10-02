/************************************************************************

                                ROCPLOT
                                      

  LICENSE AGREEMENT FOR CORRECTIVE CODE OR ADDITIONAL FUNCTIONALITY
    SAS INSTITUTE INC. IS PROVIDING YOU WITH THE COMPUTER SOFTWARE CODE
    INCLUDED WITH THIS AGREEMENT ("CODE") ON AN "AS IS" BASIS, AND
    AUTHORIZES YOU TO USE THE CODE SUBJECT TO THE TERMS HEREOF. BY USING THE
    CODE, YOU AGREE TO THESE TERMS. YOUR USE OF THE CODE IS AT YOUR OWN
    RISK. SAS INSTITUTE INC. MAKES NO REPRESENTATION OR WARRANTY, EXPRESS OR
    IMPLIED, INCLUDING, BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE, NONINFRINGEMENT AND TITLE, WITH
    RESPECT TO THE CODE.

    The Code is intended to be used solely as part of a product ("Software")
    you currently have licensed from SAS Institute Inc. or one of its
    subsidiaries or authorized agents ("SAS"). The Code is designed to
    either correct an error in the Software or to add functionality to the
    Software, but has not necessarily been tested. Accordingly, SAS makes no
    representation or warranty that the Code will operate error-free. SAS is
    under no obligation to maintain or support the Code. 

    Neither SAS nor its licensors shall be liable to you or any third party
    for any general, special, direct, indirect, consequential, incidental or
    other damages whatsoever arising out of or related to your use or
    inability to use the Code, even if SAS has been advised of the
    possibility of such damages. 

    Except as otherwise provided above, the Code is governed by the same
    agreement that governs the Software. If you do not have an existing
    agreement with SAS governing the Software, you may not use the Code. 

    SAS and all other SAS Institute Inc. product or service names are
    registered trademarks or trademarks of SAS Institute Inc. in the USA and
    other countries. (r) indicates USA registration. Other brand and product
    names are registered trademarks or trademarks of their respective
    companies. 

  PURPOSE:
    Produce a plot showing the Receiver Operating Characteristic (ROC)
    curve associated with a fitted binary-response model.  This is a
    plot of the sensitivity against 1-specificity values associated with
    the observations' predicted event probabilities. 
                                                                      
  REQUIRES:
    SAS/STAT software, Release 6.12 or later, is needed to create the
    required input data sets from PROC LOGISTIC.  The macro requires only
    Base SAS software to generate low-resolution plots via PROC PLOT.  High-
    resolution plots require PROC GPLOT in SAS/GRAPH software.

  USAGE:
    You must first run the LOGISTIC procedure to fit the desired model 
    using the OUTROC= and ROCEPS=0 options in the PROC LOGISTIC statement
    and the OUT= and P= options in the OUTPUT statement.

    You must then define the ROCPLOT macro so that it is available for 
    use.  Simply submit this file to the SAS System to define the
    macro.  You may then invoke the macro.  The EXAMPLE section below
    illustrates how this is done using the %INCLUDE statement.

    The following parameters are required when using the macro:
    
       out=          Name of the OUT= data set from PROC LOGISTIC.

       outroc=       Name of the OUTROC= data set from PROC LOGISTIC.

       p=            Name of the variable specified in the P= option in
                     the OUTPUT statement of PROC LOGISTIC.  This
                     variable contains the predicted probabilities.

       id=           Names of the variables to be used to label the
                     points on the ROC curve.  Typically, some or all of
                     the predictor variables from the fitted model are
                     specified, but you may use any variables in PROC
                     LOGISTIC input data set or OUT= data set (such as
                     the predicted probabilities).  Any number of
                     variables may be specified, but note that the
                     labels may become too long to be fully displayed.
  
    The following are optional parameters for controlling the graphical
    appearance:

       plottype=     PLOTTYPE=HIGH (the default) produces a high-
                     resolution ROC plot using PROC GPLOT.
                     PLOTTYPE=LOW produces a low-resolution plot using
                     PROC PLOT.

       roffset=      Specifies the amount of space (in percent of graphic
                     area) between the rightmost major tick mark and the
                     right end of the horizontal axis.  Use roffset= to
                     add more space to allow the label of last points to
                     fully display.  Default: 4.

       font=         Font of label text. Default: swissb.

       size=         Height of the label text. Default: 2 (2% of the
                     graphics output area).

       color=        Color of the label text. Default: black.

       position=     Location of the label text. Default: below and to
                     the right.

       plotchar=     Symbol for points on the ROC curve. Default: dot.

    
  PRINTED OUTPUT:
    This is the low-resolution (PLOTTYPE=LOW) plot generated by the first
    Example which also appears in the LOGISTIC chapter of the SAS/STAT
    User's Guide.  Of course, a high-resolution plot will be more
    attractive and will also have a reference line from (0,0) to
    (1,1).

                          ROC plot for disease = age
                     Approximate area under curve = 0.953

           Plot of _SENSIT_*_1MSPEC_$_id.  Symbol points to label.

  1.0 |                 > 55               > 45                > 35       25 <
      |
      |
  0.9 |
      |
      |
  0.8 |
      |      > 65
      |
  0.7 |
S     |
e     |
n 0.6 |
s     |> 75
i     |
t 0.5 |
i     |
v     |
i 0.4 |
t     |
y     |
  0.3 |
      |
      |
  0.2 |
      |
      |
  0.1 |
      |
      |
  0.0 |
      -+------+------+------+------+------+------+------+------+------+------+
      0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9   1.0

                                   1 - Specificity


                        Point labels are values of age


  DETAILS:
    The receiver operating characteristic (ROC) curve is a diagnostic
    tool for assessing the ability of a logistic model to discriminate
    between events and nonevents.  If the model discriminates perfectly,
    the ROC curve passes through the point (0,1) and the area below the
    curve is one.  If the model has no discriminating ability, the curve
    is a line from (0,0) to (1,1).  Each point on the ROC curve provides
    the sensitivity and specificity measures associated with a cutpoint
    in the probability scale which allows classification of each
    observation as either a predicted event or a predicted nonevent.
   
    It is important to use the ROCEPS=0 option in the MODEL statement of
    PROC LOGISTIC when you fit your model because this option allows all the
    unique predicted values to be output to the OUTROC= data set. Otherwise,
    the values may be rounded yielding fewer points on the ROC plot.

    The _ROCPLOT data set generated by the macro contains all the
    information needed to produce the ROC plot. The _ID variable in this
    data set contains the label for the points on the plot.
  
  LIMITATIONS:
    Limited error checking is done.  Be sure that the specified P= and ID=
    variables exist in the OUT= data set.

    This message:

      ROCPLOT: Some predicted values in OUT= did not match predicted values
               in OUTROC=.  Verify that you used the ROCEPS=0 option in
               PROC LOGISTIC.

    may appear in the SAS Log if:

    1) there are multiple observations which have the same predicted value
    but different values of the ID= variable(s) and/or

    2) the list of distinct predicted probabilities in the OUTROC= data set
    is shortened because the ROCEPS=0 option was not used in PROC LOGISTIC.

    This message does not indicate a problem, but the message is issued to
    remind you to use the ROCEPS=0 option in case that was the cause.
     
    Note that observations with different settings of the ID= variables may
    have the same predicted probability, but only one of the settings labels
    the point on the ROC curve associated with that predicted probability.

    Also, two (or more) points on the ROC curve may have the same label
    because they have the same values of the ID= variables but different
    predicted probabilities. For example, this may happen if a subset of the
    predictors is specified in ID= to label the points, and points having
    the same values of the predictors in ID= differ on one or more of the
    predictors not in ID=.

  SEE ALSO:
    A SAS/IML program is available which performs nonparametric comparison
    of areas under correlated ROC curves.  This program is available at
    the following address:

       http://www.sas.com/techsup/download/stat/roc.html

  REFERENCES:
    DeLong, E.R., DeLong, D.M. and Clarke-Pearson, D.L. (1988),
      "Comparing the areas under two or more correlated receiver
      operating characteristic curves: A nonparametric approach,"
      Biometrics, 44, 837-845.

    Hanley, J.A. and McNeil, B.J. (1982), "The meaning and use of the
      area under a receiver operating characteristic (ROC) curve,"
      Radiology, 143, 29-36.

    Hanley, J.A. and McNeil, B.J. (1983), "A method of comparing the
      areas under receiver operating characteristic curves derived from
      the same cases," Radiology, 148, 839-843.

    Metz, C.E. (1978), "Basic principles of ROC analysis," Seminars in
      Nuclear Medicine, 8(4), 283-298.

  EXAMPLES:
    Following are two examples from the LOGISTIC chapter of the SAS/STAT
    User's Guide.  An additional example (available at
    http://www.sas.com/techsup/download/stat/ROCcomp.html) shows a
    comparative plot of several ROC curves and a test of the equality of
    the areas below the curves.
  
    EXAMPLE 1 -- Single Predictor; Plot with area under the ROC curve in
                 title

    data Data1;
       input disease n age;
       datalines;
     0 14 25
     0 20 35
     0 19 45
     7 18 55
     6 12 65
    17 17 75
    ;
    
    proc logistic data=data1;
       model disease/n=age / outroc=roc1 roceps=0;
       output out=outp p=phat;
       ods output association=assoc;
       run;

    * The approximate area under the ROC curve is the "c" statistic in the
       Associations table.  Capture this value in a macro variable for use
       in plot title.
    ;
    data _null_;
       set assoc;
       if label2='c' then call symput("area",cvalue2);
       run;

    * Define the ROCPLOT macro ;
    %include "<path to your copy of the ROCPLOT macro>";
  
    title "ROC plot for disease = age";   
    title2 "Approximate area under curve = &area";
    %rocplot(outroc = roc1,
                out = outp,
                  p = phat,
                 id = age)



    EXAMPLE 2 -- Multiple Predictors

    data Remission;
       input remiss cell smear infil li blast temp;
       datalines;
    1   .8   .83  .66  1.9  1.1     .996
    1   .9   .36  .32  1.4   .74    .992
    0   .8   .88  .7    .8   .176   .982
    0  1     .87  .87   .7  1.053   .986
    1   .9   .75  .68  1.3   .519   .98
    0  1     .65  .65   .6   .519   .982
    1   .95  .97  .92  1    1.23    .992
    0   .95  .87  .83  1.9  1.354  1.02
    0  1     .45  .45   .8   .322   .999
    0   .95  .36  .34   .5  0      1.038
    0   .85  .39  .33   .7   .279   .988
    0   .7   .76  .53  1.2   .146   .982
    0   .8   .46  .37   .4   .38   1.006
    0   .2   .39  .08   .8   .114   .99
    0  1     .9   .9   1.1  1.037   .99
    1  1     .84  .84  1.9  2.064  1.02
    0   .65  .42  .27   .5   .114  1.014
    0  1     .75  .75  1    1.322  1.004
    0   .5   .44  .22   .6   .114   .99
    1  1     .63  .63  1.1  1.072   .986
    0  1     .33  .33   .4   .176  1.01
    0   .9   .93  .84   .6  1.591  1.02
    1  1     .58  .58  1     .531  1.002
    0   .95  .32  .3   1.6   .886   .988
    1  1     .6   .6   1.7   .964   .99
    1  1     .69  .69   .9   .398   .986
    0  1     .73  .73   .7   .398   .986
    ;

    proc logistic data=Remission;
       model remiss=smear blast / outroc=roc1 roceps=0;
       output out=outp p=p;
       run;
   
    title  "ROC plot for remiss = smear";
    %rocplot(outroc = roc1,
                out = outp,
                  p = p,
                 id = smear blast,
            roffset = 8)


************************************************************************/


%macro rocplot ( outroc=, out=, p=, id=, plottype=high, font=swissb,
                 size=2, position=F, color=black, plotchar=dot,
                 roffset=4, round=1e-8 );
options nonotes;
%let nomatch=0;

/* Verify ID= is specified */
%if %quote(&id)= %then %do;
  %put ERROR: The ID= option is required.;
  %goto exit;
%end;

/* Verify P= is specified */
%if %quote(&p)= %then %do;
  %put ERROR: The P= option is required.;
  %goto exit;
%end;

/* Verify OUTROC= is specified and the data set exists */
%if %quote(&outroc) ne %then %do;
  %if %sysfunc(exist(&outroc)) ne 1 %then %do;
    %put ERROR: OUTROC= data set not found.;
    %goto exit;
  %end;
%end;
%else %do;
  %put ERROR: The OUTROC= option is required.;
  %goto exit;
%end;

/* Verify OUT= is specified and the data set exists */
%if %quote(&out) ne %then %do;
  %if %sysfunc(exist(&out)) ne 1 %then %do;
    %put ERROR: OUT= data set not found.;
    %goto exit;
  %end;
%end;
%else %do;
  %put ERROR: The OUT= option is required.;
  %goto exit;
%end;

data _outroc;
   set &outroc;
   _prob_=round(_prob_,&round);
   run;

data _out;
   set &out;
   _prob_=round(&p , &round);
   length _id $ 200;

   /* Create single label variable */
   _id=trim(left(%scan(&id,1)))
        %let i=2;
        %do %while (%scan(&id,&i) ne %str() );
   ||'/'||trim(left(%scan(&id,&i)))
          %let i=%eval(&i+1);
        %end;
   ;
   run;

proc sort data=_out nodupkey;
   by _prob_ _id;
   run;

proc sort data=_outroc nodupkey;
   by _prob_;
   run;

data _rocplot;
   _inout=0; _inroc=0;
   merge _outroc(in=_inroc) _out(in=_inout);
   by _prob_;
   if not(_inout and _inroc) then do;
     call symput('nomatch',1);
     delete;
   end;
   run;

%if &nomatch=1 %then %do;
  %put ROCPLOT: Some predicted values in OUT= did not match predicted values;
  %put %str(         in OUTROC=.  Verify that you used the ROCEPS=0 option in);
  %put %str(         PROC LOGISTIC.);
%end;

%if %upcase(%substr(&plottype,1,1))=L %then %do;
   footnote "Point labels are values of &id";
   proc plot data=_rocplot;
      plot _sensit_*_1mspec_ $ _id /
           haxis=0 to 1 by .1 vaxis=0 to 1 by .1;
      run; quit;
%end;

%if %upcase(%substr(&plottype,1,1))=H %then %do;
   data _anno;
      length function style color $ 8 position $ 1 text $ 200;
      retain function 'label' xsys ysys '2' hsys '3'
             size &size position "&position" style "&font"
             color "&color";
      set _rocplot(keep=_sensit_ _1mspec_ _id) end=eof;
      x=_1mspec_; y=_sensit_; text=trim(left(_id)); output;
      
      /* Draw (0,0) to (1,1) reference line */
      if eof then do;
        x=0; y=0; function='move'; output;
        x=1; y=1; function='draw'; line=1; hsys='1'; size=0.5; output;
      end;
      run;
      
   symbol1 i=join v=&plotchar c=blue l=1;
   footnote "Point labels are values of &id";
   axis1 offset=(1,&roffset)pct order=(0 to 1 by .1);
   proc gplot data=_rocplot;
      plot _sensit_*_1mspec_=1 / vaxis=0 to 1 by .1 cframe=ligr
                                 haxis=axis1 annotate=_anno;
      run;
      quit;
%end;

footnote;
%exit:
options notes;
%mend;

