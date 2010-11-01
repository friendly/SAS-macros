 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: stdize                                              */
 /*   TITLE: Macro for Standardizing Variables                   */
 /* PRODUCT: stat                                                */
 /*  SYSTEM: all                                                 */
 /*    KEYS: standardization, robust statistics, cluster         */
 /*    KEYS: analysis, macros                                    */
 /*   PROCS:                                                     */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT: saswss                      UPDATE:   2Mar95        */
 /*     REF:                                                     */
 /*    MISC: changed location measure for AGK from               */
 /*             median to mean to allow weights    2Mar95        */
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

 /************ change and uncomment the following statement to refer
               to the file containing XMACRO on your system ********/

*%inc '<location of SAS/STAT samples>xmacro.sas';
%include macros(xmacro);

 /********************************************************************

The XMACRO macros from the SAS/STAT sample library in 6.10 or later are
required.  No products other than base SAS software are required for
using the %STDIZE macro unless METHOD=AGK(p) or L(p) is specified, in
which case SAS/STAT software is required.

This macro will not work in 6.04 or earlier releases.

The %STDIZE macro standardizes one or more numeric variables in a SAS
data set by subtracting a location measure and dividing by a scale
measure. A variety of location and scale measures are provided,
including estimates that are resistant to outliers and clustering (see
the METHOD= argument). You can also multiply each standardized value by
a constant and add a constant. Thus the result is:

   result = add + multiply * (original - location) / scale

where:

   result   = final output value
   add      = constant to add (ADD= option)
   multiply = constant to multiply by (MULT= option)
   original = original input value
   location = location measure
   scale    = scale measure

Missing values can be replaced by the location measure or by any
specified constant (see the REPLACE option and MISSING= argument). You
can also suppress standardization if all you want to do is replace
missing values (see the REPONLY option).

If BY variables are specified, each BY group is standardized
separately.

The following arguments may be listed within parentheses in any order,
separated by commas:

   DATA=        SAS data set to be standardized. The default is _LAST_.
                Most data set options may be used.

   VAR=         List of numeric variables to be standardized.
                The usual forms of abbreviated lists
                (e.g., X1-X100, ABC--XYZ, ABC:) may be used.
                Variable names should not begin with an underscore.

                The default is all numeric variables not listed in the
                BY=, FREQ=, or WEIGHT= lists.

   FREQ=        A single numeric frequency variable used as in PROC
                UNIVARIATE.

   WEIGHT=      A single numeric weight variable used as in PROC
                UNIVARIATE. Only works for MEAN, SUM, EUCLEN, STD, AGK,
                and L(p).

   BY=          List of variables for BY groups. Abbreviated variable
                lists (e.g., X1-X100, ABC--XYZ, ABC:) may NOT be used.

   OUT=         The output data set, which is a copy of the DATA=
                data set except that the VAR= variables have been
                standardized. The default is _DATA_.

                Data set options may NOT be used with the OUT= data set.

   METHOD=      Method for computing location and scale measures:

      method     scale                       location
      ------     -----                       --------
      MEAN       1                           mean
      MEDIAN     1                           median
      SUM        sum                         0
      EUCLEN     Euclidean length            0
      USTD       standard dev. about origin  0
      STD        standard deviation          mean
      RANGE      range                       minimum
      MIDRANGE   range/2                     midrange
      MAXABS     maximum abs value           0
      IQR        interquartile range         median
      MAD        median abs dev from median  median
      ABW(c)     biweight A-estimate         biweight 1-step M-estimate
      AHUBER(c)  Huber A-estimate            Huber 1-step M-estimate
      AGK(p)     AGK estimate (ACECLUS)      mean
      SPACING(p) minimum spacing             mid minimum-spacing
      L(p)       L(p)                        L(p)
      IN(ds)     read from data set          read from data set

                The default is METHOD=STD.

                For METHOD=ABW(c) or METHOD=AHUBER(c), c is a positive
                numeric tuning constant (Iglewicz, 1983).

                For METHOD=AGK(p), p is a numeric constant giving the
                proportion of pairs to be used with METHOD=COUNT in
                the ACECLUS procedure (SAS Technical Report P-229).
                This is the noniterative univariate form of the
                estimator described by Art, Gnanadesikan, & Kettenring
                (1982).

                For METHOD=SPACING(p), p is a numeric constant giving
                the proportion of data to be contained in the spacing.
                A spacing is the absolute difference between two data
                values. The minimum spacing for a proportion p is the
                minimum absolute difference between two data values that
                contain a proportion p of the data between them.  The
                mid minimum-spacing is the mean of these two data
                values.

                For METHOD=L(p), p is a numeric constant greater than
                or equal to 1 specifying the power to which differences
                are to be raised in computing an L(p) or Minkowski
                metric.

                For METHOD=IN(ds), ds is the name of a SAS data set
                containing the location and scale measures. The names
                of the variables are specified by the LOCATION= and
                SCALE= arguments.

                For robust estimators, see Iglewicz (1983). MAD has the
                highest breakdown point (50%), but isn't very efficient.
                ABW and AHUBER provide a good comprise between
                breakdown and efficiency. L(p) location estimates are
                increasingly robust as p drops from 2 (least squares,
                i.e. mean) to 1 (least absolute value, i.e. median),
                but the L(p) scale estimates are not robust.

                SPACING is robust to both outliers and clustering
                (Jannsen, Marron, Veraverbeke, and Sarle, 1993) and is
                therefore a good choice for cluster analysis or
                nonparametric density estimation.  The mid minimum
                spacing estimates the mode for small p.  AGK is also
                robust to clustering and more efficient than SPACING,
                but is not as robust to outliers and takes longer to
                compute. If you expect g clusters, the argument to
                SPACING or AGK should be 1/g or less. AGK is less biased
                than SPACING in small samples. It would generally be
                reasonable to use AGK for samples of size 100 or less
                and SPACING for samples of size 1000 or more, with the
                treatment of intermediate sample sizes depending on the
                available computer resources.

   FUZZ=        Relative fuzz factor. Default is 1E-14.
                If abs(score) < scale * fuzz then score = 0;
                If abs(location) < scale * fuzz then location = 0;
                If scale < abs(location) * fuzz then scale = 0;

   LOCATION=    List of numeric variables containing location measures
                in the data set specified by METHOD=IN(ds).
                The usual forms of abbreviated lists
                (e.g., X1-X100, ABC--XYZ, ABC:) may be used.
                Variable names should not begin with an underscore.

   SCALE=       List of numeric variables containing scale measures
                in the data set specified by METHOD=IN(ds).
                The usual forms of abbreviated lists
                (e.g., X1-X100, ABC--XYZ, ABC:) may be used.
                Variable names should not begin with an underscore.

   INITIAL=     Method for computing initial estimates for A estimates.
                The default is MAD.

   VARDEF=      See PROC UNIVARIATE.

   PCTLDEF=     See PROC UNIVARIATE.

   MULT=        Constant to multiply each value by after standardizing.
                The default is 1.

   ADD=         Constant to add to each value after standardizing
                and multiplying by MULT=. The default is 0.

   MISSING=     Method or a numeric value for replacing missing values.
                Use MISSING= when you want to replace missing values by
                something other than the location measure associated
                with the METHOD= argument, which is what the REPLACE
                option replaces them by. The usual methods include MEAN,
                MEDIAN, and MIDRANGE.  Any of the values for the METHOD=
                argument can also be specified for MISSING=, and the
                corresponding location measure will be used to replace
                missing values. If a numeric value is given, it replaces
                missing values after standardizing the data. However,
                the REPONLY option can be used together with MISSING= to
                suppress standardization in case you only want to
                replace missing values.

   OPTIONS=     List of additional options separated by blanks:

                PRINT          Print the standardized variables.

                PSTAT          Print the location and scale measures.

                NOMISS         Omit observations with any missing
                               values among the VAR= variables from
                               computation of the location and scale
                               measures. Otherwise, all nonmissing
                               values are used.

                NORM           Normalize the scale estimator to be
                               consistent for the standard deviation
                               of a normal distribution.

                               ONLY works for: AGK IQR MAD SPACING

                SNORM          Normalize the scale estimator to have
                               an expectation of approximately 1 for
                               a standard normal distribution.

                               ONLY works for: AGK IQR MAD SPACING

                REPLACE        Replace missing data by zero in the
                               standardized data (which corresponds
                               to the location measure before
                               standardizing). To replace missing
                               data by something else, see the
                               MISSING= argument.

                REPONLY        Replace missing data by the location
                               measure and do _not_ standardize the
                               data. You may not specify both REPLACE
                               and REPONLY.

The location and scale measures are stored in a data set called _STAT_.

The following statements may be useful for diagnosing errors:

   %let _notes_=1;       %* Prints SAS notes for all steps;
   %let _echo_=1;        %* Prints the arguments to the STDIZE macro;
   %let _echo_=2;        %* Prints the arguments to the STDIZE macro
                            after most defaults have been set;
   options mprint;       %* Prints SAS code generated by the macro
                            language;
   options mlogic symbolgen; %* Prints lots of macro debugging info;

This macro normally spends a lot of time checking the arguments you
specify for validity, in hopes of avoiding mysterious error messages
from the generated SAS code.  You can reduce the amount of time spent
checking arguments (and thereby speed up the macro at the risk of
getting inscrutable error messages if you make a mistake) by using one
of the following statements before invoking the macro:

   %let _check_=1; %* reduce argument checking;
   %let _check_=0; %* suppress argument checking--use at your own risk!;

References:

   Art, D., Gnanadesikan, R., and Kettenring, R. (1982), "Data-based
   Metrics for Cluster Analysis," Utilitas Mathematica, 21A, 75-99.

   Iglewicz, B. (1983), "Robust scale estimators and confidence
   intervals for location", in Hoaglin, D.C., Mosteller, M. and
   Tukey, J.W., eds., _Understanding Robust and Exploratory Data
   Analysis_, New York: Wiley.

   Jannsen, P., Marron, J.S., Veraverbeke, N, and Sarle, W.S. (1995),
   "Scale measures for bandwidth selection", J. of Nonparametric
   Statistics, 5, 359-380.

 *********************************************************************/

%* checks whether 6.10 version of xmacro has been included;
%macro xmacinc;
   %global _xmacro_;
   %if %bquote(&_xmacro_)= %then
      %put ERROR: XMACRO has NOT been included.;
   %else %if %bquote(&_xmacro_)=done %then %do;
      %let _xmacro_=;
      %put %qcmpres(ERROR: The 6.09 version of XMACRO has been
           included but the 6.10 or later version is required.);
   %end;
   %if %bquote(&_xmacro_)= %then %do;
      %put NOTE: The STDIZE macro will not be compiled.;
      %* comment out the rest of this file;
      %unquote(%str(/)%str(*))
   %end;
%mend xmacinc;

%xmacinc;

*===================== BEGIN STDIZE MACRO ==========================;

%unquote(%xmain(
   stdize(data=,var=,by=,freq=,weight=,
          method=,fuzz=,location=,scale=,initial=,missing=,
          vardef=,pctldef=,add=,mult=,
          out=,options=)
   ))

************** check arguments ************;
%xchkdata(data,_LAST_)
%xchklist(var,_NUMERIC_)
%xchklist(by)
%xchkname(freq)
%xchkname(weight)
%xchkdef(method,STD)
%xchknum(fuzz,1e-14,0<=FUZZ and FUZZ<1)
%xchklist(location)
%xchklist(scale)
%xchkdef(initial,MAD)
%xchkdef(missing)
%xchkkey(vardef,DF,N DF WDF WEIGHT WGT:WEIGHT)
%xchkint(pctldef,5,1,5)
%xchknum(add)
%xchknum(mult)
%xchkdsn(out,_DATA_)
%xchkeq(options)

************** process options ************;
%local print pstat pmiss nomiss norm snorm replace reponly;
%let print=0;
%let pstat=0;
%let pmiss=0;
%let nomiss=0;
%let norm=0;
%let snorm=0;
%let replace=0;
%let reponly=0;

%local n token;
%let n=1;
%let token=%scan(&options,&n,%str( ));
%do %while(%bquote(&token)^=);
   %let token=%upcase(&token);
   %if %xsubstr(&token,1,5)=PRINT %then %let print=1; %else
   %if %xsubstr(&token,1,5)=PSTAT %then %let pstat=1; %else
   %if %xsubstr(&token,1,5)=PMISS %then %let pmiss=1; %else
   %if %xsubstr(&token,1,6)=NOMISS %then %let nomiss=1; %else
   %if %xsubstr(&token,1,4)=NORM %then %let norm=1; %else
   %if %xsubstr(&token,1,5)=SNORM %then %let snorm=1; %else
   %if %xsubstr(&token,1,7)=REPLACE %then %let replace=1; %else
   %if %xsubstr(&token,1,7)=REPONLY %then %let reponly=1; %else
   %do;
      %let _xrc_=Unrecognized option &token;
      %put ERROR: &_xrc_..;
   %end;
   %let n=%eval(&n+1);
   %let token=%scan(&options,&n,%str( ));
%end;

%if &snorm %then %let norm=1;

%xbug(Options:,print pstat pmiss nomiss norm snorm replace reponly)

%if &_xrc_^=OK %then %goto exit;

************** process BY variables ************;
%xbylist;
%xvlist(data=&data,_list=by,_name=by,valid=0123)

************** dummy data step to check for errors ***************;
%xchkvar(&data,&by,&var &freq &weight)
%if &_xrc_^=OK %then %goto exit;

************** process FREQ= variable ***************;
%xvfreq(&data)

************** process WEIGHT= variable ***************;
%xvweight(&data)

************** process VAR= list ***************;
%local remove;
%let remove=;
%let var=%upcase(&var);
%xbug(,var)
%if %qupcase(&var)=_NUMERIC_ %then %do;
   %if %bquote(&by)^= %then %let remove=&remove by;
   %if %bquote(&freq)^= %then %let remove=&remove freq;
   %if %bquote(&weight)^= %then %let remove=&remove weight;
%end;
%xbug(,remove)

%xvlist(data=&data,_list=var,_name=var,_type=type,_count=nvar,
        remove=&remove,replace=1)
%xbug(,nvar var)

%if &_xrc_^=OK %then %goto exit;

%xchkend(&data);
%if &_xrc_^=OK %then %goto exit;

************** standardization ***************;

%xstdize(data=&data,method=&method,var=&var,
   replace=&replace,reponly=&reponly,missing=&missing,
   fuzz=&fuzz,initial=&initial,vardef=&vardef,pctldef=&pctldef,
   add=&add,mult=&mult,
   pstat=&pstat,pmiss=&pmiss,norm=&norm,snorm=&snorm,nomiss=&nomiss,
   _out=out)

%if &_xrc_^=OK %then %goto exit;
%if %bquote(&out)=_NULL_ %then %goto exit;

************** printing ***************;

%if &print %then %do;
   proc print data=&out uniform;
      var &var;
      by &by;
%end;

************** termination ***************;

%exit:

%xterm;

%mend stdize;


%********** INTERNAL ROUTINE TO DO ACTUAL STANDARDIZATION ************;
%* expects xbylist, xvfreq, and xvweight to have been called;

%macro xstdize(data=,method=,var=,replace=0,reponly=0,missing=,
   fuzz=1e-14,initial=MAD,vardef=DF,pctldef=5,add=,mult=,
   pstat=0,pmiss=0,norm=0,snorm=0,nomiss=0,
   _out=out); %* _out is updated in case the value is _DATA_;

%if &_echo_>=10 %then %do;
   %xput(Arguments to XSTDIZE macro:,
      data method var replace reponly missing
      fuzz initial vardef pctldef add mult
      pstat pmiss norm snorm _out, %str(   ));
   %xput(Other variables used by XSTDIZE:,
      freq fqopt fqstmt weight wtopt wtstmt
      by _bylast _bydata _byvars
      location scale, %str(   ));
%end;

%local instat; %let instat=_STAT_;

%if &replace & &reponly %then %do;
   %let _xrc_=OPTIONS=REPLACE and REPONLY may not both be specified;
   %put ERROR: &_xrc_..;
%end;

%local nostd misscon missmeth;
%let nostd=0;
%let misscon=;
%let missmeth=;
%if &reponly %then %do;
   %let nostd=1;
   %let replace=1;
   %if %qupcase(&method)=STD %then %let missmeth=MEAN;
%end;
%if %bquote(&missing)^= %then %do;
   %if %bquote(&method)= %then %let nostd=1;
   %if %datatyp(&missing)=CHAR %then %let missmeth=&missing;
   %else %do;
      %let misscon=&missing;
      %let missmeth=;
   %end;
   %let replace=1;
%end;
%xbug(Missing value replacement:,nostd misscon missmeth)

%local estbit missbit;
%let estbit=%eval(^&nostd | %bquote(&missing)=);
%let missbit=%eval(%bquote(&missmeth)^=);

************** use only numeric variables ***************;

%xvlist(data=&data,_list=var,_name=__v,_type=__t,_count=__nvar)
%xbug(xstdize-xvlist:, __nvar)

%local i n var nvar;
%let nvar=&__nvar;
%let n=0;
%do i=1 %to &nvar;
   %if &&__t&i=1 %then %do;
      %let n=%eval(&n+1);
      %local var&n type&n;
      %let var&n=&&__v&i;
      %let type&n=&&__t&i;
   %end;
%end;
%let var=%xconcat(var,&n);
%if &n^=&nvar %then %do;
   %let i=character variable(s) excluded from standardization;
   %put NOTE: %eval(&nvar-&n) &i.;
   %let nvar=&n;
%end;
%xbug(,nvar var)
%if &nvar=0 %then %do;
   data _STAT_; run;
   %goto exit;
%end;

************** nomiss ***************;

%if &nomiss %then %do;
   %local realdata;
   %let nomiss=0;
   data _null_; set &data;
      _m=nmiss(of &var);
      if _m>0 & _m<&nvar then do;
         call symput('nomiss','1');
         stop;
      end;
   run;
   %xbug(,nomiss)
   %if &syserr>4 %then %goto fail;
   %if &nomiss %then %do;
      data _NOMISS; set &data;
         _m=nmiss(of &var);
         if _m>0 & _m<&nvar then delete;
      run;
      %xbugdo(
         proc print data=_NOMISS; run;
      )
      %if &syserr>4 %then %goto fail;
      %let realdata=&data;
      %let data=_NOMISS;
   %end;
%end;

************** estimate location and scale ***************;

%if &estbit %then %do;
   %xstdest(method=&method,outstat=&instat);
   %if &_xrc_^=OK %then %goto error;

   %if &pstat %then %do;
      proc print data=&instat uniform;
         var %do i=1 %to &nvar; &&loc&i %end;
             %do i=1 %to &nvar; &&sca&i %end;
             %if &norm %then %do i=1 %to &nvar; &&nor&i %end;
            ;
      run;
   %end;
%end;

************** get missing value estimates ***************;

%if &missbit %then %do;
   %local mstat;
   %if &estbit %then %let mstat=_MISS_;
               %else %let mstat=&instat;
   %xstdest(method=&missmeth,loc=_mis,sca=_jnk,outstat=&mstat);
   %if &_xrc_^=OK %then %goto error;
   %do i=1 %to &nvar;
      %let mis&i=_mis&i;
      %let loc&i=_loc&i;
      %let sca&i=_sca&i;
   %end;

   %if &pmiss %then %do;
      proc print data=&mstat uniform;
         var %do i=1 %to &nvar; &&mis&i %end;
            ;
      run;
   %end;

   %if &mstat^=&instat %then %do;
      data &instat;
         %xmerge(&instat(firstobs=1 obs=2000000000),
                 _MISS_(firstobs=1 obs=2000000000))
      run;
      %if &syserr>4 %then %goto fail;
   %end;
%end;

************** nomiss again ***************;

%if &nomiss %then %do;
   %xdelete(&data)
   %let data=&realdata;
%end;

************** shift and scale data ***************;

%if %bquote(&&&_out)=_NULL_ %then %goto exit;

%xnotes(1)

   data &&&_out;
      %if &estbit | &missbit %then %do;
         %if &estbit %then %do i=1 %to &nvar;
            %if &norm %then %let use&i=&&nor&i;
                      %else %let use&i=&&sca&i;
         %end;
         %xmerge(&data,&instat(firstobs=1 obs=2000000000));
         drop
            %do i=1 %to &nvar;
               %if &estbit %then &&loc&i &&sca&i;
               %if &norm %then &&nor&i;
               %if &missbit %then &&mis&i _jnk&i;
            %end;
            ;
         retain
            %do i=1 %to &nvar;
               %if &estbit %then &&loc&i &&use&i;
               %if &missbit %then &&mis&i;
            %end;
            ;
      %end;
      %else %do;
         set &data;
      %end;
      %if &_putall_ %then %do;
         put _all_;
      %end;
      %if &estbit %then %do;
         %if %bquote(&by)= %then %do;
            if _n_=1 then do;
         %end;
         %else %do;
            if first.&_bylast then do;
         %end;
         %do i=1 %to &nvar;
               if &&use&i<=0 then &&use&i=1;
         %end;
            end;
      %end;
      %do i=1 %to &nvar;
         %if &missbit %then %do;
            if nmiss(&&var&i) then &&var&i=&&mis&i;
         %end;
         %else %do;
            if nmiss(&&var&i)=0 then do;
         %end;
         %if ^&nostd %then %do;
            &&var&i=(&&var&i-&&loc&i)/&&use&i;
            %if %bquote(&fuzz)^=0 %then %do;
               if abs(&&var&i)<=&&use&i*&fuzz then &&var&i=0;
            %end;
         %end;
         %if ^&missbit %then %do;
            end;
            %if &replace %then %do;
               %if %bquote(&misscon)^= %then %do;
                  else &&var&i=&misscon;
               %end;
               %else %if ^&nostd %then %do;
                  else &&var&i=0;
               %end;
               %else %do;
                  else &&var&i=&&loc&i;
               %end;
            %end;
         %end;
         %if %bquote(&mult)^= | %bquote(&add)^= %then %do;
            %if ^&replace %then %do;
               if nmiss(&&var&i)=0 then do;
            %end;
            %if %bquote(&mult)^= %then %do;
               &&var&i=&&var&i*&mult;
            %end;
            %if %bquote(&add)^= %then %do;
               &&var&i=&&var&i+&add;
            %end;
            %if ^&replace %then %do;
               end;
            %end;
         %end;
      %end;
      %if &_putall_ %then %do;
         put _all_;
      %end;
   run;
   %let &_out=&syslast;
   %if &syserr>4 %then %goto fail;

   %****** fuzz the estimates;
   %if &estbit %then %if %bquote(&fuzz)^=0 %then %do;
      %xnotes(0)
      data &instat;
         set &instat(firstobs=1 obs=2000000000);
         %do i=1 %to &nvar;
            if &&sca&i<=abs(&&loc&i)*&fuzz then do;
               &&sca&i=0;
               %if &norm %then %do;
                  &&nor&i=0;
               %end;
            end;
            else if abs(&&loc&i)<=&&use&i*&fuzz then &&loc&i=0;
         %end;
      run;
      %if &syserr>4 %then %goto fail;
      options _last_=&_out;
      %xnotes(1)
   %end;

%goto exit;

%fail:
   %let _xrc_=%bquote(Error while standardizing variables);

%error:
   %put ERROR: &_xrc_..;

%exit:

%mend xstdize;

**************************** xstdest ***************************;

%macro xstdest(method=,loc=_loc,sca=_sca,outstat=_STAT_);
%let _xrc_=XSTDEST failed;
%if &wtopt %then %let wtopt=1;
%if &snorm %then %let snorm=1;
%else %if &norm %then %let norm=1;

%if &_echo_>=10 %then %do;
   %xput(Arguments to the XSTDEST macro:,
         method loc sca outstat wtopt snorm norm);
   %xput(Other variables used by XSTDEST:,
         wtopt snorm norm
%end;

************ define location and scale variables *************;

%do i=1 %to &nvar;
   %global loc&i sca&i;
   %let loc&i=&loc&i;
   %let sca&i=&sca&i;
   %if &norm %then %do;
      %global nor&i;
      %let nor&i=_nor&i;
   %end;
%end;

************** run estimation method ***************;

%* avoid conflict with DISTANCE macro;
%let method=%upcase(&method);
%if %scan(&method,1)=L %then %let method=STD_&method;
%let initial=%upcase(&initial);
%if %scan(&initial,1)=L %then %let initial=STD_&initial;

************** invoke method= macro ***************;
%let _xrc_=%qcmpres(No macro found to implement METHOD=&method);
%let method=%unquote(&method);
%&method
%if &_xrc_^=STD_OK %then %put ERROR: &_xrc_..;
%else %let _xrc_=OK;

%if &wtopt=1 %then %stdmsg(WEIGHT,&method);
%if &snorm=1 %then %stdmsg(SNORM,&method);
%else %if &norm=1 %then %stdmsg(NORM,&method);

%mend xstdest;


%************** METHODS FOR LOCATION & SCALE ESTIMATION **************;

%macro mean;
   %let _xrc_=STD_OK;
   %if &wtopt %then %let wtopt=2;
   proc summary data=&data nway;
      output out=&outstat(drop=_type_ _freq_)
         mean=
            %do i=1 %to &nvar;
               &&loc&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
   %else %do;
   data &outstat;
       set &outstat(firstobs=1 obs=2000000000);
       %do i=1 %to &nvar;
          &&sca&i=1;
       %end;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend mean;

%macro median;
   %let _xrc_=STD_OK;
   proc univariate data=&data pctldef=&pctldef noprint;
      output out=&outstat
         median=
            %do i=1 %to &nvar;
               &&loc&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC UNIVARIATE failed);
   %else %do;
   data &outstat;
       set &outstat(firstobs=1 obs=2000000000);
       %do i=1 %to &nvar;
          &&sca&i=1;
       %end;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend median;

%macro sum;
   %let _xrc_=STD_OK;
   %if &wtopt %then %let wtopt=2;
   proc summary data=&data nway;
      output out=&outstat(drop=_type_ _freq_)
         sum=
            %do i=1 %to &nvar;
               &&sca&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
   %else %do;
   data &outstat;
       set &outstat(firstobs=1 obs=2000000000);
       %do i=1 %to &nvar;
          &&loc&i=0;
       %end;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend sum;

%macro euclen;
   %let _xrc_=STD_OK;
   %if &wtopt %then %let wtopt=2;
   proc summary data=&data nway;
      output out=&outstat(drop=_type_ _freq_)
         uss=
            %do i=1 %to &nvar;
               &&sca&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
   %else %do;
   data &outstat;
       set &outstat(firstobs=1 obs=2000000000);
       %do i=1 %to &nvar;
          &&loc&i=0;
          &&sca&i=sqrt(&&sca&i);
       %end;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend euclen;

%macro ustd;
   %let _xrc_=STD_OK;
   %if &wtopt %then %let wtopt=2;
   proc summary data=&data nway;
      output out=&outstat(drop=_type_ _freq_)
         %if &vardef=WEIGHT | &vardef=WDF %then sumwgt=;
                                          %else n=;
            %xconcat(loc,&nvar)
         uss=%xconcat(sca,&nvar)
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
   %else %do;
   data &outstat;
       set &outstat(firstobs=1 obs=2000000000);
       %do i=1 %to &nvar;
          %if &vardef^=N & &vardef^=WEIGHT %then %do;
             &&loc&i=max(0,&&loc&i-1);
          %end;
          &&sca&i=sqrt(&&sca&i/&&loc&i);
          &&loc&i=0;
       %end;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend ustd;

%macro std;
   %let _xrc_=STD_OK;
   %if &wtopt %then %let wtopt=2;
   proc summary data=&data nway vardef=&vardef;
      output out=&outstat(drop=_type_ _freq_)
         mean=
            %do i=1 %to &nvar;
               &&loc&i
            %end;
         std=
            %do i=1 %to &nvar;
               &&sca&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
%mend std;

%macro range;
   %let _xrc_=STD_OK;
   %if &norm %then %stdmsg(NORM,RANGE yet);
   proc summary data=&data nway;
      output out=&outstat(drop=_type_ _freq_)
         min=
            %do i=1 %to &nvar;
               &&loc&i
            %end;
         range=
            %do i=1 %to &nvar;
               &&sca&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
%mend range;

%macro midrange;
   %let _xrc_=STD_OK;
   %if &norm %then %stdmsg(NORM,MIDRANGE yet);
   proc summary data=&data nway;
      output out=&outstat(drop=_type_ _freq_)
         max=
            %do i=1 %to &nvar;
               _max&i
            %end;
         min=
            %do i=1 %to &nvar;
               _min&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
   %else %do;
   data &outstat;
       set &outstat(firstobs=1 obs=2000000000);
       drop
          %do i=1 %to &nvar;
             _max&i _min&i
          %end;
          ;
       %do i=1 %to &nvar;
          &&loc&i=(_max&i+_min&i)/2;
          &&sca&i=(_max&i-_min&i)/2;
       %end;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend midrange;

%macro maxabs;
   %let _xrc_=STD_OK;
   proc summary data=&data nway;
      output out=&outstat(drop=_type_ _freq_)
         max=
            %do i=1 %to &nvar;
               _max&i
            %end;
         min=
            %do i=1 %to &nvar;
               _min&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC SUMMARY failed);
   %else %do;
   data &outstat;
       set &outstat(firstobs=1 obs=2000000000);
       drop
          %do i=1 %to &nvar;
             _max&i _min&i
          %end;
          ;
       %do i=1 %to &nvar;
          &&loc&i=0;
          &&sca&i=max(_max&i,-_min&i);
       %end;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend maxabs;

%macro iqr;
   %let _xrc_=STD_OK;
   proc univariate data=&data pctldef=&pctldef noprint;
      output out=&outstat
         median=
            %do i=1 %to &nvar;
               &&loc&i
            %end;
         qrange=
            %do i=1 %to &nvar;
               &&sca&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %let _xrc_=%qcmpres(PROC UNIVARIATE failed);
   %if &norm %then %do;
      %let norm=2;
      data &outstat;
         set &outstat(firstobs=1 obs=2000000000);
         retain _norm; drop _norm;
         if _n_=1 then _norm=2*probit(.75);
         %do i=1 %to &nvar;
            &&nor&i=&&sca&i/_norm;
         %end;
      run;
      %if &syserr>4 %then %let _xrc_=%qcmpres(DATA step failed);
   %end;
%mend iqr;

%macro mad;
   %let _xrc_=STD_OK;
   proc univariate data=&data pctldef=&pctldef noprint;
      output out=&outstat
         median=
            %do i=1 %to &nvar;
               &&loc&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %goto fail;
   data _TEMP;
      %xmerge(&data,&outstat(firstobs=1 obs=2000000000))
      %if &_putall_ %then %do;
         put _all_;
      %end;
      %do i=1 %to &nvar;
         &&var&i=abs(&&var&i-&&loc&i);
      %end;
   run;
   %if &syserr>4 %then %goto fail;
   proc univariate data=_TEMP(firstobs=1 obs=2000000000)
        pctldef=&pctldef noprint;
      output out=_STAT2
         median=
            %do i=1 %to &nvar;
               &&sca&i
            %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %goto fail;
   data &outstat;
      merge &outstat(firstobs=1 obs=2000000000)
            _STAT2(firstobs=1 obs=2000000000);
      %if %bquote(&by)= %then %do;
         by &_bydata;
      %end;
      %if &norm %then %do;
         %let norm=2;
         retain _norm; drop _norm;
         if _n_=1 then _norm=probit(.75);
         %do i=1 %to &nvar;
            &&nor&i=&&sca&i/_norm;
         %end;
      %end;
   run;
   %if &syserr>4 %then %goto fail;
   %goto exit;
%fail:
   %let _xrc_=%qcmpres(PROC SUMMARY or DATA step failed);
%exit:
%mend mad;

%macro agk(p);
   %let _xrc_=STD_OK;
   %if &wtopt %then %let wtopt=2;
   proc summary data=&data nway vardef=&vardef;
      output out=&outstat(drop=_type_ _freq_)
         mean=
            %do i=1 %to &nvar;
               &&loc&i
            %end;
         %if &norm %then %do;
            n=
               %do i=1 %to &nvar;
                  _n&i
               %end;
         %end;
         ;
      var &var;
      &fqstmt
      &wtstmt
      by &by;
   run;
   %if &syserr>4 %then %goto fail;
   %do i=1 %to &nvar;
      proc aceclus data=&data method=count p=&p noprint outstat=_STAT2;
         var &&var&i;
         &fqstmt
         &wtstmt
         by &by;
      run;
      %if &syserr>4 %then %goto fail;
      %xbugdo(
         proc print data=_STAT2; run;
      )
      data _STAT2;
         set _STAT2(firstobs=1 obs=2000000000);
         keep &&sca&i
         %if %bquote(&by)= %then %do;
            &_byvars
         %end;
         ;
         if _type_='ACE';
         &&sca&i=sqrt(&&var&i);
      run;
      %if &syserr>4 %then %goto fail;
      data &outstat;
         merge &outstat(firstobs=1 obs=2000000000)
               _STAT2(firstobs=1 obs=2000000000);
         %if %bquote(&by)= %then %do;
            by &_bydata;
         %end;
         %if &norm %then %do;
            %let norm=2;
            drop _t _p _norm _width _avg;
            _t=_n&i*(_n&i-1)/2;
            _p=floor(&p*_t+1e-10)/_t;
            _width=sqrt(2)*probit(.5+.5*_p);
            _avg=probgam(.25*_width*_width,1.5);
            _norm=sqrt(_avg/_p);
            &&nor&i=&&sca&i/_norm;
            * put _p= _norm= &&sca&i= &&nor&i=;
         %end;
       run;
      %if &syserr>4 %then %goto fail;
   %end;
   %goto exit;
%fail:
   %let _xrc_=%qcmpres(PROC SUMMARY, PROC ACECLUS,
                       or DATA step failed);
%exit:
%mend agk;

%macro spacing(p);
   %let _xrc_=STD_OK;
   proc summary nway data=&data;
      output out=_N
         n=
            %do i=1 %to &nvar;
               _n&i
            %end;
         ;
      &fqstmt
      var &var;
      by &by;
   run;
   %if &syserr>4 %then %goto fail;
   %do i=1 %to &nvar;
      data _TEMP; * select relevant variables;
         set &data;
         keep &&var&i &freq
         %if %bquote(&by)^= %then %do;
            &_byvars
         %end;
         ;
         %if &freq^= %then %do;
            if nmiss(&freq) then delete;
            if &freq<1 then delete;
         %end;
      run;
      %if &syserr>4 %then %goto fail;
      %if &freq= %then %do;
         data _null_;
            retain _max .;
            set _N(firstobs=1 obs=2000000000) end=_end;
            _max=max(_max,_n&i);
            if _end then do;
               _max=ceil(_max*&p)-1;
               call symput('maxlag',trim(left(put(_max,12.))));
            end;
         run;
         %xbug(,maxlag)
         %if &syserr>4 %then %goto fail;
      %end;
      %else %do;
         proc sort data=_TEMP(firstobs=1 obs=2000000000) out=_FREQ;
            by &by &freq;
         run;
         data _null_;
            retain _max _n _f _i 0;
            %xmerge(_FREQ(firstobs=1 obs=2000000000),
                    _N(firstobs=1 obs=2000000000));
            %if %bquote(&by)= %then %do;
               if _n_=1 then do;
            %end;
            %else %do;
               if first.&_bylast then do;
            %end;
                  _n=ceil(_n&i*&p);
                  _f=0;
                  _i=0;
               end;
            if _f<_n then do;
               _f+floor(&freq);
               _i+1;
            end;
            %if %bquote(&by)= %then %do;
               if _end1 then do;
            %end;
            %else %do;
               if last.&_bylast then do;
            %end;
                  _max=max(_max,_i);
               end;
               if _end1 then do;
                  call symput('maxlag',trim(left(put(_max,12.))));
               end;
         run;
         %xbug(,maxlag)
         %if &syserr>4 %then %goto fail;
      %end;
      proc sort data=_TEMP(firstobs=1 obs=2000000000);
         by &by &&var&i;
      run;
      %if &syserr>4 %then %goto fail;
      data %if &i=1 %then &outstat; %else _STAT2; ;
         keep &&loc&i &&sca&i;
         %if &norm %then %do;
            %let norm=2;
            %if &snorm %then %let snorm=2;
            keep &&nor&i;
         %end;
         retain _min _mode _m _res;
         %if &freq= %then %xlagi(&maxlag);
                    %else %xlagfqi(&maxlag);
         %xmerge(_TEMP(firstobs=1 obs=2000000000),
                 _N(firstobs=1 obs=2000000000))
         %if %bquote(&by)= %then %do;
            if _n_=1 then do;
         %end;
         %else %do;
            if first.&_bylast then do;
         %end;
               _min=.;
               _m=ceil(_n&i*&p);
               _p=_m/_n&i;
               _m=_m-1;
               %if &_putall_ %then %do;
                  put _n&i= _m= _p= _res=;
               %end;
               %if &norm %then %do;
                  _res=2*probit(.5+.5*_p)
                  %if &snorm %then %do;
                       -2.6*_n&i**-.55*_p**.25
                  %end;
                  ;
                  %if &_putall_ %then %do;
                     put _res=;
                  %end;
               %end;
               %if &freq= %then %xlagr(&maxlag);
                          %else %xlagfqr(&maxlag);
           end;
         if nmiss(&&var&i)=0 then do;
            %if &freq= %then %xlag(_l,&&var&i,_m,&maxlag);
                       %else %xlagfq(_l,&&var&i,&freq,_m,&maxlag);
            if _l^=. then do;
               _d=&&var&i-_l;
               if _min=. | _d<_min then do;
                  _min=_d;
                  _mode=_l+_d/2;
               end;
            end;
         end;
         %if %bquote(&by)= %then %do;
            if _end1 then do;
         %end;
         %else %do;
            if last.&_bylast then do;
         %end;
               &&loc&i=_mode;
               &&sca&i=_min;
               %if &norm %then %do;
                  &&nor&i=_min/_res;
               %end;
               output;
            end;
         %if &_putall_ %then %do;
            put _all_;
         %end;
      run;
      %if &syserr>4 %then %goto fail;
      %if &i>1 %then %do;
         data &outstat;
            merge &outstat(firstobs=1 obs=2000000000)
                  _STAT2(firstobs=1 obs=2000000000);
            %if %bquote(&by)= %then %do;
               by &_bydata;
            %end;
         run;
         %if &syserr>4 %then %goto fail;
      %end;
   %end;
   %goto exit;
%fail:
   %let _xrc_=%qcmpres(PROC SUMMARY, PROC SORT, or DATA step failed);
%exit:
%mend spacing;

%macro aest(c);
   %let _xrc_=%qcmpres(No macro found to implement INITIAL=&initial);
   %let initial=%unquote(&initial);
   %&initial;
   %if &_xrc_^=STD_OK %then %do;
      %put ERROR: &_xrc_..;
      %goto exit;
   %end;
   %if %bquote(&freq)^= %then %let fq=&freq;
                        %else %let fq=1;
   data &outstat;
      %xmerge(&data,&outstat(firstobs=1 obs=2000000000))
      keep
      %if %bquote(&by)= %then %do;
         &_byvars
      %end;
      %do i=1 %to &nvar; &&loc&i &&sca&i %end;
      ; %* end of keep stmt;
      array _loc %do i=1 %to &nvar; &&loc&i %end; ;
      array _sca %do i=1 %to &nvar; &&sca&i %end; ;
      array _var %do i=1 %to &nvar; &&var&i %end; ;
      array _n _n1-_n&nvar;
      array _ln _ln1-_ln&nvar;
      array _sn _sn1-_sn&nvar;
      array _den _den1-_den&nvar;
      retain _n1-_n&nvar _ln1-_ln&nvar _sn1-_sn&nvar _den1-_den&nvar;
      %if %bquote(&by)= %then %do;
         if _n_=1 then do;
      %end;
      %else %do;
         if first.&_bylast then do;
      %end;
            do _i=1 to &nvar;
               _n[_i]=0;
               _ln[_i]=0;
               _sn[_i]=0;
               _den[_i]=0;
            end;
         end;
      do _i=1 to &nvar;
         if nmiss(_var[_i])=0 then do;
            _n[_i]+&fq;
            if _sca[_i]>0 then do;
               _u=(_var[_i]-_loc[_i])/(&c*_sca[_i]);
               _use=1;
               %psi(_psi,_u)
               if _use then do;
                  _ln[_i]+&fq*_psi;
                  _sn[_i]+&fq*_psi**2;
                  %dpsi(_dpsi,_u)
                  _den[_i]+&fq*_dpsi;
               end;
            end;
         end;
      end;
      %if &_putall_ %then %do;
         put _all_;
      %end;
      %if %bquote(&by)= %then %do;
         if _end1 then do;
      %end;
      %else %do;
         if last.&_bylast then do;
      %end;
            do _i=1 to &nvar;
               if _den[_i]^=0 then do;
                  _loc[_i]+&c*_sca[_i]*_ln[_i]/_den[_i];
                  %if &vardef=DF %then %do;
                     _sca[_i]=&c*_sca[_i]*_n[_i]*
                              sqrt(_sn[_i]/(_n[_i]-1))/
                              abs(_den[_i]);
                  %end;
                  %else %do;
                     _sca[_i]=&c*_sca[_i]*sqrt(_n[_i]*_sn[_i])/
                              abs(_den[_i]);
                  %end;
               end;
            end;
            output;
         end;
   run;
   %if &syserr>4 %then %goto fail;
   %goto exit;
%fail:
   %let _xrc_=%qcmpres(A-estimation DATA step failed);
%exit:
%mend aest;

%macro abw(c);
   %macro psi(p,u);
      if abs(&u)>=1 then _use=0;
      else do;
         _u2=&u**2;
         &p=&u*(1-_u2)**2;
      end;
   %mend psi;
   %macro dpsi(d,u);
      &d=(1-_u2)*(1-5*_u2);
   %mend dpsi;
   %aest(&c);
%mend abw;

%macro awave(c);
   %let pi=3.14159265358979;
   %macro psi(p,u);
      if abs(&u)>=1 then _use=0;
      else do;
         &p=sin(&pi*&u)/&pi;
      end;
   %mend psi;
   %macro dpsi(d,u);
      &d=cos(&pi*&u);
   %mend dpsi;
   %aest(&c);
%mend awave;

%macro ahuber(c);
   %macro psi(p,u);
      if &u>1 then &p=1;
      else if &u<-1 then &p=-1;
      else &p=&u;
   %mend psi;
   %macro dpsi(d,u);
      if abs(&u)>1 then &d=0;
      else &d=1;
   %mend dpsi;
   %aest(&c);
%mend ahuber;

%macro std_l(p);
   %let _xrc_=STD_OK;
   %if &wtopt %then %let wtopt=2;
   %if &norm %then %stdmsg(NORM,L);
   %if &p=2 %then %std;
   %else %do i=1 %to &nvar;
      proc fastclus data=&data least=&p maxc=1 noprint vardef=&vardef
                    outseed= %if &i=1 %then &outstat; %else _STAT2;
                    (keep=&&var&i _scale_
                     %if %bquote(&by)= %then %do;
                        &_byvars
                     %end;
                     rename=(&&var&i=&&loc&i _scale_=&&sca&i));
         var &&var&i;
         &fqstmt
         &wtstmt
         by &by;
      run;
      %if &syserr>0 %then %goto fail;
      %if &i>1 %then %do;
         data &outstat;
            merge &outstat(firstobs=1 obs=2000000000)
                  _STAT2(firstobs=1 obs=2000000000);
            %if %bquote(&by)= %then %do;
               by &_bydata;
            %end;
         run;
         %if &syserr>4 %then %goto fail;
      %end;
   %end;
   %goto exit;
%fail:
   %let _xrc_=%qcmpres(PROC FASTCLUS or DATA step failed);
%exit:
%mend std_l;

%macro in(ds);
   %let _xrc_=STD_OK;
   %if %qupcase(&ds)=_LAST_ %then %let instat=&syslast;
   %else %let instat=&ds;
   %local remove;
   %let remove=;
   %if %bquote(&by)^= %then %let remove=&remove by;
   %if %bquote(&location)^= %then %do;
      %xvlist(data=&ds,_list=location,_name=loc,_count=nloc,
           remove=&remove,valid=1);
      %if &nloc^=&nvar %then %do;
         %let _xrc_=%qcmpres(The number of LOCATION= variables, &nloc,
            does not equal the number of VAR= variables, &nvar);
      %end;
   %end;
   %if %bquote(&scale)^= %then %do;
      %xvlist(data=&ds,_list=scale,_name=sca,_count=nsca,
           remove=&remove,valid=1);
      %if &nsca^=&nvar %then %do;
         %let _xrc_=%qcmpres(The number of SCALE= variables, &nsca,
            does not equal the number of VAR= variables, &nvar);
      %end;
   %end;
%mend in;

%macro stdmsg(option,method);
   %put WARNING: The &option option does not apply to METHOD=&method..;
   %let &option=0;
%mend;

%global _stdize_;
%let _stdize_=610;

%* close comment possibly generated by xmacro */;

*===================== END STDIZE MACRO ============================;
